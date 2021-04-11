// Full sources required to compile srep.cpp are available at http://freearc.org/Download-Alpha.aspx
// All rights reserved. Mail me if you have any questions or want to buy a commercial license for the source code.
char *program_version     = "SREP 3.93 beta", *program_date = "August 3, 2013";
char *program_description = "huge-dictionary LZ77 preprocessor   (c) Bulat.Ziganshin@gmail.com";
char *program_homepage    = "http://freearc.org/research/SREP.aspx";

#include <algorithm>
#include <set>
#include <stack>
#include <vector>
#include <malloc.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>

#include "../Common.h"
#include "../Compression.h"
#include "../MultiThreading.h"
#include "../MultiThreading.cpp"

// Constants defining compressed file format
const uint SREP_SIGNATURE = 0x50455253;
const uint SREP_FORMAT_VERSION1 = 1;
const uint SREP_FORMAT_VERSION2 = 2;
const uint SREP_FORMAT_VERSION3 = 3;
const uint SREP_FORMAT_VERSION4 = 4;
const uint SREP_FOOTER_VERSION1 = 1;
enum SREP_METHOD {SREP_METHOD0=0, SREP_METHOD1=1, SREP_METHOD2, SREP_METHOD3, SREP_METHOD4, SREP_METHOD5,
                  SREP_METHOD_FIRST=SREP_METHOD0, SREP_METHOD_LAST=SREP_METHOD5};
typedef uint32 STAT;
const int STAT_BITS=32, ARCHIVE_HEADER_SIZE=4, BLOCK_HEADER_SIZE=3, MAX_HEADER_SIZE=4, MAX_HASH_SIZE=256;
enum COMMAND_MODE {COMPRESSION, DECOMPRESSION, INFORMATION};
const char* SREP_EXT = ".srep";

// Compression algorithms constants and defaults
const int MINIMAL_MIN_MATCH = 16;       // minimum match length that sometimes allows to reduce file using the match
const int DEFAULT_MIN_MATCH = 32;       // minimum match length that usually produces smallest compressed file (don't taking into account further compression)

// Program exit codes
enum { NO_ERRORS         = 0
     , WARNINGS          = 1
     , ERROR_CMDLINE     = 2
     , ERROR_IO          = 3
     , ERROR_COMPRESSION = 4
     , ERROR_MEMORY      = 5
     };

typedef uint64 Offset;               // Filesize or position inside file


// Performance counters printed by -pc option - useful for further program optimization
static struct {Offset max_offset, find_match, find_match_memaccess, check_hasharr, hash_found, check_len, record_match, total_match_len;} pc;


#if defined(_M_X64) || defined(_M_AMD64) || defined(__x86_64__)
#define _32_or_64(_32,_64) (_64)
#define _32_only(_32)      (void(0))
typedef size_t NUMBER;               // best choice for loop index variables on most 64-bit compilers
#else
#define _32_or_64(_32,_64) (_32)
#define _32_only(_32)      (_32)
typedef int NUMBER;                  // best choice for loop index variables on most 32-bit compilers
#endif

#ifdef MY_MEMCPY
// GCC4.7.2/x64 has rather slow memcpy :(
void *my_memcpy (void *__restrict b, const void *__restrict a, size_t n){
  char *__restrict s1 = (char *)b;
  const char *__restrict s2 = (const char *)a;
  for(; 0<n; --n)*s1++ = *s2++;
  return b;
}
#else
#define my_memcpy memcpy
#endif

#ifdef MY_MEMSET
void *my_memset (void *a, int v, size_t n){
  char *s1 = (char *)a;
  for(; 0<n; --n) *s1++ = v;
  return a;
}
#else
#define my_memset memset
#endif


// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Win32 СryptoAPI: SHA-1 & MD5 *************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define MD5_SIZE    16
#define SHA1_SIZE   20
#define SHA512_SIZE 64


#if 0   // #ifdef FREEARC_WIN : unfortunately, it doesnt' show consistent speed improvement
        // Also, CryptoAPI functions should be dynaloaded from the Advapi32.dll because they are available only starting from WinXP

#include <Wincrypt.h>
struct Win32CryptoAPI
{
  HCRYPTPROV hCryptProv;

  // Acquire/release handle to a cryptography provider context.
  Win32CryptoAPI()   {CryptAcquireContext (&hCryptProv, NULL, NULL, PROV_RSA_FULL, 0);}
  ~Win32CryptoAPI()  {CryptReleaseContext (hCryptProv, 0);}

  bool hash (ALG_ID alg, DWORD count, void *buf, int size, void *result)
  {
    HCRYPTHASH hHash;
    if(!CryptCreateHash (hCryptProv, alg, 0, 0, &hHash))                   return false;
    if(!CryptHashData (hHash, (BYTE*)buf, size, 0))                        return false;
    if(!CryptGetHashParam (hHash, HP_HASHVAL, (BYTE*)result, &count, 0))   return false;
    CryptDestroyHash (hHash);
    return true;
  }

  bool sha1 (void *buf, int size, void *result)  {return hash (CALG_SHA1, SHA1_SIZE, buf, size, result);}
  bool md5  (void *buf, int size, void *result)  {return hash (CALG_MD5,  MD5_SIZE,  buf, size, result);}

} CryptoAPI;

#else

static struct
{
  bool sha1 (void *buf, int size, void *result)  {return false;}
  bool md5  (void *buf, int size, void *result)  {return false;}
} CryptoAPI;

#endif


// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// LibTomCrypt: SHA-1, MD5 & Fortuna CPRNG  *********************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define LTC_NO_HASHES
#define   LTC_MD5
#define   LTC_SHA1
#define   LTC_SHA512
#define LTC_NO_CIPHERS
#define   LTC_RIJNDAEL
#define     ENCRYPT_ONLY

// crypt_argchk.c and aes.c are included via vmac.h below
//#include "crypt/crypt_argchk.c"
//#include "ciphers/aes/aes.c"
#include "hashes/md5.c"
#include "hashes/sha1.c"
#include "hashes/sha2/sha512.c"
#include "prngs/fortuna.c"
#include "misc/zeromem.c"

typedef unsigned char Digest[SHA1_SIZE];
#define compute_digest compute_sha1

void compute_sha1 (void*, void *buf, int size, void *result)
{
  if (CryptoAPI.sha1 (buf, size, result))  return;
  hash_state state;
  sha1_init    (&state);
  sha1_process (&state, (unsigned char*)buf, (unsigned long)size);
  sha1_done    (&state, (unsigned char*)result);
}


void compute_md5 (void*, void *buf, int size, void *result)
{
  if (CryptoAPI.md5 (buf, size, result))  return;
  hash_state state;
  md5_init    (&state);
  md5_process (&state, (unsigned char*)buf, (unsigned long)size);
  md5_done    (&state, (unsigned char*)result);
}

void compute_sha512 (void*, void *buf, int size, void *result)
{
  hash_state state;
  sha512_init    (&state);
  sha512_process (&state, (unsigned char*)buf, (unsigned long)size);
  sha512_done    (&state, (unsigned char*)result);
}


void cryptographic_prng (void *result, size_t size)
{
  static prng_state prng[1];
  static bool initialized = false;

  if (!initialized)
  {
    fortuna_start(prng);
    const int size=4096;
    unsigned char buf[size];
    int bytes = systemRandomData (buf,size);
    fortuna_add_entropy (buf, bytes, prng);
    time((time_t*)buf);
    fortuna_add_entropy (buf, sizeof(time_t), prng);
    fortuna_ready (prng);
    initialized = true;
  }

  fortuna_read ((unsigned char *)result, size, prng);
}


// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Hash functions ***********************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// hash содержит значение хеш-функции от последних L обработанных байт, для удобства обновления используется скользящая хеш-функция.
// constructor(buf,L,seed) создаёт хеш, параметризованный seed, и инициализирует его первыми L байтами буфера
// update(sub,add) выносит из хеша байт sub и добавляет байт add.

template <class ValueT>
struct FakeRollingHash
{
  operator ValueT ()                               {return 0;}
  FakeRollingHash (void *buf, int L, ValueT seed)  {}
  void update     (BYTE sub, BYTE add)             {}
};


// Возведение в степень
template <class T>
T power (T base, unsigned n)
{
  T result = 1;
  while (n) {
    if (n % 2)  result*=base, n--;
    n /= 2;  base*=base;
  }
  return result;
}

template <class ValueT>
struct PolynomialHash
{
  operator ValueT()  {return value;}
  ValueT value, PRIME;

  PolynomialHash (ValueT seed) : value(0), PRIME(seed) {}

  PolynomialHash (void *buf, int L, ValueT seed) : value(0), PRIME(seed)
  {
    update (buf, L);
  }

  void update (BYTE b)
  {
    value = value*PRIME + b;
  }

  void update (void *buf, int L)
  {
    for (NUMBER i=0; i<L; i++)  update (((BYTE*)buf)[i]);
  }
};

template <class ValueT>
struct PolynomialRollingHash
{
  operator ValueT()  {return value;}
  ValueT value, PRIME, PRIME2, PRIME3, PRIME4, PRIME5, PRIME6, PRIME7, PRIME8, PRIME_L, PRIME_L1, PRIME_L2, PRIME_L3;
  int L;

  PolynomialRollingHash (int _L, ValueT seed)
  {
    L = _L;
    PRIME8 = seed * (PRIME7 = seed * (PRIME6 = seed * (PRIME5 = seed * (PRIME4 = seed * (PRIME3 = seed * (PRIME2 = seed * (PRIME = seed)))))));
    PRIME_L3 = seed * (PRIME_L2 = seed * (PRIME_L1 = seed * (PRIME_L = power(PRIME,L))));
  }

  PolynomialRollingHash (void *buf, int _L, ValueT seed)
  {
    L = _L;
    PRIME8 = seed * (PRIME7 = seed * (PRIME6 = seed * (PRIME5 = seed * (PRIME4 = seed * (PRIME3 = seed * (PRIME2 = seed * (PRIME = seed)))))));
    PRIME_L3 = seed * (PRIME_L2 = seed * (PRIME_L1 = seed * (PRIME_L = power(PRIME,L))));
    moveto (buf);
  }

  void update (BYTE sub, BYTE add)
  {
    value = value*PRIME + add - PRIME_L*sub;
  }

  // Roll hash by N==power(2,x) bytes
  template <int N>
  void update (void *_ptr)
  {
    BYTE *ptr = (BYTE*) _ptr;
    switch(N%4)
    {
    case 0:   break;

    case 1:   value = value*PRIME + ptr[L] - PRIME_L*ptr[0]; break;

    case 2:   value = value*PRIME2 + PRIME*ptr[L] + ptr[L+1]
                                   - PRIME_L1*ptr[0] - PRIME_L*ptr[1]; break;

    case 3:   value = value*PRIME3 + PRIME2*ptr[L] + PRIME*ptr[L+1] + ptr[L+2]
                                   - PRIME_L2*ptr[0] - PRIME_L1*ptr[1] - PRIME_L*ptr[2]; break;
    }

    for (int i=0; i<N/4; i++, ptr+=4)
      value = value*PRIME4 + PRIME3*ptr[N%4+L] + PRIME2*ptr[N%4+L+1] + PRIME*ptr[N%4+L+2] + ptr[N%4+L+3]
                           - PRIME_L3*ptr[N%4+0] - PRIME_L2*ptr[N%4+1] - PRIME_L1*ptr[N%4+2] - PRIME_L*ptr[N%4+3];
  }

  void moveto (void *_buf);
};

template <class ValueT>
void PolynomialRollingHash<ValueT>::moveto (void *_buf)
{
  value = 0;  BYTE *buf = (BYTE*) _buf;
  const int N=16, S=4;
  for (int i=0; i < (L&~(N-1)); i+=N)
  {
#   define STEP(n)   (value = value*PRIME4 + PRIME3*buf[i+(n)*S] + PRIME2*buf[i+1+(n)*S] + PRIME*buf[i+2+(n)*S] + buf[i+3+(n)*S])
    STEP(0); if (N>S) STEP(1); if (N>2*S) {STEP(2); STEP(3);} if (N>4*S) {STEP(4); STEP(5); STEP(6); STEP(7);}
  }
  for (int i = L&~(N-1); i<L; i++)
    value = value*PRIME + buf[i];
}

// Large 32-bit primes suitable for seeding the polynomial hash
const uint32  PRIME1 = 153191,  PRIME2 = 3141601;


// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// CRC hashing **************************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if GCC_VERSION >= 403

// Requires GCC4.3 and SSE4.2-enabled CPU; and of course compatible only with Crc32CastagnoliPolynom
#include <x86intrin.h>
#include <cpuid.h>
uint32 a_mm_crc32_u8(uint32 crc, uint8 value) {
  asm("crc32b %[value], %[crc]\n" : [crc] "+r" (crc) : [value] "rm" (value));
  return crc;
}
#define update_CRC(crc,CRCTable,c)  (a_mm_crc32_u8((crc),(c)))

bool crc32c()  /* Check CPU for CRC32c asm instruction support (part of SSE4.2) */
{
  uint32 eax, ebx, ecx, edx;
  __get_cpuid(1, &eax, &ebx, &ecx, &edx);
  return (ecx & bit_SSE4_2) != 0;
}

#else

#define update_CRC(crc,CRCTable,c)  (CRCTable[((crc)^(c)) & 0xFF] ^ ((crc)>>8))
#define crc32c()                    false    /* CRC32c asm instruction isn't supported */

#endif

template <class ValueT>
struct CrcRollingHash
{
  operator ValueT()  {return value;}
  ValueT value, CRCTab[256], RollingCRCTab[256];
  NUMBER L;

  CrcRollingHash (           int _L, ValueT seed)   {init (_L, seed);}
  CrcRollingHash (void *buf, int _L, ValueT seed)   {init (_L, seed);  moveto (buf);}

  void init (int L, ValueT seed);

  // Calculate initial hash value
  void moveto (void *buf)
  {
    value = 0;
    for (NUMBER i=0; i<L; i++)  update (0, ((BYTE*)buf)[i]);
  }

  void update (BYTE sub, BYTE add)
  {
    value = update_CRC(value,CRCTab,add) ^ RollingCRCTab[sub];
  }
};

// Fast CRC table construction algorithm
template <class ValueT>
void FastTableBuild (ValueT CRCTable[256], ValueT seed, ValueT CrcPolynom)
{
  ValueT crc    = seed;
  CRCTable[0]   = 0;
  CRCTable[128] = crc;
  for (NUMBER i=64; i; i/=2)
    CRCTable[i] = crc = (crc >> 1) ^ (CrcPolynom & ~((crc & 1) - 1));

  for (NUMBER i=2; i<256; i*=2)
    for (NUMBER j=1; j<i; j++)
      CRCTable[i+j] = CRCTable[i] ^ CRCTable[j];
}

// Calculate CRC of buffer
template <class ValueT>
ValueT calcCRC (BYTE *buffer, int len, ValueT CRCTable[256])
{
  ValueT crc = 0;
  for (NUMBER i=0; i<len; i++)
    crc = update_CRC (crc, CRCTable, buffer[i]);
  return crc;
}

template <class ValueT>
void CrcRollingHash<ValueT>::init (int _L, ValueT CrcPolynom)
{
  L = _L;

  // Fast CRC table construction
  FastTableBuild (CRCTab, CrcPolynom, CrcPolynom);

  // Fast table construction for rolling CRC
  ValueT crc = update_CRC(0,CRCTab,128);
  for (NUMBER i=0; i<L; i++)
    crc = update_CRC (crc, CRCTab, 0);
  FastTableBuild (RollingCRCTab, crc, CrcPolynom);
}

// Some popular CRC polynomes
const uint32 Crc32IeeePolynom = 0xEDB88320;
const uint32 Crc32CastagnoliPolynom = 0x82F63B78;
const uint64 Crc64EcmaPolynom = 0xC96C5795D7870F42ULL;


// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SIPHASH: keyed cryptographic hash ****************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "siphash/siphash.c"
#define SIPHASH_TAG_LEN_BYTES 8
#define SIPHASH_KEY_LEN_BYTES 16

void* new_siphash (void *seed, int size)
{
  if (size!=SIPHASH_KEY_LEN_BYTES)
    return NULL;
  void *key = malloc(SIPHASH_KEY_LEN_BYTES);
  memcpy (key, seed, SIPHASH_KEY_LEN_BYTES);
  return key;
}

void compute_siphash (void *key, void *buf, int size, void *result)
{
  *(uint64_t*)result = siphash ((unsigned char *)key, (const unsigned char *)buf, size);
}


// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// VHASH: keyed cryptographic hash ******************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define VMAC_TAG_LEN     128  /* Requesting VMAC-128 algorithm (instead of VMAC-64) */
#define VMAC_KEY_LEN     256  /* Must be 128, 192 or 256 (AES key size)        */
#define VMAC_NHBYTES     4096 /* Must 2^i for any 3 < i < 13. Standard = 128   */
#define VMAC_USE_LIB_TOM_CRYPT 1
#include "vmac/vmac.c"
#define VMAC_ALIGNMENT   16   /* SSE-compatible memory alignment */
#define VMAC_TAG_LEN_BYTES (VMAC_TAG_LEN/CHAR_BIT)
#define VMAC_KEY_LEN_BYTES (VMAC_KEY_LEN/CHAR_BIT)

struct VHash
{
  bool initialized;
  ALIGN(VMAC_ALIGNMENT) vmac_ctx_t ctx;

  VHash() : initialized(false) {}

  // Initialize ctx
  void init (void *seed = NULL)
  {
    if (!initialized || seed)
    {
      ALIGN(4) unsigned char key[VMAC_KEY_LEN_BYTES];
      if (seed)  memcpy (key, seed, VMAC_KEY_LEN_BYTES);
      else       cryptographic_prng (key, VMAC_KEY_LEN_BYTES);
      vmac_set_key(key, &ctx);
      initialized = true;
    }
  }

  // Return hash value for the buffer
  void compute (const void *ptr, size_t size, void *result)
  {
    uint64_t res, tagl;
    init();

    res = vhash((unsigned char*)ptr, size, &tagl, &ctx);

    ((uint64_t*)result)[0] = res;
    if (VMAC_TAG_LEN==128)
      ((uint64_t*)result)[1] = tagl;
  }
};

void* new_vhash (void *seed, int size)
{
  if (size!=VMAC_KEY_LEN_BYTES)
    return NULL;
  VHash *h = new VHash;
  h->init(seed);
  return h;
}

void compute_vhash (void *hash, void *buf, int size, void *result)
{
  ((VHash *)hash)->compute(buf, size, result);
}


// Using VHash instead of SHA-1 for digest
struct VDigest
{
  VHash vhash1, vhash2;
  void init()  {vhash1.init(); vhash2.init();}
  void compute (const void *ptr, size_t size, void *result)
  {
    vhash1.compute (ptr, size, result);
    vhash2.compute (ptr, size, (BYTE*)result + sizeof(Digest) - VMAC_TAG_LEN_BYTES);
  }
};



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Hash descriptors *********************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Function returning hash object initialized by the provided seed
typedef void* (*new_hash_t) (void *seed, int size);

// Hash function processing the (buf,size) and storing computed hash value to the result
typedef void (*hash_func_t) (void *hash, void *buf, int size, void *result);

// Description for various hash algorithms
struct hash_descriptor {
  char*        hash_name;           // name used in the -hash=... option
  unsigned     hash_num;            // numeric tag stored in the archive header
  unsigned     hash_seed_size;      // additional bytes stored in the archive header (seed value for randomized hashes)
  unsigned     hash_size;           // bytes stored in the each block (hash value)
  new_hash_t   new_hash;            // create hash object
  hash_func_t  hash_func;           // hash function
} hash_descriptors[] = {{"md5",     0,  0,                      MD5_SIZE,               0,            compute_md5},
                        {"",        1,  0,                      MD5_SIZE,               0,            0},
                        {"sha1",    2,  0,                      SHA1_SIZE,              0,            compute_sha1},
                        {"sha512",  3,  0,                      SHA512_SIZE,            0,            compute_sha512},
                        {"vmac",    4,  VMAC_KEY_LEN_BYTES,     VMAC_TAG_LEN_BYTES,     new_vhash,    compute_vhash},
                        {"siphash", 5,  SIPHASH_KEY_LEN_BYTES,  SIPHASH_TAG_LEN_BYTES,  new_siphash,  compute_siphash},
                       };

const char *DEFAULT_HASH = "vmac",  *HASH_LIST = "vmac(default)/siphash/md5/sha1/sha512";


// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Hash table ***************************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef size_t HashValue;            // Hash of L-byte block, used as first step to find the match
typedef uint32 StoredHashValue;      // Hash value stored in hasharr[]
typedef uint64 BigHash;              // We need 64 bits for storing index+value in the chunkarr+hasharr
typedef uint32 Chunk;                // Uncompressed file are splitted into L-byte chunks, it's the number of chunk in the file
const Chunk  MAX_CHUNK = Chunk(-1),  NOT_FOUND = 0;
const int MAX_HASH_CHAIN = 12;

#define min_hash_size(n)   (((n)/4+1)*5)     /* Minimum size of hash for storing n elements */


// Improves EXHAUSTIVE_SEARCH by filtering out ~90% of false matches
struct SliceHash
{
  typedef uint32 entry;
  static const NUMBER BITS=4, ONES=(1<<BITS)-1;   // how much bits used from every calculated hash

  entry *h;  // Array holding all slice hashes, one `entry` per L bytes
  Offset memreq;
  int check_slices, errcode;
  NUMBER L, slices_in_block, slice_size;

  SliceHash (Offset filesize, unsigned _L, unsigned MIN_MATCH, int io_accelerator)
  {
    h = NULL;  errcode = ERROR_MEMORY;  L = _L;
    slices_in_block = sizeof(entry)*CHAR_BIT/BITS;
    slice_size      = L/slices_in_block;                               // to do: 8/16-byte entry (считывать в check по одному байту из h)
    check_slices    = int((MIN_MATCH-L)/slice_size) - io_accelerator;  // if less that this amount of slices around match has the same hashes, then we are sure that match can't be extended to MIN_MATCH size
    if (io_accelerator<0 || check_slices<=0)
         memreq = 0;                                                   // no need in SliceHash since each potential match is almost guaranteed to have MIN_MATCH matched bytes
    else memreq = filesize/L * sizeof(entry);                          // one `entry` per L input bytes
  }

  // Actual memory allocation (should be performed after allocation of more frequently accessed arrays of the HashTable)
  void alloc (LPType LargePageMode)
  {
    if (memreq == 0)   {errcode = NO_ERRORS; return;}
    h        =  (entry*) BigAlloc (memreq, LargePageMode);
    errcode  =  (h==NULL? ERROR_MEMORY : NO_ERRORS);
  }
  ~SliceHash()      {BigFree(h);}

  // Hash of provided buffer
  entry hash (void *ptr, int size)
  {
    uint32 hash = 111222341u;
    for (BYTE *p = (BYTE*)ptr;  (BYTE*)p < (BYTE*)ptr+size;  p++)
      hash  =  (hash*123456791u) + *p;
    return (hash*123456791u) >> (sizeof(uint32)*CHAR_BIT-BITS);
  }

  // Fill h[] with hashes of slices of each chunk in the buf
  void prepare_buffer (Offset offset, char *buf, int size)
  {
    if (h==NULL) return;
    Chunk curchunk = offset/L;
    for (char *p = buf;  p < buf+size;  )
    {
      entry checksum = 0;
      for (int i=0;  i<slices_in_block;  i++, p+=slice_size)
        checksum  +=  hash(p,slice_size) << (i*BITS);
      h[curchunk++] = checksum;
    }
  }

  // Return TRUE if match MAY BE large enough, FALSE - if that's absolutely impossible
  bool check (Chunk chunk, void *p, int i, int block_size)
  {
    if (h==NULL) return true;
    if (i<L || block_size-i<2*L)   // not enough bytes around chunk in the buffer to check them against saved hashes
      return true;
    entry checksum = h[chunk+1];  NUMBER j,k;
    for (j=0; ; j++)
    {
      if (j==check_slices)
        return true;
      if (((checksum>>(j*BITS)) & ONES)  !=  hash ((char*)p+L+j*slice_size, slice_size))
        break;
    }
    checksum = h[chunk-1];
    for (k=0; ; k++)
    {
      if (k+j==check_slices)
        return true;
      if (((checksum>>((slices_in_block-(k+1))*BITS)) & ONES)  !=  hash ((char*)p-(k+1)*slice_size, slice_size))
        break;
    }
    return false;
  }
};


// Match search engine
struct HashTable
{
  bool ROUND_MATCHES;
  bool COMPARE_DIGESTS;
  bool PRECOMPUTE_DIGESTS;
  bool CONTENT_DEFINED_CHUNKING;
  int _errcode;
  size_t L;
  MMAP_FILE &mmap_infile;
  Offset filesize;
  Offset total_chunks;
  Chunk  curchunk, chunknum_mask, hash_mask;
  Offset hs;
  size_t hashsize, hashsize1, hash_shift;
  Chunk           *chunkarr;
  StoredHashValue *hasharr;
  Offset          *startarr;
  Digest          *digestarr;
  SliceHash        slicehash;
  VDigest          MainDigest, PrepDigest;

  // bitarr[] used for fast probing of hash values - it helps to detect whether we ever seen such hash value before
  size_t  bitarrsize;
  size_t  bitshift;
  BYTE   *bitarr;

  HashTable (bool _ROUND_MATCHES, bool _COMPARE_DIGESTS, bool _PRECOMPUTE_DIGESTS, bool INMEM_COMPRESSION, bool _CONTENT_DEFINED_CHUNKING, unsigned _L, unsigned MIN_MATCH, int io_accelerator, unsigned BITARR_ACCELERATOR, MMAP_FILE &_mmap_infile, Offset _filesize, LPType LargePageMode)
    : mmap_infile(_mmap_infile), slicehash(_filesize,_L,MIN_MATCH,io_accelerator)
  {
    _errcode = ERROR_MEMORY;
    ROUND_MATCHES = _ROUND_MATCHES;  COMPARE_DIGESTS = _COMPARE_DIGESTS;  PRECOMPUTE_DIGESTS = _PRECOMPUTE_DIGESTS;  CONTENT_DEFINED_CHUNKING = _CONTENT_DEFINED_CHUNKING;
    L = _L;  filesize = mymax(_filesize,L);  curchunk = 0;  bitarr = NULL;  chunkarr = NULL;  hasharr = NULL;  startarr = NULL;  digestarr = NULL;
    if (INMEM_COMPRESSION)  {_errcode = NO_ERRORS; hs=total_chunks=bitarrsize=0; slicehash.alloc(LargePageMode); return;}
    MainDigest.init();  PrepDigest = MainDigest;  // we need two equal digests since they are used in 2 threads and have internal state modified due hashing
    total_chunks  =  filesize/L;  if (CONTENT_DEFINED_CHUNKING)  total_chunks += total_chunks/(total_chunks>1024?10:1);  // In the CONTENT_DEFINED_CHUNKING mode, chunks may have any size, so we alloc 10% extra space
    chunknum_mask = roundup_to_power_of(total_chunks+2,2)-1;  hash_mask = ~chunknum_mask;   // Masks for cnunk number and hash bits in the chunkarr[] item
    hs = roundup_to_power_of (min_hash_size(total_chunks), 2);
    if (hs > size_t(-1)  ||  total_chunks > MAX_CHUNK-2)  return;
    hashsize = hs,  hashsize1 = hashsize-1,  hash_shift = sizeof(HashValue)*CHAR_BIT - lb(hashsize);
    bitarrsize = (BITARR_ACCELERATOR==0 || CONTENT_DEFINED_CHUNKING)
                   ? 0 : roundup_to_power_of (mymax(total_chunks/CHAR_BIT * BITARR_ACCELERATOR, 2), 2);   // bit array checking works fine until 1/8 of bitarr[] gets filled
    bitshift = sizeof(HashValue)*CHAR_BIT - lb(bitarrsize);    // bitarrsize should be >=2, otherwise hash>>bitshift == hash>>64 == hash>>0 and indexing panics

    // Allocate arrays starting with the most frequently accessed (to increase their chances to become allocated using large pages)
                                     bitarr    = (BYTE*)            BigAlloc (bitarrsize                            , LargePageMode);    if (!bitarr && bitarrsize!=0) return;
                                     chunkarr  = (Chunk*)           BigAlloc (hashsize     * sizeof(Chunk)          , LargePageMode);    if (!chunkarr)  return;
    if (!CONTENT_DEFINED_CHUNKING)  {hasharr   = (StoredHashValue*) BigAlloc (total_chunks * sizeof(StoredHashValue), LargePageMode);    if (!hasharr)   return;}
                                     slicehash.alloc(LargePageMode);
    if (CONTENT_DEFINED_CHUNKING)   {startarr  = (Offset*)          BigAlloc (total_chunks * sizeof(Offset)         , LargePageMode);    if (!startarr)  return;}
    if (COMPARE_DIGESTS)            {digestarr = (Digest*)          BigAlloc (total_chunks * sizeof(Digest)         , LargePageMode);    if (!digestarr) return;}

    my_memset (bitarr,   0, bitarrsize);
    my_memset (chunkarr, 0, hashsize*sizeof(*chunkarr));   if (NOT_FOUND!=0)  {fprintf(stderr, "\nHashTable::HashTable() error: NOT_FOUND!=0\n");  abort();}
    _errcode = NO_ERRORS;
  }
  ~HashTable() {BigFree(digestarr); BigFree(startarr); BigFree(hasharr); BigFree(chunkarr); BigFree(bitarr);}

  // Return errcode if any
  int errcode()  {return _errcode!=NO_ERRORS? _errcode : slicehash.errcode;}

  // How much memory required for hash tables with given file and compression method settings
  Offset memreq() {return hs * sizeof(*chunkarr)
                        + total_chunks * ((CONTENT_DEFINED_CHUNKING? sizeof(*startarr) : sizeof(*hasharr)) + (COMPARE_DIGESTS? sizeof(*digestarr) : 0))
                        + bitarrsize
                        + slicehash.memreq;}

  // Performed once for each block read
  void prepare_buffer (Offset offset, char *buf, int size)
  {
    if (PRECOMPUTE_DIGESTS) {                                              // Save chunk digests for secondary, reliable match checking
      Chunk curchunk = offset/L;
      for (char *p = buf;  (buf+size)-p >= L;  p += L)
        PrepDigest.compute (p, L, &digestarr[curchunk++]);
    }
    slicehash.prepare_buffer (offset, buf, size);
  }


  // A quick first probe using bitarr[]
  template <unsigned ACCELERATOR>  void prefetch_check_match_possibility (HashValue hash)  {if    (ACCELERATOR!=0)  prefetch(bitarr[hash>>bitshift]);}
  template <unsigned ACCELERATOR>  bool check_match_possibility          (HashValue hash)  {return ACCELERATOR!=0?          (bitarr[hash>>bitshift]  &  (1<<(size_t(hash)&(CHAR_BIT-1)))) : true;}
  template <unsigned ACCELERATOR>  void mark_match_possibility           (HashValue hash)  {if    (ACCELERATOR!=0)           bitarr[hash>>bitshift]  |=  1<<(size_t(hash)&(CHAR_BIT-1));}


#define stored_hash(hash2)            ((hash2)>>(CHAR_BIT*(sizeof(BigHash)-sizeof(StoredHashValue))))   /* value saved in hasharr[] */
#define index_hash(hash2)             (hash2)                                                           /* value used to index chunkarr[] */

  // Run add_hash0/prefetch_match0/find_match0 with index/stored values deduced in the SAME way from hash2
  Chunk add_hash (void *p, int i, int block_size, Chunk curchunk, BigHash hash2, Offset new_offset)
  {
    return add_hash0<false> (p, i, block_size, curchunk, index_hash(hash2), stored_hash(hash2), new_offset);
  }
  void prefetch_match (BigHash hash2)
  {
    return prefetch_match0 (index_hash(hash2));
  }
  Chunk find_match (void *p, int i, int block_size, BigHash hash2, Offset new_offset)
  {
    return find_match0 (p, i, block_size, index_hash(hash2), stored_hash(hash2), new_offset);
  }


#define first_hash_slot(index)        (index)                              /* the first chunkarr[] slot */
#define next_hash_slot(index,h)       ((h)*123456791+((h)>>16)+462782923)  /* jump to the next chunkarr[] slot */
#define hash_index(h)                 ((h)&hashsize1)                      /* compute chunkarr[] index for given hash value h;  we prefer to use lower bits since higher ones may be shared with stored_hash value */

#define chunkarr_value(hash,chunk)    ((Chunk(hash)&hash_mask)+(chunk))    /* combine hash and number of chunk into the one Chunk value for storing in the chunkarr[] */
#define get_hash(value)               ((value)&hash_mask)                  /* get hash from the combined value */
#define get_chunk(value)              ((value)&chunknum_mask)              /* get chunk number from the combined value */

#define speed_opt                     true                                 /* true: don't use slicehash to try >1 match in -m5 */

  // Add chunk pointed by p to hash, returning equivalent previous chunk
  template <bool CDC>
  Chunk add_hash0 (void *p, int i, int block_size, Chunk curchunk, BigHash index, StoredHashValue stored_value, Offset new_offset)
  {
    CDC && pc.find_match++;
    CDC || (hasharr[curchunk] = stored_value);     // save hash of chunk for primary, quick match checking
    if (curchunk == NOT_FOUND)  return NOT_FOUND;  // it's impossible to hash this chunk number since it's used as signal value

    size_t h = first_hash_slot(index);  int limit = MAX_HASH_CHAIN;  Chunk found = NOT_FOUND,  saved_hash = chunkarr_value(index,0);
    for (Chunk value;  (value = chunkarr[hash_index(h)]) != NOT_FOUND  &&  --limit;  )
    {
      // First check a few hash bits stored in unused chunkarr[] item bits
      if (get_hash(value) == saved_hash)
      {
        Chunk chunk = get_chunk(value);
        Offset match_offset  =  new_offset - start(chunk);
        if (match_offset < pc.max_offset)  CDC && pc.check_hasharr++;
        // Replace in hash chunk with the same digest (-m1..-m3) or hash value (-m4/-m5), supposing that it has the same contents
        if (CDC || hasharr[chunk] == stored_value)
        {
          if (match_offset < pc.max_offset)  CDC && pc.hash_found++;                                               // reuse chunkarr[] item only when...
          if (!COMPARE_DIGESTS?  (speed_opt || slicehash.check (chunk, p, i, block_size))                   // .. subblocks around has the same 1-bit digests (-m5) or always (-m4)
                              :  (0==memcmp(digestarr[chunk], digestarr[curchunk], sizeof(*digestarr))))    // .. entire block has the same 160-bit digest (-m1..-m3)
          {
            found=chunk; break;
          }
        }
      }
      h++, ((limit&3)==0) && (CDC && pc.find_match_memaccess++, h=next_hash_slot(index,h));  // compute next hash slot
    }
    chunkarr[hash_index(h)] = chunkarr_value(index,curchunk);
    return found;
  }

  // Prefetch chunkarr[] element for the find_match0()
  void prefetch_match0 (BigHash index)
  {
    size_t h = first_hash_slot(index);
    prefetch(chunkarr[hash_index(h)]);
  }

  // Find previous L-byte chunk with the same contents
  Chunk find_match0 (void *p, int i, int block_size, BigHash index, StoredHashValue stored_value, Offset new_offset)
  {
    pc.find_match++;
    size_t h = first_hash_slot(index);  int limit = MAX_HASH_CHAIN;  Chunk saved_hash = chunkarr_value(index,0);
    for (Chunk value;  (value = chunkarr[hash_index(h)]) != NOT_FOUND  &&  --limit;  )
    {
      // First check a few hash bits stored in unused chunkarr[] item bits
      if (get_hash(value) == saved_hash)
      {
        Chunk chunk = get_chunk(value);
        Offset match_offset  =  new_offset - Offset(chunk)*L;
        if (match_offset < pc.max_offset)  pc.check_hasharr++;
        // If hash value is the same...
        if (hasharr[chunk] == stored_value)
        {
          if (match_offset < pc.max_offset)  pc.hash_found++;
          if (!COMPARE_DIGESTS)
          {
            // ... we either suppose that chunks are the same (for -m4), check 1-bit digests of subchunks around (for -m5) ...
            if (slicehash.check (chunk, p, i, block_size))
              return chunk;
            else if (speed_opt)
              return NOT_FOUND;
          }
          else
          {
            // ... or compare 160-bit chunk digests (for -m3)
            Digest dig;
            MainDigest.compute (p, L, &dig);
            if (0==memcmp(dig, digestarr[chunk], sizeof(dig)))
              return chunk;
          }
        }
      }
      h++, ((limit&3)==0) && (pc.find_match_memaccess++, h=next_hash_slot(index,h));  // compute next hash slot
    }
    return NOT_FOUND;
  }

  // Length of match, in bytes
  unsigned match_len (Chunk start_chunk, char *min_p, char *start_p, char *last_p, Offset offset, char *buf, unsigned *add_len)
  {
    Offset new_offset = offset+(start_p-buf);
    Offset old_offset = Offset(start_chunk)*L;
    if (new_offset-old_offset < pc.max_offset)  pc.check_len++;
    char *p = start_p;  *add_len = 0;

    // Comparing with data before the current block
    if (COMPARE_DIGESTS)
    {
      // -m3: check match by using saved digests of old chunks
      while (p += L, (old_offset += L) < offset)        // Skip checking first chunk since it was already done in find_match()
      {
        if (last_p-p < L)                               // We have no L-byte chunk to digest
          goto stop;

        Digest dig;                                     // Compare two L-byte blocks by comparison of their digests
        MainDigest.compute (p, L, &dig);
        if (0!=memcmp(dig, digestarr[old_offset/L], sizeof(dig)))
          goto stop;
      }
    }
    else if (old_offset < offset)
    { // -m4/-m5: check match by rereading old data from infile

      // First, compare bytes before match start (which is rounded to L-byte chunk boundary)
      int n = mymin (old_offset, mymin(L, start_p-min_p));   // how much bytes we can check
      if (n > 0  &&  !ROUND_MATCHES)
      {
        // Compare n bytes before start_p
        char *old = (char*) alloca(n);
        int len = mmap_infile.read(&old,old,old_offset-n,n), i;
        if (len != n)  goto stop;
        for (i=1;  i <= n  &&  start_p[-i] == old[n-i];  i++);
        *add_len = i-1;
      }

      // Second, compare bytes after match start
      const int BUFSIZE = 4096;
      for (;  old_offset < offset;  old_offset += BUFSIZE)
      {
        char *old, oldbuf[BUFSIZE];                                   // Buffer for old data
        int len = mmap_infile.read(&old,oldbuf,old_offset,BUFSIZE);   // Read old data from file
        if (len != BUFSIZE)  goto stop;                               // If there was any problem reading entire buf
        for (char *q = old;  q < old+len;  p++, q++)
          if (p==last_p  ||  *p != *q)  goto stop;                    // Exit function once we've found end of match
      }
    }
    else if (!ROUND_MATCHES)
    {
      // -m4/-m5: compare bytes (that are present in current block) before match start
      int i, n = mymin (old_offset-offset, mymin(L, start_p-min_p));   // how much bytes we can check
      for (i=1;  i <= n  &&  start_p[-i] == (buf+(old_offset-offset))[-i];  i++);
      *add_len = i-1;
    }

    // Comparing with data in the current block
    for (char *q = buf+(old_offset-offset);
         p < last_p  &&  *p == *q;
         p++,q++);

    stop:  return p-start_p + *add_len;
  }


  // Chunk start position
  Offset start (Chunk chunk) {return CONTENT_DEFINED_CHUNKING? startarr[chunk] : Offset(chunk)*L;}

  // Chunk size in -m1/-m2 mode
  Offset chunksize_CDC (Chunk chunk)  {return startarr[chunk+1] - startarr[chunk];}

  // Индексировать новый блок и вернуть смещение до эквивалентного ему старого (или 0)
  Offset find_match_CDC (Offset offset, void *p, int size, BYTE *vhashes);
};

Offset HashTable::find_match_CDC (Offset offset, void *p, int size, BYTE *vhashes)
{
  // we have space allocated only for total_chunks chunks
  if (++curchunk >= total_chunks)   return 0;
  startarr[curchunk] = offset;

  // compute digest and hashes of the new chunk (160+64+32 unused bits == 2*128)
  if (sizeof(*digestarr)+sizeof(BigHash) > 2*VMAC_TAG_LEN_BYTES)
    {fprintf(stderr, "\nfind_match_CDC hashsize error: %d+%d > 2*%d\n", int(sizeof(*digestarr)), int(sizeof(BigHash)), VMAC_TAG_LEN_BYTES);  abort();}
  memcpy (digestarr+curchunk, vhashes, sizeof(*digestarr));
  BigHash index  =  *(BigHash*) (vhashes + sizeof(*digestarr));

  // найти в хеш-таблице старый блок, эквивалентный новому, и заменить его новым блоком
  Chunk chunk = add_hash0<true> (p, 0, 0, curchunk, index, 0, offset);

  // если найден старый эквивалентный блок, то возвратить расстояние до него
  if (chunk!=NOT_FOUND && chunksize_CDC(chunk)==size) {
    if (offset < pc.max_offset)
      pc.check_len++,
      pc.record_match++,
      pc.total_match_len += size;
    return offset-startarr[chunk];
  } else {
    return 0;
  }
}



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Match handling ***********************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Structure storing LZ match info
struct LZ_MATCH
{
  LZ_MATCH() : src(Offset(-1)), dest(Offset(-1)), len(STAT(-1)) {}
  Offset src, dest;  // LZ match source & destination absolute positions in file
  STAT   len;        // Match length
};

// Compare LZ matches by source position (for lz_matches[])
bool order_by_LZ_match_src (const LZ_MATCH &left, const LZ_MATCH &right)
{
  return (left.src < right.src);
}


// Maximum number of STAT values per compressed block. For every L input bytes, we can write up to 4 STAT values to statbuf
//#define MAX_STATS_PER_BLOCK(block_size, L) (((block_size)/(L)+1)*4)

// FUTURE_LZ needs more space because matches are moved to their source blocks
// CONTENT_DEFINED_CHUNKING needs more space because there is no lower limit on the match size
// So we just alloc block_size bytes
#define MAX_STATS_PER_BLOCK(block_size, L) ((block_size)/sizeof(STAT))

// Number of STAT values used to encode one LZ match
#define STATS_PER_MATCH(ROUND_MATCHES) (ROUND_MATCHES? 3:4)

// Encode one LZ record to stat[]
#define ENCODE_LZ_MATCH(stat, ROUND_MATCHES, L,  lit_len, lz_match_offset, lz_match_len)                                                             \
  unsigned L1 = (ROUND_MATCHES? L : 1);                                           /* lz_match_len should be divisible by L in -m3 mode */            \
  *stat++ = (lit_len);                                                                                                                               \
  *stat++ = (lz_match_offset)/L1;                                                                                                                    \
  if (!ROUND_MATCHES)  *stat++ = ((lz_match_offset)/L1) >> STAT_BITS;                                                                                \
  *stat++ = ((lz_match_len)-L)/L1;

// Decode one LZ record from stat[]
#define DECODE_LZ_MATCH(stat, FUTURE_LZ, ROUND_MATCHES, L, basic_pos,  lit_len, LZ_MATCH_TYPE, lz_match)                                             \
  unsigned L1              = (ROUND_MATCHES? L : 1);                              /* lz_match_len should be divisible by L in -m3 mode */            \
  unsigned lit_len         = *stat++;                                             /* length of literal (copied from in[]) */                         \
  Offset   lz_match_offset = *stat++;                                             /* LZ.dest-LZ.src (divided by L when ROUND_MATCHES==true) */       \
  if (!ROUND_MATCHES)         lz_match_offset += Offset(*stat++) << STAT_BITS;    /* High word of lz_match_offset */                                 \
                              lz_match_offset *= L1;                                                                                                 \
  LZ_MATCH_TYPE lz_match;                                                                                                                            \
                lz_match.len = (*stat++)*L1 + L;                                                                                                     \
  if (!FUTURE_LZ) {                                                                                                                                  \
           lz_match.dest   = (basic_pos) + lit_len;                                                                                                  \
           lz_match.src    = lz_match.dest/L1*L1 - lz_match_offset;                                                                                  \
  } else {                                                                                                                                           \
           lz_match.src    = (basic_pos) + lit_len;                                                                                                  \
           lz_match.dest   = lz_match.src + lz_match_offset;                                                                                         \
  }


// Копирует данные из буфера в буфер, идя в порядке возрастания адресов
// (это важно, поскольку буфера могут пересекаться и в этом случае нужно
// размножить существующие данные)
void memcpy_lz_match (void* _dest, void* _src, unsigned len)
{
  if (len) {
    char *dest = (char*)_dest,  *src = (char*)_src;
    do { *dest++ = *src++;
    } while (--len);
  }
}


// Include compression algorithms
#include "compress_inmem.cpp"
#include "compress_cdc.cpp"
#include "compress.cpp"


// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Single block decompressor for IO-LZ **************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Decompress data using stat[] and in[] and return original data in outbuf[]. Returns TRUE on successful decompression
bool decompress (bool ROUND_MATCHES, unsigned L, FILE *fout, Offset block_start, STAT *stat, char *in, char *inend, char *outbuf, char *outend)
{
  STAT *statend = (STAT*)in;
  char *out = outbuf;

  while (statend-stat >= STATS_PER_MATCH(ROUND_MATCHES))
  {
    // Like in original LZ77, LZ matches and literals are strictly interleaved
    DECODE_LZ_MATCH(stat, false, ROUND_MATCHES, L, block_start+(out-outbuf),  lit_len, LZ_MATCH, lz_match);
    if (lit_len>inend-in || lit_len+lz_match.len>outend-out || lz_match.src>=lz_match.dest)  return false;   // Bad compressed data: in/out buffer overflow or src>=dest

    // First, copy literal data
    my_memcpy (out, in, lit_len);
    in  += lit_len;
    out += lit_len;

    // Second, copy LZ match data from previous blocks
    if (lz_match.src < block_start)
    {
      unsigned bytes = mymin (lz_match.len, block_start-lz_match.src);
      file_seek(fout, lz_match.src);
      file_read(fout, out, bytes);
      out          += bytes;
      lz_match.src += bytes;
      lz_match.len -= bytes;
    }

    // Third, copy LZ match data from the current block
    memcpy_lz_match (out, outbuf + (lz_match.src-block_start), lz_match.len);
    out += lz_match.len;
  }

  // Copy literal data up to the block end
  if (inend-in != outend-out)  return false;    // Bad compressed data
  my_memcpy(out, in, inend-in);
  return true;
}



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Memory manager ***********************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Manages memory in fixed-size chunks in order to prevent uncontrolled memory fragmentation provided by malloc()
class MEMORY_MANAGER
{
public:  // ******************************************************* HIGH-LEVEL API: OPERATIONS ON VARIABLE-SIZED MEMORY AREAS *************
  typedef uint32 INDEX;                                          // Index of chunk

  INDEX save (char *ptr, int len) {                              // Save contents of memory area
    INDEX index=INVALID_INDEX, first_index=INVALID_INDEX, prev_index=INVALID_INDEX;
    while (len>0) {
      index = allocate();
      if (prev_index != INVALID_INDEX)  set_next_index(prev_index, index);  else first_index = index;
      prev_index = index;
      int bytes = mymin(len, USEFUL_CHUNK_SPACE);
      my_memcpy (data_ptr(index), ptr, bytes);
      ptr += bytes;
      len -= bytes;
    }
    set_next_index(index, INVALID_INDEX);
    return first_index;
  }
  void restore (INDEX index, char *ptr, int len) {               // Restore saved contents
    while (len>0) {
      int bytes = mymin(len, USEFUL_CHUNK_SPACE);
      my_memcpy (ptr, data_ptr(index), bytes);
      ptr += bytes;
      len -= bytes;
      index = next_index(index);
    }
  }
  void free (INDEX index) {                                      // Free saved contents
    while (index != INVALID_INDEX) {
      INDEX next = next_index(index);
      mark_as_free(index);
      index = next;
    }
  }

private: // ******************************************************* MID-LEVEL API: OPERATIONS ON CHUNKS ***********************************
  INDEX allocate() {                                             // Allocate one chunk and return its index
    if (first_free == INVALID_INDEX)  allocate_block();
    INDEX free_chunk = first_free;
    first_free = next_index(first_free);
    used_chunks++;
    return free_chunk;
  }
  void mark_as_free (INDEX index) {                              // Mark chunk as free
    set_next_index(index, first_free);
    first_free = index;
    used_chunks--;
  }
  INDEX next_index (INDEX index) {                               // Returns contents of "next chunk" field for given chunk
    return *(INDEX*)chunk_ptr(index);
  }
  void set_next_index (INDEX index, INDEX next_index) {          // Sets "next chunk" field of given chunk to the value of next_index
    *(INDEX*)chunk_ptr(index) = next_index;
  }
  char *data_ptr (INDEX index) {                                 // Data part of given chunk
    return chunk_ptr(index) + sizeof(INDEX);
  }

private: // ******************************************************* LOW-LEVEL API: OPERATIONS ON BLOCKS ***********************************
  char *chunk_ptr (INDEX index) {                                // Address of chunk with given index
    return block_addr[index>>lbK] + (index&K1)*CHUNK_SIZE;
  }
  void allocate_block() {                                        // Allocate one more block of memory and add its chunks to the chain of free chunks
    char *p = new char[aBLOCK_SIZE];
    block_addr.push_back(p);
    int block = block_addr.size()-1;
    for(INDEX i=block*K; i<(block+1)*K-1; i++)
      set_next_index(i,i+1);
    set_next_index ((block+1)*K-1, first_free);
    first_free = block*K;
    if (first_free == INVALID_INDEX)  first_free++;
  }

private: // ******************************************************* VARIABLES AND CONSTANTS ***********************************************
  std::vector<char*> block_addr;
  INDEX first_free;
  size_t used_chunks, useful_memory;

  static const size_t CHUNK_SIZE=64, USEFUL_CHUNK_SPACE = CHUNK_SIZE-sizeof(INDEX);
  static const size_t aBLOCK_SIZE=1*mb, K=aBLOCK_SIZE/CHUNK_SIZE, K1=K-1, lbK=14;

public:
  static const INDEX INVALID_INDEX=0;
  MEMORY_MANAGER (size_t memlimit)   {first_free=INVALID_INDEX; used_chunks=0; useful_memory=(memlimit/aBLOCK_SIZE*aBLOCK_SIZE/CHUNK_SIZE-1)*USEFUL_CHUNK_SPACE; CHECK(FREEARC_ERRCODE_INTERNAL,  K==(1<<lbK),  (s,"INTERNAL ERROR: K!=(1<<lbK)"));}
  Offset current_mem()               {return used_chunks*CHUNK_SIZE;}
  Offset max_mem()                   {return block_addr.size()*aBLOCK_SIZE;}
  Offset available_space()           {return useful_memory > used_chunks*USEFUL_CHUNK_SPACE? useful_memory-used_chunks*USEFUL_CHUNK_SPACE : 0;}
  static size_t needmem (size_t len) {return ((len-1)/USEFUL_CHUNK_SPACE+1)*CHUNK_SIZE;}
};



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Future-LZ match handling *************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Statistics! Statistics! Statistics!
Offset total_matches=0, cur_matches=0, max_matches=0, total_bytes=0, cur_bytes=0, max_bytes=0, total_reads=0;
void PLUS_MATCH(unsigned bytes)  {if (bytes>0)  bytes+=16;
                                  total_matches++;    cur_matches++;    max_matches=mymax(cur_matches, max_matches);
                                  total_bytes+=bytes; cur_bytes+=bytes; max_bytes  =mymax(cur_bytes,   max_bytes);}
void MINUS_MATCH(unsigned bytes) {if (bytes>0)  bytes+=16;
                                  cur_matches--;      cur_bytes-=bytes;}
void PLUS_READ()                 {total_reads++;}


// Structure storing Future-LZ match info
struct FUTURE_LZ_MATCH : LZ_MATCH
{
  MEMORY_MANAGER::INDEX  index;      // Index of data from the match saved by MEMORY_MANAGER
  FUTURE_LZ_MATCH() : index(MEMORY_MANAGER::INVALID_INDEX) {}

  // Save match data to buffers provided by MEMORY_MANAGER
  void save_match_data (MEMORY_MANAGER &mm, char *ptr)
  {
    index = mm.save (ptr, len);
    PLUS_MATCH(len);
  }

  // Copy match data to ptr
  void restore_match_data (MEMORY_MANAGER &mm, char *ptr) const
  {
    mm.restore (index, ptr, len);
  }

  // Copy match data to ptr
  void restore_match_data (MEMORY_MANAGER &mm, char *ptr, char *buf, Offset buf_start) const
  {
    if (index != MEMORY_MANAGER::INVALID_INDEX)
      mm.restore (index, ptr, len);
    else
      memcpy_lz_match (ptr, buf+(src-buf_start), len);
  }

  // Free memory allocated by match data
  void free(MEMORY_MANAGER &mm) const
  {
    mm.free(index);
    MINUS_MATCH (index!=MEMORY_MANAGER::INVALID_INDEX? len : 0);
  }

  // Pseudo-match used to mark positions where restore_from_disk() should be called
  void set_marking_point()       {len=0;}
  bool is_marking_point() const  {return len==0;}
};

typedef std::multiset<FUTURE_LZ_MATCH>  LZ_MATCH_HEAP;                            // Used to store matches ordered by LZ destination
typedef LZ_MATCH_HEAP::iterator         LZ_MATCH_ITERATOR;
typedef LZ_MATCH_HEAP::reverse_iterator LZ_MATCH_REVERSE_ITERATOR;

// Compare LZ matches by destination position (for LZ_MATCH_HEAP)
bool operator < (const FUTURE_LZ_MATCH &left, const FUTURE_LZ_MATCH &right)
{
  return (left.dest < right.dest);
}



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Virtual memory manager ***************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct VIRTUAL_MEMORY_MANAGER
{
  char *vmfile_name;                        // File used as virtual memory
  FILE *vmfile;                             // -.-
  Offset VMBLOCK_SIZE;                      // VM block size
  char *vmbuf;                              // Buffer temporarily storing one VM block contents
  std::stack<unsigned> free_blocks;         // List of free blocks (that were allocated previosly)
  unsigned new_block;                       // Next block to alloc if free_blocks list is empty
  Offset total_read, total_write;           // Bytes read/written to disk by VMM

  VIRTUAL_MEMORY_MANAGER (char *_vmfile_name, Offset _VMBLOCK_SIZE)  :  vmfile_name(_vmfile_name), vmfile(NULL), vmbuf(NULL), VMBLOCK_SIZE(_VMBLOCK_SIZE), new_block(0), total_read(0), total_write(0) {}
  ~VIRTUAL_MEMORY_MANAGER() {delete vmbuf;  if(vmfile) {fclose(vmfile); remove(vmfile_name);}}
  Offset current_mem()      {return max_mem() - free_blocks.size()*VMBLOCK_SIZE;}
  Offset max_mem()          {return new_block*VMBLOCK_SIZE;}

  // Save matches with largest LZ.dest to disk
  void save_to_disk (MEMORY_MANAGER &mm, LZ_MATCH_HEAP &lz_matches)
  {
    if (!vmbuf)    vmbuf  = new char[VMBLOCK_SIZE];
    if (!vmfile)   vmfile = fopen(vmfile_name, "w+b");

    // Encode matches to the block, while it has enough space
    Offset min_dest = Offset(-1);
    LZ_MATCH_REVERSE_ITERATOR lz = lz_matches.rbegin();
    char *p = vmbuf;
    for (;;)
    {
      lz++;
      if (lz == lz_matches.rend())                        break;      // There are no more matches in the heap :D
      if (lz->index == MEMORY_MANAGER::INVALID_INDEX)     continue;   // Match doesn't contain match data - skip it
      if (vmbuf+VMBLOCK_SIZE-p  <  24+lz->len)            break;      // Not enough space in the block to save this match - go writing block to the disk

      *(STAT*)p = lz->len;   *(Offset*)(p+4) = lz->src;   *(Offset*)(p+12) = min_dest = lz->dest;
      lz->restore_match_data(mm,p+20);
      p += 20 + lz->len;

      lz->free(mm);
      lz_matches.erase(*lz);
    }
    *(STAT*)p = 0;       // End-of-block mark

    // Save block to disk, and add to the heap pseudo-match marking the restore point
    unsigned block;  if(free_blocks.empty())  block=new_block++;  else block=free_blocks.top(), free_blocks.pop();     // First free block in the file
    file_seek (vmfile, block*VMBLOCK_SIZE);
    file_write(vmfile, vmbuf,VMBLOCK_SIZE);
    total_write += VMBLOCK_SIZE;
    FUTURE_LZ_MATCH mark;  mark.src=block;  mark.dest=min_dest;  mark.set_marking_point();
    lz_matches.insert(mark);
  }


  // Restore matches, pointed by mark, from disk
  void restore_from_disk (MEMORY_MANAGER &mm, LZ_MATCH_HEAP &lz_matches, LZ_MATCH_ITERATOR &mark)
  {
    // Free up enough memory to ensure that there are space to restore the block
    while (mm.available_space() < VMBLOCK_SIZE)   save_to_disk (mm, lz_matches);

    // Read block from disk
    unsigned block = mark->src;
    file_seek (vmfile, block*VMBLOCK_SIZE);
    file_read (vmfile, vmbuf,VMBLOCK_SIZE);
    total_read += VMBLOCK_SIZE;
    free_blocks.push(block);

    // Restore matches encoded in the block
    for (char *p=vmbuf;  *(STAT*)p != 0;)     // Until end-of-block mark
    {
      FUTURE_LZ_MATCH lz;   lz.len = *(STAT*)p;   lz.src = *(Offset*)(p+4);   lz.dest = *(Offset*)(p+12);
      lz.save_match_data(mm,p+20);
      lz_matches.insert(lz);
      p += 20 + lz.len;
    }
  }
};



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Single block Future-LZ decompressor **************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Decompress data using stat[] and in[] and return original data in outbuf[]. Returns TRUE on successful decompression
bool decompress_FUTURE_LZ (bool ROUND_MATCHES, unsigned L, FILE *fout, Offset block_start, STAT *statbuf, STAT *statend, char *in, char *inend, char *outbuf, char *outend,
                           MEMORY_MANAGER &mm, VIRTUAL_MEMORY_MANAGER &vm, LZ_MATCH_HEAP &lz_matches, unsigned maximum_save)
{
  Offset block_end = block_start + (outend-outbuf);   // Absolute file position corresponding to end of the current block
  char *out = outbuf;

  // 1. Insert into lz_matches matches with LZ.dest in the current block
  Offset block_pos = block_start;
  for (STAT *stat = statbuf;  statend-stat >= STATS_PER_MATCH(ROUND_MATCHES);  )
  {
    DECODE_LZ_MATCH(stat, true, ROUND_MATCHES, L, block_pos,  lit_len, FUTURE_LZ_MATCH, lz_match);
    if (lz_match.src<block_pos || lz_match.src>=block_end || lz_match.len>block_end-lz_match.src || lz_match.dest<=lz_match.src)  return false;    // Bad compressed data
    if (lz_match.dest < block_end)
      lz_matches.insert(lz_match), PLUS_MATCH(0);
    block_pos = lz_match.src;
  }

  // 2. LZ decompression loop, processing all LZ matches with LZ.dest in the current block
  for (LZ_MATCH_ITERATOR lz_match = lz_matches.begin();  lz_match->dest < block_end;  lz_match = lz_matches.begin())
  {
    if (lz_match->is_marking_point()) {
      vm.restore_from_disk (mm, lz_matches, lz_match);
    } else {
      // Copy literal data up to match start
      int lit_len = (lz_match->dest - block_start) - (out-outbuf);
      if ((lz_match->dest < block_start+(out-outbuf))  ||  (in+lit_len > inend)  ||  (out+lit_len+lz_match->len > outend))  return false;    // Bad compressed data
      my_memcpy (out, in, lit_len);
      in  += lit_len;
      out += lit_len;

      // Copy match data
      if (lz_match->len >= maximum_save  &&  lz_match->src < block_start) {
        file_seek(fout, lz_match->src);
        file_read(fout, out, lz_match->len);   PLUS_READ();
      } else {
        lz_match->restore_match_data (mm, out, outbuf, block_start);
      }
      out += lz_match->len;
      lz_match->free(mm);
    }
    lz_matches.erase(lz_match);
  }
  // Copy literal data up to the block end
  if (inend-in != outend-out)  return false;    // Bad compressed data
  my_memcpy (out, in, inend-in);

  // 3. Insert into lz_matches matches with LZ.dest in future blocks
  block_pos = block_start;
  for (STAT *stat = statbuf;  statend-stat >= STATS_PER_MATCH(ROUND_MATCHES);  )
  {
    DECODE_LZ_MATCH(stat, true, ROUND_MATCHES, L, block_pos,  lit_len, FUTURE_LZ_MATCH, lz_match);
    if (lz_match.dest >= block_end)
    {
      if (lz_match.len >= maximum_save)  PLUS_MATCH(0);
      else {
        while (lz_match.len > mm.available_space())   vm.save_to_disk (mm, lz_matches);
        lz_match.save_match_data (mm, outbuf + (lz_match.src-block_start));      // copy match data into dynamically-allocated buffer
      }
      lz_matches.insert(lz_match);
    }
    block_pos = lz_match.src;
  }

  return true;
}



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Error handling ***********************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Exit on error
void error (int ExitCode, char *ErrmsgFormat...)
{
  va_list argp;
  va_start(argp, ErrmsgFormat);
  fprintf  (stderr, "\n  ERROR! ");
  vfprintf (stderr, ErrmsgFormat, argp);
  fprintf  (stderr, "\n");
  va_end(argp);

  exit(ExitCode);
}

#define checked_file_read(f, buf, size)                                           \
{                                                                                 \
  if (file_read(f, (buf), (size)) != (size))                                      \
  {                                                                               \
    fprintf (stderr, "\n  ERROR! Can't read from input file");                    \
    errcode = ERROR_IO;                                                           \
    goto cleanup;                                                                 \
  }                                                                               \
}                                                                                 \

#define checked_file_write(f, buf, size)                                          \
{                                                                                 \
  if (file_write(f, (buf), (size)) != (size))                                     \
  {                                                                               \
    fprintf (stderr, "\n  ERROR! Can't write to output file (disk full?)");       \
    errcode = ERROR_IO;                                                           \
    goto cleanup;                                                                 \
  }                                                                               \
}                                                                                 \



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Background thread ***********************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct BG_COMPRESSION_THREAD : BackgroundThread
{
  static const int BUFFERS = 2;
  unsigned k;
  char *dict;
  index*hashtable[BUFFERS];    // Place for saving info about maximum hashes and their indexes
  char *bufptr[BUFFERS];
  char *buf[BUFFERS];
  STAT *statbuf[BUFFERS];
  STAT *stat_end[BUFFERS];
  STAT *header[BUFFERS];
  unsigned len[BUFFERS];
  unsigned stat_size[BUFFERS];
  unsigned outsize[BUFFERS];

  volatile int errcode;
  bool ROUND_MATCHES, COMPARE_DIGESTS, no_writes;
  hash_func_t hash_func;
  void *hash_obj;
  unsigned BASE_LEN, bufsize, header_size;
  Offset filesize;
  Offset dictsize;
  HashTable& h;
  DictionaryCompressor& inmem;
  MMAP_FILE& infile;
  FILE *fin, *fout, *fstat;
  Event ReadDone, WriteReady, BgThreadFinished;


  BG_COMPRESSION_THREAD (bool _ROUND_MATCHES, bool _COMPARE_DIGESTS, unsigned _BASE_LEN, bool _no_writes, hash_func_t _hash_func, void* _hash_obj, Offset _filesize, Offset _dictsize, unsigned _bufsize, unsigned _header_size, HashTable& _h, DictionaryCompressor& _inmem, MMAP_FILE& _infile, FILE* _fin, FILE* _fout, FILE* _fstat, LPType LargePageMode)
    : errcode(NO_ERRORS), k(0), ROUND_MATCHES(_ROUND_MATCHES), COMPARE_DIGESTS(_COMPARE_DIGESTS), BASE_LEN(_BASE_LEN), no_writes(_no_writes), hash_func(_hash_func), hash_obj(_hash_obj), filesize(_filesize), bufsize(_bufsize), header_size(_header_size), h (_h), inmem(_inmem), infile(_infile), fin(_fin), fout(_fout), fstat(_fstat)
  {
    dictsize = roundUp (mymax(_dictsize,BUFFERS*bufsize), bufsize);   // Dictionary size should be divisible by bufsize and BUFFERS*bufsize at least
    dict = (char*) BigAlloc (dictsize, LargePageMode);
    for (int i=0; i<BUFFERS; i++)
    {
      hashtable[i] = (index*) malloc(sizeof(index) * bufsize/inmem.L*2);                    // For every L bytes in the buffer, we need 2 hash table elements
      statbuf  [i] = (STAT *) malloc(sizeof(STAT ) * (MAX_STATS_PER_BLOCK(bufsize,BASE_LEN)+10));
      header   [i] = (STAT *) calloc(header_size,1);
      if (!dict || !hashtable[i] || !statbuf[i] || !header[i])
        {errcode=ERROR_MEMORY; return;}
    }
  }

  Offset memreq()  {return dictsize + BUFFERS*(sizeof(index)*bufsize/inmem.L*2 + sizeof(STAT)*MAX_STATS_PER_BLOCK(bufsize,BASE_LEN) + header_size);}

  void wait()
  {
    BgThreadFinished.Wait();
    for (int i=BUFFERS; --i>=0; )
    {
      free(header[i]);
      free(statbuf[i]);
      free(hashtable[i]);
    }
    BigFree(dict);
  }

  int read (char **_buf, STAT **_statbuf, STAT **_header, index **_hashtable)
  {
    ReadDone.Wait();
    k = (k+1)%BUFFERS;
    *_buf     = bufptr[k];
    *_statbuf = statbuf[k];
    *_header  = header[k];
    *_hashtable = hashtable[k];
    return len[k];
  }

  void write (unsigned _stat_size, STAT *_statend, unsigned _outsize)
  {
    stat_size[k] = _stat_size;
    stat_end[k]  = _statend;
    outsize[k]   = _outsize;
    WriteReady.Signal();
  }


private:   // Background thread code
  void run()
  {
    Offset pos = 0;  index buf_offset = 0;
    for(int i=1, first_block=1;  ;  buf_offset=(buf_offset+bufsize)%dictsize, i=(i+1)%BUFFERS, first_block=0)    // i = 1 0 1 0 1...  first_block = 1 0 0 0...
    {
      // 1. Read input data
      buf[i] = dict + buf_offset;
      len[i] = infile.read (&bufptr[i], buf[i], pos, bufsize, fin);       // mmap
      if (!COMPARE_DIGESTS  &&  infile.mmapped()) {
        len[i] = file_read (fin, buf[i], bufsize);  bufptr[i] = buf[i];   // file_read
      }
      if (filesize-pos < len[i])  {fprintf (stderr, "\n  ERROR! Input file is larger than filesize specified"); errcode=ERROR_IO; ReadDone.Signal(); break;}  // Ensure that we don't read more than `filesize` bytes

      // 2. Perform b/g processing of input data
      if (hash_func)                                                      // Save checksum of every input block for error-checking during decompression
        hash_func (hash_obj, bufptr[i],len[i], header[i]+3);
      h.prepare_buffer (pos, bufptr[i], len[i]);
      inmem.prepare_buffer (hashtable[i], bufptr[i], len[i]);

      // 3. Wait for output data from prev. block and write them
      if (!first_block)
        WriteReady.Wait();                          // Wait for output data from prev. block
      ReadDone.Signal();                            // Allow to use input data
      if (!first_block && !no_writes)
        {if (!save_data((i-1+BUFFERS)%BUFFERS))  goto cleanup;}

      // 4. Stop thread on EOF
      if (len[i]==0) break;
      pos += len[i];
    }
cleanup:
    BgThreadFinished.Signal();
  }

  // Write compressed block, returning TRUE on success
  bool save_data (int k)
  {
    char *in   = bufptr [k],  *inend   = bufptr [k] + len[k];
    STAT *stat = statbuf[k],  *statend = stat_end[k];

    checked_file_write (fout,  header [k], header_size);
    checked_file_write (fstat, statbuf[k], stat_size[k]);

    while (statend-stat >= STATS_PER_MATCH(ROUND_MATCHES))
    {
      // Like in original LZ77, LZ matches and literals are strictly interleaved
      DECODE_LZ_MATCH(stat, false, ROUND_MATCHES, BASE_LEN, 0,  lit_len, LZ_MATCH, lz_match);
      if (lit_len > inend-in)  return false;   // Bad compressed data

      // Save literal data
      checked_file_write (fout, in, lit_len);
      in += lit_len+lz_match.len;
    }

    // Copy literal data up to the block end
    checked_file_write (fout, in, inend-in);
    return true;

cleanup:
    return false;
  }
};



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Main *********************************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Structure describing one compressed block
struct COMPRESSED_BLOCK
{
  COMPRESSED_BLOCK *next;  // Next block in chain
  Offset start, end;       // First and next-after-last byte of the block
  unsigned size;           // Bytes in uncompressed block
  STAT *header;            // Block header data
  STAT *statbuf, *statend; // LZ matches in the block
};


// Parse -mem option, examples are: -mem100mb, -mem75%, -mem75p, -mem75%-600mb
int64 parse_mem_option (char *option, int *errcode, char spec)
{
  if (*errcode)  return 0;

  // Parse -mem100mb variant
  int64 mem = parseMem (option, errcode, spec);
  if (*errcode==0)  return mem;
  *errcode=0;

  // Parse XX% part
  int percent = 0;
  while (*option >='0'  &&  *option <='9')
    percent = percent*10 + (*option++ - '0');
  if (*option!='%' && *option!='p')  {*errcode=1; return 0;}
  option++;

  // Parse XXmb part
  if      (*option == '\0')   {mem = 0;}                                        // -mem75% variant
  else if (*option++ != '-')  {*errcode=1; return 0;}                           // illegal option
  else                        {mem = parseMem (option, errcode, spec);}         // -mem75%-600mb variant
  return percent*(GetPhysicalMemory()/100) - mem;
}

// Find hash descriptor by the hash name
struct hash_descriptor *hash_by_name (const char *hash_name, int &errcode)
{
  if (errcode)  return NULL;
  for (int i=0; i<elements(hash_descriptors); i++)
    if (strcasecmp (hash_descriptors[i].hash_name, hash_name) == EQUAL)
      {errcode=0; return &hash_descriptors[i];}
  errcode=1; return NULL;
}

// Find hash descriptor by the hash tag
struct hash_descriptor *hash_by_num (int hash_num)
{
  for (int i=0; i<elements(hash_descriptors); i++)
    if (hash_descriptors[i].hash_num == hash_num)
      return &hash_descriptors[i];
  return NULL;
}


void signal_handler(int)
{
  Taskbar_Done();
  error (ERROR_IO, "^Break pressed");
}


int main (int argc, char **argv)
{
  COMMAND_MODE cmdmode = COMPRESSION;
  SREP_METHOD method = SREP_METHOD3;
  const int DEFAULT_ACCEL = 4;
  Offset filesize = Offset(25)*gb;
  const Offset DEFAULT_DICTSIZE = Offset(512)*mb;
  Offset dictsize = DEFAULT_DICTSIZE,  dict_hashsize = 0;
  double GlobalTime0 = GetGlobalTime();
  unsigned L=0, min_match=0, dict_chunk=0, dict_min_match=0, maximum_save=unsigned(-1), accel=9000, ACCELERATOR=9000, vm_block=8*mb, bufsize=8*mb, NumThreads=0;
  bool INDEX_LZ=true, FUTURE_LZ=false, IO_LZ=false, use_mmap=false, delete_input_files=false, print_pc=false;
  char *index_file="",  *tempfile=NULL,  *DEFAULT_TEMPFILE="srep-data.tmp",  *vmfile_name="srep-virtual-memory.tmp",  *option_s="+";
  int errcode=0, warnings=0, verbosity=2, io_accelerator=1;      LPType LargePageMode=TRY;
  struct hash_descriptor *selected_hash = hash_by_name(DEFAULT_HASH, errcode);
  int64 vm_mem = parse_mem_option ("75%", &errcode, 'm');         if (vm_mem > 1536*mb)    _32_only(vm_mem = 1536*mb);
  setbuf(stderr,NULL);    // Disable buffering even if stderr is redirected to file/pipe
  if (errcode)
    error (ERROR_CMDLINE, "Internal error: incorrect default settings");

  //*********************************************************************************************************
  /// PARSE CMDLINE
  //*********************************************************************************************************

  char **filenames = argv,  **next_filename = argv+1;
  while(argv[1])
  {
    if (strequ(argv[1],"-d")) {
      cmdmode = DECOMPRESSION;
    } else if (strequ(argv[1],"-i")) {
      cmdmode = INFORMATION;
    } else if (strequ(argv[1],"-delete")) {
      delete_input_files = true;
    } else if (strequ(argv[1],"-mmap")) {
      use_mmap = true;
    } else if (strequ(argv[1],"-nommap")) {
      use_mmap = false;
    } else if (strequ(argv[1],"-s")  ||  strequ(argv[1],"-s-") ||  strequ(argv[1],"-s+")  ||  (start_with(argv[1],"-s") && (strchr(argv[1],'.')||strchr(argv[1],'e')))) {
      option_s = argv[1]+2;
    } else if (start_with(argv[1],"-m")  &&  in_set(last_char(argv[1]),"bkmg")) {
      maximum_save = parseMem (argv[1]+2, &errcode, 'b');
      if (maximum_save < 10)  errcode=1;
    } else if (start_with(argv[1],"-m")  &&  (isdigit(argv[1][2]) || argv[1][2]=='x')) {
      method  = (argv[1][2]=='x'?  SREP_METHOD_LAST  :  SREP_METHOD(argv[1][2]-'0'));
      errcode = (method<SREP_METHOD_FIRST || method>SREP_METHOD_LAST);
      if      (strequ(argv[1]+3, ""))   INDEX_LZ=true,  FUTURE_LZ=false, IO_LZ=false;
      else if (strequ(argv[1]+3, "f"))  INDEX_LZ=false, FUTURE_LZ=true,  IO_LZ=false;
      else if (strequ(argv[1]+3, "o"))  INDEX_LZ=false, FUTURE_LZ=false, IO_LZ=true;
      else                              errcode=1;
    } else if (strequ(argv[1],"-f")) {
      INDEX_LZ=false, FUTURE_LZ=true, IO_LZ=false;
    } else if (strequ(argv[1],"-a-")) {
      accel = 0;
    } else if (start_with(argv[1],"-a")  &&  isdigit(argv[1][2])) {
      char* endptr;
      accel        =  strtol (argv[1]+2, &endptr, 0);
      ACCELERATOR  =  (*endptr == '/'?  strtol (endptr+1, &endptr, 0)  :  9000);
      if (*endptr != '\0')
        errcode = 1;
    } else if (strequ(argv[1],"-ia-")) {
        io_accelerator = -1;
    } else if (strequ(argv[1],"-ia+")) {
        io_accelerator = 1;
    } else if (strequ(argv[1],"-slp")) {
        LargePageMode = TRY;
    } else if (strequ(argv[1],"-slp-")) {
        LargePageMode = DISABLE;
    } else if (strequ(argv[1],"-slp+")) {
        LargePageMode = FORCE;
    } else if (strequ(argv[1],"-hash-") || strequ(argv[1],"-nomd5")) {
      selected_hash = hash_by_name("", errcode);
    } else if (start_with(argv[1],"-hash=")) {
      selected_hash = hash_by_name(argv[1]+6, errcode);
    } else if (strequ(argv[1],"-v")) {
      verbosity = 1;
    } else if (start_with(argv[1],"-v")) {
      verbosity = parseInt (argv[1]+2, &errcode);
    } else if (start_with(argv[1],"-pc")) {
      print_pc = true;
      pc.max_offset  =  argv[1][3]? parseMem64 (argv[1]+3, &errcode, 'm') : Offset(-1);
    } else if (start_with(argv[1],"-index=")) {
      index_file = argv[1]+7;
    } else if (start_with(argv[1],"-temp=")) {
      tempfile = argv[1]+6;
    } else if (start_with(argv[1],"-vmfile=")) {
      vmfile_name = argv[1]+8;
    } else if (start_with(argv[1],"-vmblock=")) {
      vm_block = parseMem (argv[1]+9, &errcode, 'm');
    } else if (start_with(argv[1],"-mem")) {
      vm_mem = parse_mem_option (argv[1]+4, &errcode, 'm');
    } else if (start_with(argv[1],"-l")) {
      min_match = parseMem (argv[1]+2, &errcode, 'b');
    } else if (start_with(argv[1],"-c")) {
      L = parseMem (argv[1]+2, &errcode, 'b');
    } else if (start_with(argv[1],"-s")) {
      filesize = parseMem64 (argv[1]+2, &errcode, 'b');
    } else if (start_with(argv[1],"-b")) {
      bufsize = parseMem (argv[1]+2, &errcode, 'm');
    } else if (start_with(argv[1],"-dc")) {
      dict_chunk = parseMem (argv[1]+3, &errcode, 'b');
    } else if (start_with(argv[1],"-dl")) {
      dict_min_match = parseMem (argv[1]+3, &errcode, 'b');
    } else if (start_with(argv[1],"-dh")) {
      dict_hashsize = parse_mem_option (argv[1]+3, &errcode, 'm');
    } else if (strequ(argv[1],"-d-")) {
      dictsize = 0;
    } else if (strequ(argv[1],"-d+")) {
      dictsize = DEFAULT_DICTSIZE;
    } else if (start_with(argv[1],"-d")) {
      dictsize = parse_mem_option (argv[1]+2, &errcode, 'm');
    } else if (start_with(argv[1],"-t")) {
      NumThreads = parseInt (argv[1]+2, &errcode);
    } else if (start_with(argv[1],"-rem")) {
      // Command-line remark
    } else if (strequ(argv[1],"--")) {
      argv++;
      do {*next_filename++ = argv[1];} while (*++argv);    // no more options - copy remaining filenames
      break;
    } else if (start_with(argv[1],"-")  &&  !strequ(argv[1],"-")) {
      errcode = 1;
    } else {
      *next_filename++ = argv[1];                          // not an option - copy argv[1] to the filenames list
    }
    if (errcode)
      error (ERROR_CMDLINE, "Invalid option: %s", argv[1]);
    argv++;
  }
  *next_filename = NULL;
  char *_filenames[] = {"","-","-",NULL};

  // (De)compress from stdin to stdout if no filenames are given, but both stdin and stdout are redirected
  if (filenames[1]==NULL  &&  !isatty(fileno(stdin))  &&  !isatty(fileno(stdout)))
    filenames = _filenames;

  const bool INMEM_COMPRESSION         =  (method == SREP_METHOD0);   // In-memory compression (REP algorithm)
  const bool CONTENT_DEFINED_CHUNKING  =  (SREP_METHOD1<=method && method<=SREP_METHOD2);   // Content-defined-chunking deduplication
  const bool ZPAQ_CDC                  =  (method == SREP_METHOD2);   // ZPAQ algorithm of content-defined chunking
  const bool COMPARE_DIGESTS           =  (method <= SREP_METHOD3);   // Check matches by comparison of their digests, otherwise - check matches by rereading old data
  const bool PRECOMPUTE_DIGESTS        =  (method == SREP_METHOD3);   // Split data into fixed-size blocks and precompute their digests prior to main processing cycle
  const bool ROUND_MATCHES             =  (method == SREP_METHOD3) && (dictsize==0);   // Match lengths are multiplies of L, otherwise - arbitrary value (>= min_match for -m4/-m5)
  const bool EXHAUSTIVE_SEARCH         =  (method == SREP_METHOD5);   // Check all matches starting at L/2 length in order to find L-byte match
  if (!L && !min_match)  min_match  =  (CONTENT_DEFINED_CHUNKING? 4096 : 512);       // Default -l values
  if (!L) {
    if (CONTENT_DEFINED_CHUNKING)     L=min_match, min_match=0;                                // For -m1/-m2, -lX===-l0 -cX
    else  L  =  (!EXHAUSTIVE_SEARCH?  min_match  :  rounddown_to_power_of(min_match+1,2)/2);   // Only -m5 performs exhaustive search for ALL matches of min_match bytes or longer
  }
  if (!min_match)        min_match = (CONTENT_DEFINED_CHUNKING? DEFAULT_MIN_MATCH : L);
  if (!dict_min_match)   dict_min_match = min_match;
  if (!dict_chunk)       dict_chunk     = dict_min_match/8;    // For in-memory compression, default chunk size is 1/8 of the minimum match length
  unsigned BASE_LEN = mymin (min_match, dict_min_match);       // Guaranteed minimum match length, so lengths in STAT are encoded minus this value
  if (L!=roundup_to_power_of(L,2) && !CONTENT_DEFINED_CHUNKING) {
    fprintf (stderr, "Warning: -l parameter should be power of 2, otherwise compressed file may be corrupt\n");
    warnings++;
  }
  if (CONTENT_DEFINED_CHUNKING)  dictsize = 0;         // CDC isn't yet compatible with in-memory compression
  if (vm_mem > size_t(-1))    vm_mem = size_t(-1);     // For 32-bit systems (say, 50% of 16gb RAM may be a bit too much). Better, use GetTotalMemoryToAlloc()

  if (filenames[1]==NULL) {
    printf (         "%s: %s\n"
                     "%s    homepage: %s\n"
                     "\n"
                     "Usage: SREP [options] infile [outfile]\n"
                     "   infile/outfile can be specified as \"-\" for stdin/stdout\n"
                     "   \"SREP [options] somefile\" compresses data from somefile to somefile.srep\n"
                     "   \"SREP [options] somefile.srep\" decompresses data back to somefile\n"
                     "   \"SREP [options]\" compresses and \"SREP -d [options]\" decompresses data from stdin to stdout\n"
                     "\n"
                     "Options are:\n"
                     "   -m0: only in-memory compression (REP algorithm)\n"
                     "   -m1: fixed-window content-defined chunking with matches checked by VMAC\n"
                     "   -m2: order-1 content-defined chunking with matches checked by VMAC\n"
                     "   -m3: check matches by VMAC digest (compression memory = 7-8%% of filesize)\n"
                     "   -m4: check matches by rereading old data (compression memory = 3-4%% of filesize)\n"
                     "   -m5/-mx: rereading with byte-accurate matches (compression memory = 7-9%% of filesize)\n"
                     "   -l: minimum LZ match length, default %d\n"
                     "   -c: size of hash chunk, by default as small as required to find all these LZ matches\n"
                     "   -aX[/Y]: alloc X bytes of those Y bits will be set per L input bytes for compression accelerator\n"
                     "            Y=0/1/2/4/8/16/32/64, -a0 is slowest but requires least memory\n"
                     "   -ia-: disable I/O acceleration to reduce memory usage (-m5* only)\n"
                     "   -tN: use N compression threads (only for -m1/-m2)\n"
                     "   -dBYTES: dictionary size for in-memory compression (REP algorithm), default %dmb\n"
                     "   -dhBYTES/-dc/-dl: size of hash / size of hash chunk / minimum match length for in-memory compression\n"
                     "\n"
                     "   -m1..-m5: index-LZ - list of matches saved at the end of compressed file\n"
                     "   -m1f..-m5f: future-LZ - decompression dictionary will hold only future matches\n"
                     "   -m1o..-m5o: I/O LZ - output file used as decompression dictionary\n"
                     "   -memBYTES: amount of RAM used by future-LZ/index-LZ decompression (extra goes into VM file)\n"
                     "      -mem75%% AKA -mem75p means \"use no more than 75%% of RAM\" - that's by default\n"
                     "      -mem600mb means itself\n"
                     "      -mem75%%-600mb means \"use no more than 75%% of RAM minus 600 mb\"\n"
                     "   -mBYTES: don't store matches larger than BYTES on future-LZ/index-LZ decompression\n"
                     "\n"
                     "   -d: decompression (for -m0o..m5o requires only 24 mb of memory besides of OS I/O buffers)\n"
                     "   -i: print info about compressed file\n"
                     "   -delete: delete source file after successful (de)compression\n"
                     "   -sBYTES: explicitly specify filesize (for compression from stdin), default %dgb\n"
                     "   -bBYTES: change compression block size, default %dmb\n"
                     "   -index=FILENAME: read/write index of compressed data into separate file\n"
                     "   -temp=[FILENAME]: keep uncompressed data in the file in stdin-to-stdout mode, default %s\n"
                     "   -vmfile=FILENAME: temporary file used by Virtual Memory manager, default %s\n"
                     "   -vmblock=BYTES: size of one block in VM temporary file, default %dmb\n"
                     "\n"
                     "   -hash=%s: store hash checksums in every block\n"
                     "   -hash-: don't store/check block checksums\n"
                     "   -mmap: use memory-mapped files for match checking\n"
                     "   -slp[+/-/]: force/disable/try(default) large pages support (2mb/4mb)\n"
                     "   -pc[max_offset]: display performance counters [for matches closer than max_offset]\n"
                     "   -s: save printed stats from overwriting; -s+/-s-/-sX.Y: update stats every X.Y seconds\n"
                     "   -v[0..2]: verbosity level\n"
                     "   -rem...: command-line remark\n",
                     program_version, program_description, program_date, program_homepage,
                     int(dictsize/mb), min_match, int(filesize/gb), int(bufsize/mb), DEFAULT_TEMPFILE,  vmfile_name, int(vm_block/mb), HASH_LIST);
    exit (NO_ERRORS);
  }
  if (cmdmode==INFORMATION && filenames[2]) {
    error (ERROR_CMDLINE, "Too much filenames: %s %s", filenames[1], filenames[2]);
  }
  if (filenames[2] && filenames[3]) {
    error (ERROR_CMDLINE, "Too much filenames: %s %s %s", filenames[1], filenames[2], filenames[3]);
  }

  //*********************************************************************************************************
  /// OPEN INPUT/OUTPUT/TEMPORARY FILES
  //*********************************************************************************************************

  char *finame   = filenames[1];
  char *foutname = (cmdmode==INFORMATION? (char*)"nul" : filenames[2]);
  if (!foutname)
  {
    // If second filename isn't provided, then decompress or compress depending on infile extension (.srep or not).
    // Output filename is input filename plus .srep for compression, or minus .srep for decompression.
    foutname = new char [strlen(finame) + strlen(SREP_EXT) + 2];
    strcpy(foutname,finame);
    if (end_with(finame,SREP_EXT)) {
      foutname[strlen(foutname)-strlen(SREP_EXT)] = '\0';
      cmdmode = DECOMPRESSION;
    } else {
      strcat(foutname,SREP_EXT);
    }
  }

  bool single_pass_compression  =  COMPARE_DIGESTS && !FUTURE_LZ;    // -m0..-m3/-m0o..-m3o are the only compression modes having no need to reread input data
  if (cmdmode==COMPRESSION && !single_pass_compression && strequ(finame,"-"))
  {
    if (!tempfile)
      tempfile = DEFAULT_TEMPFILE;
    else if (*tempfile==0)
      error (ERROR_IO, "Reading data to compress from stdin without tempfile isn't supported for this method");
  }
  if (!strequ(finame,"-") && strequ(finame,foutname))
    error (ERROR_IO, "Input and output files should have different names");

  FILE *fin = strequ (finame, "-")? stdin : fopen (finame, "rb");
  if (fin == NULL)  error (ERROR_IO, "Can't open %s for read", finame);
  set_binary_mode (fin);

  FILE *fout = strequ (foutname, "-")? stdout : fopen (foutname, "w+b");
  if (fout == NULL)  error (ERROR_IO, "Can't open %s for write", foutname);
  set_binary_mode (fout);

  FILE *fstat = *index_file? fopen (index_file, cmdmode==COMPRESSION? "wb" : "rb") : (cmdmode==COMPRESSION? fout : fin);
  if (fstat == NULL)  error (ERROR_IO, "Can't open index file %s for write", index_file);

  FILE *ftemp = NULL;

  STAT header[MAX_HEADER_SIZE+MAX_HASH_SIZE];  zeroArray(header);   // header size depends on the *selected_hash properties
  filesize  =  (strequ(finame,"-")? filesize : get_flen(fin));
  Offset origsize = 0,  compsize = 0;
  // Reduce default accel value for small L
  if (accel==9000)         accel        =  mymin (mymax(L/32,1), DEFAULT_ACCEL);  unsigned BITARR_ACCELERATOR = accel*8;
  // Maximum default acceleration level for main loop is 16; larger -a values only increase bitarr[] size
  if (ACCELERATOR==9000)   ACCELERATOR  =  mymin(accel,16);
  // Required to overwrite previous, longer stats line
  const char *newline = strequ(option_s,"")? "\n":"    \b\b\b\b";
  // Interval in seconds between statistics updates
  double TimeInterval = strequ(option_s,"")? 1e-30 : strequ(option_s,"-")? 1e30 : strequ(option_s,"+")? 0.2 : atof(option_s);
  // Last time/origsize when progress indicator was printed
  double LastGlobalTime = 0;  Offset last_origsize = Offset(-1);
  // Miscellaneous
  void *hash_obj = NULL;  if (NumThreads==0)  NumThreads = GetProcessorsCount();
  Install_signal_handler(signal_handler);

  if (cmdmode==COMPRESSION)
  {
    //*********************************************************************************************************
    /// COMPRESSION
    //*********************************************************************************************************

    const int header_size = sizeof(STAT)*BLOCK_HEADER_SIZE + selected_hash->hash_size;  // compressed block header size
    ftemp  =  (tempfile && *tempfile)? fopen (tempfile, "w+b") : (strequ (finame, "-")? fin : fopen (finame, "rb"));
    if (ftemp == NULL)  error (ERROR_IO, "Can't open tempfile %s for write", tempfile);
    if (tempfile && *tempfile)  use_mmap = false;

    // Seed keyed hash like VMAC with the random data
    void *seed = malloc(selected_hash->hash_seed_size);
    if (selected_hash->new_hash)
    {
      cryptographic_prng (seed, selected_hash->hash_seed_size);
      hash_obj = selected_hash->new_hash (seed, selected_hash->hash_seed_size);
    }

    COMPRESSED_BLOCK *first_block,  **last_block = &first_block;                 // head of linked list of blocks in FUTURE_LZ mode
    Offset lz_matches_count = 0;                                                 // LZ matches found
    size_t total_blocks = 0;
    {
      MMAP_FILE  mmap_infile(use_mmap, ftemp, "r", filesize);
      CDC_Global g(CONTENT_DEFINED_CHUNKING, NumThreads);
      HashTable  h(ROUND_MATCHES, COMPARE_DIGESTS, PRECOMPUTE_DIGESTS, INMEM_COMPRESSION, CONTENT_DEFINED_CHUNKING, L, min_match, io_accelerator, BITARR_ACCELERATOR, mmap_infile, filesize, LargePageMode);
      DictionaryCompressor inmem(dictsize, dict_hashsize, dict_min_match, dict_chunk, BASE_LEN, bufsize, BG_COMPRESSION_THREAD::BUFFERS, LargePageMode);
      BG_COMPRESSION_THREAD bg_thread(ROUND_MATCHES, COMPARE_DIGESTS, BASE_LEN, FUTURE_LZ, selected_hash->hash_func, hash_obj, filesize, dictsize, bufsize, header_size, h, inmem, mmap_infile, fin, fout, fstat, LargePageMode);
      double memreq = double(h.memreq()+inmem.memreq()+bg_thread.memreq()) / mb;
      if (g.errcode || h.errcode() || inmem.errcode || bg_thread.errcode)   error (ERROR_MEMORY, "Can't allocate memory: %.0lf mb required (-l64k -a- -ia- options may help)", memreq);
      bg_thread.start();
      if (verbosity > 1)
      {
        char l_c_t_str[100], l_c_a_str[100], hashname_str[100], bufsize_str[100], dictsize_str[100], dict_hashsize_str[100], dict_compressor_str[100];
        sprintf(l_c_t_str," -l%d -c%d -t%d", min_match, L, NumThreads);
        sprintf(l_c_a_str," -l%d -c%d -a%d/%d", min_match, L, accel, ACCELERATOR);
        sprintf(hashname_str,"=%s", selected_hash->hash_name);  showMem (bufsize, bufsize_str);  showMem (dictsize, dictsize_str);  showMem (inmem.hashsize, dict_hashsize_str);
        sprintf(dict_compressor_str," -d%s -dh%s -dl%d -dc%d", dictsize_str, dict_hashsize_str, dict_min_match, dict_chunk);
        fprintf (stderr, "%s (%s): input size %.0lf mb, memory used %.0lf mb, -m%d%s%s%s%s%s -hash%s -b%s%s\n", program_version, program_date,
                         double(filesize/mb), memreq, method, FUTURE_LZ?"f":(IO_LZ?"o":""),
                         INMEM_COMPRESSION? "" : (CONTENT_DEFINED_CHUNKING? l_c_t_str : l_c_a_str),
                         EXHAUSTIVE_SEARCH? (io_accelerator>0? " -ia+":" -ia-") : "",
                         use_mmap? " -mmap":"",  LargePageMode==FORCE? " -slp+" : (LargePageMode==DISABLE? " -slp-" : ""),
                         strequ(selected_hash->hash_name,"")?"-":hashname_str, bufsize_str, dictsize? dict_compressor_str: "");
      }

      // Write compressed file header
      header[0] = BULAT_ZIGANSHIN_SIGNATURE;
      header[1] = SREP_SIGNATURE;
      header[2] = (INDEX_LZ? SREP_FORMAT_VERSION4 : (FUTURE_LZ? SREP_FORMAT_VERSION3 : (ROUND_MATCHES? SREP_FORMAT_VERSION1 : SREP_FORMAT_VERSION2)))
                  + (selected_hash->hash_num       << 8)
                  + (selected_hash->hash_seed_size << 16)
                  +((selected_hash->hash_size-16)  << 24);
      header[3] = BASE_LEN;
      checked_file_write (fout, header, sizeof(STAT)*ARCHIVE_HEADER_SIZE);
      checked_file_write (fout, seed,   selected_hash->hash_seed_size);
      compsize = sizeof(STAT)*ARCHIVE_HEADER_SIZE + selected_hash->hash_seed_size;

#define INDEX_LZ_FOOTER_SIZE (sizeof(STAT)*6)
      if (INDEX_LZ)
        compsize += INDEX_LZ_FOOTER_SIZE;   // accounting for the future write of INDEX_LZ_FOOTER

      // Compress data by 8mb (bufsize) blocks until EOF
      for(;;)
      {
        // Read next input block (saving its copy to tempfile)
        char *buf;  STAT *statbuf, *stat, *header;  index *hashptr;  unsigned literal_bytes;
        int len = bg_thread.read (&buf, &statbuf, &header, &hashptr);   // Read data from file
        if (bg_thread.errcode)  {errcode = bg_thread.errcode; goto cleanup;}
        if (len==0)  goto print_stats;
        if (tempfile && *tempfile) {
          file_seek (ftemp, origsize);
          checked_file_write (ftemp, buf, len);
        }

        // Compress the block
        static STAT static_statbuf[8*mb]; //// temporary!
        inmem.compress (bg_thread.dict, buf,len, hashptr, literal_bytes,INMEM_COMPRESSION?statbuf:static_statbuf,stat);
        if (!INMEM_COMPRESSION)
          {ENCODE_LZ_MATCH(stat,ROUND_MATCHES,BASE_LEN, len+1,Offset(BASE_LEN),BASE_LEN);}  // pseudo-match used to limit matches read from the static_statbuf[]

        if (INMEM_COMPRESSION)
          {}
        else if (CONTENT_DEFINED_CHUNKING)
          compress_CDC (ZPAQ_CDC,L,min_match,origsize,h,g, buf,len,literal_bytes,statbuf,stat);
        else
          switch (ACCELERATOR)
          {
            case  0:  compress< 0>(ROUND_MATCHES,L,min_match,BASE_LEN,origsize,h, buf,len, literal_bytes,static_statbuf,statbuf,stat); break;
            case  1:  compress< 1>(ROUND_MATCHES,L,min_match,BASE_LEN,origsize,h, buf,len, literal_bytes,static_statbuf,statbuf,stat); break;
            case  2:  compress< 2>(ROUND_MATCHES,L,min_match,BASE_LEN,origsize,h, buf,len, literal_bytes,static_statbuf,statbuf,stat); break;
            case  4:  compress< 4>(ROUND_MATCHES,L,min_match,BASE_LEN,origsize,h, buf,len, literal_bytes,static_statbuf,statbuf,stat); break;
            case  8:  compress< 8>(ROUND_MATCHES,L,min_match,BASE_LEN,origsize,h, buf,len, literal_bytes,static_statbuf,statbuf,stat); break;
            case 16:  compress<16>(ROUND_MATCHES,L,min_match,BASE_LEN,origsize,h, buf,len, literal_bytes,static_statbuf,statbuf,stat); break;
            case 32:  compress<32>(ROUND_MATCHES,L,min_match,BASE_LEN,origsize,h, buf,len, literal_bytes,static_statbuf,statbuf,stat); break;
            case 64:  compress<64>(ROUND_MATCHES,L,min_match,BASE_LEN,origsize,h, buf,len, literal_bytes,static_statbuf,statbuf,stat); break;
          }

       {// Fill compressed block header
        unsigned stat_size  =  (char*)stat - (char*)statbuf;
        header[0] = literal_bytes;
        header[1] = len;
        header[2] = (INDEX_LZ? 0 : stat_size);

        // Write compressed block to output file(s)
        bg_thread.write (header[2], stat, literal_bytes);
        if (bg_thread.errcode)  {errcode = bg_thread.errcode; goto cleanup;}

        // Store matches in memory
        if (FUTURE_LZ || INDEX_LZ)
        {
          total_blocks++;
          lz_matches_count += (stat - statbuf) / STATS_PER_MATCH(ROUND_MATCHES);

          // Save header[] and statbuf[] to the COMPRESSED_BLOCK struct
          int blocksize = sizeof(COMPRESSED_BLOCK) + header_size + stat_size;
          COMPRESSED_BLOCK *block = (COMPRESSED_BLOCK *) new char[blocksize];

          block->start = origsize;
          block->size  = len;
          block->end   = block->start + block->size;
          block->header  = (STAT*)(block+1);
          block->statbuf = (STAT*)((char*)block->header + header_size);
          block->statend = (STAT*)((char*)block->statbuf + stat_size);
          my_memcpy(block->header,  header,  header_size);
          my_memcpy(block->statbuf, statbuf, stat_size);

          // Join COMPRESSED_BLOCK structs into linked list
          *last_block = block;
          last_block = &block->next;

          if (ROUND_MATCHES)
            compsize += stat_size / STATS_PER_MATCH(ROUND_MATCHES);   // Добавить размер одного слова STAT из-за того, что данные собираются с ROUND_MATCHES (по 12 байт), а кодироваться будут без него (по 16 байт)

          if (INDEX_LZ)
            compsize += sizeof(STAT);   // accounting for the future write of statsize_buf[]
        }

        // Update statistics
        compsize += literal_bytes + stat_size + header_size;
        origsize += len;}

print_stats:
        if (len==0)  bg_thread.wait();     // Wait until all compressed data are successfully saved to disk
        if (verbosity)
        {
          double GlobalTime = GetGlobalTime()-GlobalTime0;
          if (origsize!=last_origsize  &&  (len==0 || GlobalTime-LastGlobalTime>TimeInterval))
          {
            LastGlobalTime = GlobalTime;  last_origsize = origsize;  double CPUTime = GetCPUTime();
            char temp1[100], temp2[100], temp3[100], temp4[100], temp5[100], temp6[100], temp7[100], counters[200];
            Offset x = pc.check_len-pc.record_match;
            sprintf (counters, "PC %s+%s+%s  %s%s+%s%s.  ",
                             show3(pc.find_match,temp1), show3(pc.find_match_memaccess,temp2), show3(pc.check_hasharr,temp3),
                             show3(pc.record_match,temp4), x?show3(x,temp5,"+"):"", show3(pc.hash_found-pc.check_len,temp6),
                             pc.max_offset<Offset(-1)? show3(pc.total_match_len,temp7,"  "):"");

            fprintf (stderr, "\r%d%%: %s -> %s: %.2lf%%.  %sCpu %.0lf mb/s (%.3lf sec), real %.0lf mb/s (%.3lf sec) = %.0lf%%%s",
                             int(double(origsize)*100/filesize),  show3(origsize,temp1),  show3(compsize,temp2),  double(compsize)*100/origsize, print_pc? counters:"",
                             origsize/CPUTime/mb, CPUTime, origsize/GlobalTime/mb, GlobalTime, CPUTime/GlobalTime*100, newline);
            Taskbar_SetProgressValue (origsize, filesize);
          }
        }
        if (len==0) break;
      }

      if (bg_thread.errcode)  {errcode = bg_thread.errcode; goto cleanup;}
    }


    //*********************************************************************************************************
    /// SECOND PASS FOR FUTURE_LZ/INDEX_LZ MODES
    //*********************************************************************************************************

    if (FUTURE_LZ || INDEX_LZ)
    {
      *last_block = NULL;   // Mark end of linked list
      if (verbosity)
        fprintf (stderr, "\nSorting matches...");

      char *buf = new char[bufsize+1];
      char *outbuf  = new char[bufsize], *out;
      STAT *statbuf = new STAT[MAX_STATS_PER_BLOCK(bufsize,BASE_LEN)], *stat;  // for every BASE_LEN input bytes, we can write up to 4 STAT values to statbuf

      // 1. Fill lz_matches[] with matches from all compressed blocks
      LZ_MATCH *lz_matches = new LZ_MATCH[lz_matches_count+1];          // +1 is for the loop barrier
      int i = 0;                                                        // lz_matches[] index

      for (COMPRESSED_BLOCK *block = first_block;  block;  block = block->next)
      {
        // Copy to lz_matches[] all matches from the block
        Offset block_pos = block->start;                                // current position in the input file
        for (STAT *stat = block->statbuf; stat < block->statend; )
        {
          // Copy one LZ match record from stat[] to lz_matches[]
          DECODE_LZ_MATCH(stat, false, ROUND_MATCHES, BASE_LEN, block_pos,  lit_len, LZ_MATCH, lz_match);
          lz_matches[i++]  = lz_match;
          block_pos       += lit_len + lz_match.len;
        }
      }

      // Duplicate match list for computation of the decompression RAM
      LZ_MATCH *lz_matches_by_dest = new LZ_MATCH[lz_matches_count+1];          // +1 is for the loop barrier
      my_memcpy (lz_matches_by_dest, lz_matches, lz_matches_count*sizeof(LZ_MATCH));
      lz_matches_by_dest[lz_matches_count].src = origsize;   // Loop barrier


      // 2. Sort array by absolute position of LZ match source
      std::sort (lz_matches, lz_matches+lz_matches_count, order_by_LZ_match_src);
      lz_matches[lz_matches_count].src = origsize;   // Loop barrier

      // Calculate how much RAM will be required for decompression
      Offset ram=0, max_ram=0;
      for (int i=0,j=0;  i<lz_matches_count || j<lz_matches_count;  )
      {
        if (lz_matches[i].src < lz_matches_by_dest[j].dest) {
          if (lz_matches[i].src/bufsize != lz_matches[i].dest/bufsize  &&  lz_matches[i].len < maximum_save)
            ram += MEMORY_MANAGER::needmem(lz_matches[i].len),
            max_ram = mymax(ram,max_ram);
          i++;
        } else {
          if (lz_matches_by_dest[j].src/bufsize != lz_matches_by_dest[j].dest/bufsize  &&  lz_matches_by_dest[j].len < maximum_save)
            ram -= MEMORY_MANAGER::needmem(lz_matches_by_dest[j].len);
          j++;
        }
      }
      if (verbosity)
      {
        char temp1[100], maximum_save_str[100];  showMem (maximum_save, maximum_save_str);
        sprintf(temp1, (maximum_save != unsigned(-1)? " with -m%s":""), maximum_save_str);
        printf("\rDecompression memory%s is %d mb", temp1, int(max_ram/mb)+1);
      }


      // 3. Process input file once again, joining future-LZ matches info with literal data
      file_seek (ftemp, 0);  Offset processed = 0, total_stat_size = 0;  i = 0;
      STAT *statsize_buf = new STAT[total_blocks],  *statsize_ptr = statsize_buf;
      if (FUTURE_LZ && verbosity)
        fprintf (stderr, ".  Second pass:  0.0%%");

      for (COMPRESSED_BLOCK *block = first_block; block; block = block->next )
      {
        // Fill stat[] with future-LZ matches whose LZ.src lies in the current block
        stat = statbuf;  Offset block_pos = block->start;
        int saved_i = i;                                                // First match that should be checked in the next block
        for (;  lz_matches[i].src < block->end;  i++)
        {
          if (lz_matches[i].src+lz_matches[i].len <= block->start)      // Skip any matches entirely owned by previous blocks, updating saved_i too
            {saved_i = i; continue;}
          Offset src = mymax (lz_matches[i].src, block->start);         // Truncate match if it starts before block->start ...
          STAT   len = lz_matches[i].len - (src-lz_matches[i].src);
                 len = mymin (len, block->end - src);                   // ... or ends after block->end
          ENCODE_LZ_MATCH(stat, false, BASE_LEN,  src - block_pos, lz_matches[i].dest - lz_matches[i].src, len);
          block_pos = src;
        }
        i = saved_i;

        // Write compressed block header to compressed file
        unsigned stat_size  =  (char*)stat - (char*)statbuf;
        if (FUTURE_LZ)
        {
          block->header[2] = stat_size;
          checked_file_write (fout, block->header, header_size);
        }

        // Write match list for compressed block to compressed file
        checked_file_write (fstat, statbuf, stat_size);
        *statsize_ptr++ = stat_size;
        total_stat_size += stat_size;

        if (FUTURE_LZ)
        {
          // Read next input block
          checked_file_read (ftemp, buf, block->size);

          // Copy literal data, not covered by LZ matches, to outbuf[]
          char *in = buf,  *out = outbuf;
          for (STAT *stat = block->statbuf; stat < block->statend; )
          {
            DECODE_LZ_MATCH(stat, false, ROUND_MATCHES, BASE_LEN, 0,  lit_len, LZ_MATCH, lz_match);
            my_memcpy(out, in, lit_len);
            out += lit_len;
            in  += lit_len + lz_match.len;
          }
          my_memcpy(out, in, block->size - (in-buf));
          out += block->size - (in-buf);

          // Write compressed block literals to compressed file
          checked_file_write (fout, outbuf, out-outbuf);

          if (verbosity)
          {
            processed += block->size;
            double GlobalTime = GetGlobalTime()-GlobalTime0;
            if (processed==origsize || GlobalTime-LastGlobalTime>TimeInterval)
            {
              LastGlobalTime = GlobalTime;
              fprintf (stderr, "\b\b\b\b\b\b%5.1lf%%", double(processed)*100/origsize);
              Taskbar_SetProgressValue (processed, origsize);
            }
          }
        }
      }


      // 4. Write compressed file footer
      if (INDEX_LZ)
      {
        unsigned statsize_size  =  (BYTE*)statsize_ptr - (BYTE*)statsize_buf;
        header[0] = total_stat_size;
        header[1] = total_stat_size>>32;
        header[2] = INDEX_LZ_FOOTER_SIZE+statsize_size;   // footer size
        header[3] = SREP_FOOTER_VERSION1;                 // footer version
        header[4] = ~SREP_SIGNATURE;                      // footer signature
        header[5] = ~BULAT_ZIGANSHIN_SIGNATURE;
        checked_file_write (fout, statsize_buf, statsize_size);
        checked_file_write (fout, header, INDEX_LZ_FOOTER_SIZE);
      }
    }
  }
  else
  {
    //*********************************************************************************************************
    /// DECOMPRESSION
    //*********************************************************************************************************

    unsigned compbufsize = bufsize + sizeof(STAT)*(MAX_HEADER_SIZE+MAX_HASH_SIZE+MAX_STATS_PER_BLOCK(bufsize,0));
    char *buf     = new char[compbufsize];
    char *out     = new char[bufsize];
    STAT *statbuf = (STAT*)buf,  *statptr = (STAT*)buf,  *statend = NULL;
    STAT *statsize_buf = NULL,  *statsize_ptr = NULL,  *statsize_end = NULL;

    int io_mem = vm_block+bufsize+compbufsize+8*mb;
    MEMORY_MANAGER mm (vm_mem>=io_mem+vm_block*4? vm_mem-io_mem : vm_block*4);
    VIRTUAL_MEMORY_MANAGER vm(vmfile_name, vm_block);
    LZ_MATCH_HEAP lz_matches;
    FUTURE_LZ_MATCH barrier;  barrier.dest = Offset(-1);  lz_matches.insert(barrier);

    // Check header of compressed file
    int len = file_read (fin, header, sizeof(STAT)*ARCHIVE_HEADER_SIZE);
    if (len != sizeof(STAT)*ARCHIVE_HEADER_SIZE
     || header[0] != BULAT_ZIGANSHIN_SIGNATURE
     || header[1] != SREP_SIGNATURE)            error (ERROR_COMPRESSION, "Not an SREP compressed file: %s", finame);
    int format_version  =  header[2] & 255;
    if (format_version < SREP_FORMAT_VERSION1
     || format_version > SREP_FORMAT_VERSION4)  error (ERROR_COMPRESSION, "Incompatible compressed data format: v%d (%s supports only v%d..v%d) in file %s", format_version, program_version, SREP_FORMAT_VERSION1, SREP_FORMAT_VERSION4, finame);

    // Get compression params from the header
    unsigned BASE_LEN = header[3];
    int hash_num       = (header[2] >>  8) & 255;
    int hash_seed_size = (header[2] >> 16) & 255;
    int hash_size      =((header[2] >> 24) + 16) & 255;
    if (selected_hash->hash_func != NULL)      // unless hash checking was disabled by -hash- option
    {
      selected_hash = hash_by_num(hash_num);
      if (selected_hash == NULL) {
        fprintf (stderr, "Block checksums can't be checked since they are using unknown hash #%d-%d\n", hash_num, hash_size*CHAR_BIT);
        selected_hash = hash_by_name("", errcode);
      } else if (selected_hash->hash_func == NULL) {
        fprintf (stderr, "Block checksums can't be checked since they aren't saved in the compressed data\n");
      } else if (hash_seed_size > selected_hash->hash_seed_size  ||  hash_size > selected_hash->hash_size) {
        char temp[100];
        fprintf (stderr, "Block checksums can't be checked since they are using unsupported hashsize %s-%s%d", selected_hash->hash_name, hash_seed_size?show3(hash_seed_size*CHAR_BIT,temp,"-"):"", hash_size*CHAR_BIT);
        selected_hash = hash_by_name("", errcode);
      }
    }

    // For keyed hashes like VMAC, we should read the key (seed) and create a hash using this key
    len = file_read (fin, header, hash_seed_size);
    if (len!=hash_seed_size)   error (ERROR_COMPRESSION, "Decompression problem: unexpected end of file %s or I/O error", finame);
    if (selected_hash->new_hash)
    {
      hash_obj = selected_hash->new_hash (header, hash_seed_size);
    }
    unsigned full_archive_header_size = sizeof(STAT)*ARCHIVE_HEADER_SIZE + hash_seed_size;
    compsize = full_archive_header_size;

    const int  header_size    =  sizeof(STAT)*BLOCK_HEADER_SIZE + hash_size;  // compressed block header size
    const bool ROUND_MATCHES  =  (format_version == SREP_FORMAT_VERSION1);
    const bool IO_LZ          =  (format_version <= SREP_FORMAT_VERSION2);
    const bool FUTURE_LZ      =  (format_version == SREP_FORMAT_VERSION3);
    const bool INDEX_LZ       =  (format_version == SREP_FORMAT_VERSION4);
    if (cmdmode==INFORMATION)
      fprintf (stderr, "%s: -l%d -hash=%s%s",  FUTURE_LZ? "Future-LZ":INDEX_LZ? "Index-LZ":"I/O LZ",  BASE_LEN,  selected_hash->hash_name,  INDEX_LZ? "":"\n");

    // INDEX_LZ: Read match list from footer of the compressed file
    if (INDEX_LZ)
    {
      file_seek (fin, filesize-INDEX_LZ_FOOTER_SIZE);
      checked_file_read (fin, header, INDEX_LZ_FOOTER_SIZE);

      unsigned footer_version = (header[3] & 255);
      unsigned footer_size = header[2];
      Offset stat_size = header[0] + (Offset(header[1])<<32);
      compsize += footer_size+stat_size;

      if (header[5] != ~BULAT_ZIGANSHIN_SIGNATURE
       || header[4] != ~SREP_SIGNATURE)            error (ERROR_COMPRESSION, "Not found SREP compressed file footer in file %s", finame);
      if (footer_version != SREP_FOOTER_VERSION1)  error (ERROR_COMPRESSION, "Incompatible compressed file footer format: v%d (%s supports only v%d) in file %s", footer_version, program_version, SREP_FOOTER_VERSION1, finame);
      if (compsize > filesize)                     error (ERROR_COMPRESSION, "Broken SREP compressed file footer: %0.lf bytes footer + %0.lf bytes index in file %s", double(footer_size), double(stat_size), finame);

      // Read match list
      statbuf = statptr = new STAT[stat_size/sizeof(STAT)];
      statend = statbuf+stat_size/sizeof(STAT);
      file_seek (fin, filesize-footer_size-stat_size);
      checked_file_read (fin, statbuf, stat_size);

      // Read block list (count of matches for every block)
      unsigned total_blocks  =  (footer_size-INDEX_LZ_FOOTER_SIZE)/sizeof(STAT);
      statsize_buf = statsize_ptr = new STAT[total_blocks];
      statsize_end = statsize_buf+total_blocks;
      checked_file_read (fin, statsize_buf, total_blocks*sizeof(STAT));

      file_seek (fin, full_archive_header_size);

      if (cmdmode==INFORMATION)
      {
        // Original file size = literal bytes + match bytes
        origsize = filesize-footer_size-stat_size-full_archive_header_size-total_blocks*header_size;   // compute literal bytes
        for (STAT *stat = statbuf; stat < statbuf+stat_size/sizeof(STAT); )
        {
          DECODE_LZ_MATCH(stat, false, ROUND_MATCHES, BASE_LEN, 0,  lit_len, LZ_MATCH, lz_match);
          origsize += lz_match.len;                                                                    // add match bytes
        }

        char temp1[100], temp2[100];
        fprintf (stderr, ".  %s -> %s: %.2lf%%\n", show3(filesize,temp1), show3(origsize,temp2), double(filesize)*100/origsize);
        goto cleanup;
      }
    }

    // If we will need to reread data from the stdout, it will be wise to duplicate them to tempfile
    if ((IO_LZ || maximum_save!=unsigned(-1))  &&  strequ(foutname,"-"))
    {
      if (!tempfile)
        tempfile = DEFAULT_TEMPFILE;
      else if (*tempfile==0)
        error (ERROR_IO, "Writing decompressed data to stdout without tempfile isn't supported for this file and settings");
    }

    ftemp  =  (tempfile && *tempfile)? fopen (tempfile, "w+b") : fout;
    if (ftemp == NULL)  error (ERROR_IO, "Can't open tempfile %s for write", tempfile);

    // Decompress data by blocks until EOF
    for (bool finished=false; !finished; )
    {
      // Read block header
      int len = file_read (fin, header, header_size);
      // If there is no more data or EOF header (two zero 32-bit words) detected
      if ((len==0  ||  len>=2*sizeof(STAT) && header[0]==0 && header[1]==0)  &&  lz_matches.size()==1)   {finished=true; goto print_decompression_stats;}
      if (len!=header_size)   error (ERROR_COMPRESSION, "Decompression problem: unexpected end of file %s or I/O error", finame);

     {unsigned compsize1 = header[0]+header[2],
               origsize1 = header[1],
               statsize1 = header[2];
      if (origsize1>bufsize)                                                         error (ERROR_COMPRESSION, "Decompression problem: uncompressed block size is %u bytes, while maximum supported size is %u bytes", origsize1, bufsize);
      if (compsize1>compbufsize || header[0]>compbufsize || header[2]>compbufsize)   error (ERROR_COMPRESSION, "Decompression problem: compressed block size is %u bytes, while maximum supported size is %u bytes",   compsize1, compbufsize);

      if (cmdmode==INFORMATION)
      {
        // We scan through the compressed file only in order to compute the uncompressed file size
        file_seek_cur (fstat, statsize1);
        file_seek_cur (fin, compsize1-statsize1);
        compsize += header_size + compsize1;
        origsize += origsize1;
        goto print_decompression_stats;
      }

      // Read compressed data
      len = file_read (fstat, buf, statsize1);
      if (len!=statsize1)   error (ERROR_COMPRESSION, "Decompression problem: unexpected end of file %s or I/O error", *index_file? index_file : finame);
      len = file_read (fin, buf+statsize1, compsize1-statsize1);
      if (len!=compsize1-statsize1)   error (ERROR_COMPRESSION, "Decompression problem: unexpected end of file %s or I/O error", finame);

      STAT *statendptr = (STAT*)(buf+statsize1);                // Should point AFTER the last match belonging to the block
      if (INDEX_LZ) {
        statendptr = statptr + (*statsize_ptr++)/sizeof(STAT);
        finished = (statsize_ptr==statsize_end);
      }

      // Perform decompression
      bool ok = IO_LZ? decompress           (ROUND_MATCHES, BASE_LEN, ftemp, origsize, statptr,             buf+statsize1, buf+compsize1, out, out+origsize1)
                     : decompress_FUTURE_LZ (ROUND_MATCHES, BASE_LEN, ftemp, origsize, statptr, statendptr, buf+statsize1, buf+compsize1, out, out+origsize1, mm, vm, lz_matches, maximum_save);
      if (!ok)   error (ERROR_COMPRESSION, "Decompression problem: broken compressed data");

      if (INDEX_LZ)
        statptr = statendptr;

      // Check hashsum of decompressed data
      if (selected_hash->hash_func) {
        char checksum [MAX_HASH_SIZE];
        selected_hash->hash_func (hash_obj, out,origsize1, checksum);
        if (memcmp (checksum, header+3, hash_size) != EQUAL)   error (ERROR_COMPRESSION, "Decompression problem: checksum of decompressed data is not the same as checksum of original data");
      }

      // Write decompressed data to output file, plus to temporary file if it's different
      file_seek (ftemp, origsize);
      checked_file_write (ftemp, out, origsize1);
      if (tempfile && *tempfile)
        checked_file_write (fout, out, origsize1);

      // Update statistics
      compsize += header_size + compsize1;
      origsize += origsize1;}

print_decompression_stats:
      if (verbosity)
      {
        double GlobalTime = GetGlobalTime()-GlobalTime0;
        if (origsize!=last_origsize  &&  (finished || GlobalTime-LastGlobalTime>TimeInterval))
        {
          LastGlobalTime = GlobalTime;  last_origsize = origsize;  double CPUTime = GetCPUTime();
          char stats[1000], total_reads_str[100], vm_stats_str[100], temp1[100], temp2[100];
          sprintf (total_reads_str, maximum_save != unsigned(-1)? ", I/Os %.0lf":"", double(total_reads));
          sprintf (vm_stats_str, vm.total_write? ", VM %d/%d, R/W %d/%d":"", int(vm.current_mem()/mb), int(vm.max_mem()/mb), int(vm.total_read/mb), int(vm.total_write/mb));
          sprintf (stats, (IO_LZ || !print_pc? "" : ".  Matches %.0lf %.0lf %.0lf%s, RAM %d/%d%s"),
                   double(cur_matches), double(max_matches), double(total_matches), total_reads_str,
                   int(mm.current_mem()/mb), int(mm.max_mem()/mb), vm_stats_str);
          fprintf (stderr, "\r%d%%: %s -> %s: %.2lf%%.  Cpu %.0lf mb/s (%.3lf sec), real %.0lf mb/s (%.3lf sec) = %.0lf%%%s%s",
                   int(double(compsize)*100/filesize), show3(compsize,temp1), show3(origsize,temp2), double(compsize)*100/origsize,
                   origsize/CPUTime/mb, CPUTime, origsize/GlobalTime/mb, GlobalTime, CPUTime/GlobalTime*100, stats, newline);
          Taskbar_SetProgressValue (compsize, filesize);
        }
      }
    }
  }


  //*********************************************************************************************************
  /// CLOSE FILES
  //*********************************************************************************************************

cleanup:
  fprintf (stderr, "\n");
  fclose(fin);
  fclose(fout);
  if (fstat!=fin && fstat!=fout)
    fclose(fstat);
  if (ftemp!=fin && ftemp!=fout)
    fclose(ftemp);
  tempfile && remove(tempfile);
  if (errcode)
  {
    // Delete output files on error
    if (!strequ(foutname,"-"))
      remove(foutname);
    if (index_file)
      remove(index_file);
    return errcode;
  }
  if (warnings==0 && delete_input_files && !strequ(finame,"-"))
    remove(finame);
  return warnings? WARNINGS : NO_ERRORS;
}
