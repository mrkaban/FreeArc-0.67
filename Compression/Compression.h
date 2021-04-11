#ifndef FREEARC_COMPRESSION_H
#define FREEARC_COMPRESSION_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <time.h>

#include "Common.h"


#ifdef __cplusplus
extern "C" {
#endif

// Signature of files made by my utilities
#define BULAT_ZIGANSHIN_SIGNATURE 0x26351817

// ��������� ��� ������� ������ ������� ������
#define b_ (1u)
#define kb (1024*b_)
#define mb (1024*kb)
#define gb (1024*mb)
#define terabyte (1024*uint64(gb))

// ���������� ����, ������� ������ ��������/������������ �� ���� ��� �� ���� �����������
#define BUFFER_SIZE (256*kb)

// ���������� ����, ������� ������ ��������/������������ �� ���� ��� � ������� ������� � ��� ���������� ������������� ����������
#define LARGE_BUFFER_SIZE (256*kb)

// ���������� ����, ������� ������ ��������/������������ �� ���� ��� � ����� ������� ������� (storing, tornado � ���� ��������)
// ���� ����� ������������ ������ �� disk seek operations - ��� �������, ��� ������������ �� ���������� �/� � ������ ������ ;)
#define HUGE_BUFFER_SIZE (8*mb)

// C ����� �������� ���� �������� � ��������� � ��������/����������
#define PROGRESS_CHUNK_SIZE (64*kb)

// �������������� ����������� ��� �������� �������� �������� ����� ������� ������
#define COMPRESSION_METHODS_DELIMITER            '+'   /* ����������� ������� ������ � ��������� �������� ����������� */
#define COMPRESSION_METHOD_PARAMETERS_DELIMITER  ':'   /* ����������� ���������� � ��������� �������� ������ ������ */
#define MAX_COMPRESSION_METHODS    1000        /* ������ ���� �� ������ ����� ������� ������, �������������� � ������� AddCompressionMethod */
#define MAX_PARAMETERS             200         /* ������ ���� �� ������ ������������� ���-�� ���������� (���������� �����������), ������� ����� ����� ����� ������ */
#define MAX_COMPRESSOR_STRLEN      2048        /* ������������ ����� ������, ����������� ���������� */
#define MAX_METHOD_STRLEN          512         /* ������������ ����� ������, ����������� ����� ������ */
#define MAX_METHODS_IN_COMPRESSOR  100         /* ������������ ����� ������� � ����� ����������� */
#define MAX_EXTERNAL_COMPRESSOR_SECTION_LENGTH 2048  /* ������������ ����� ������ [External compressor] */


// ****************************************************************************************************************************
// ������� ������/������ ������ � ������� ������ ******************************************************************************
// ****************************************************************************************************************************

// ��� ������� ��� �������� �������
typedef int CALLBACK_FUNC (const char *what, void *data, int size, void *auxdata);

// ������� ��� ������/������ �(�)������ ������� � ���������, ��� �������� ����� ������� ������, ������� ���� ���������
#define checked_read(ptr,size)         {if ((x = callback("read" ,ptr,size,auxdata)) != size) {x>=0 && (x=FREEARC_ERRCODE_READ);  goto finished;}}
#define checked_write(ptr,size)        {if ((x = callback("write",ptr,size,auxdata)) != size) {x>=0 && (x=FREEARC_ERRCODE_WRITE); goto finished;}}
// ������ ��� ������ ������� ������� � ��������� �� ������ � ����� ������� ������
#define checked_eof_read(ptr,size)     {if ((x = callback("read", ptr,size,auxdata)) != size) {x>0  && (x=FREEARC_ERRCODE_READ);  goto finished;}}

// Auxiliary code to read/write data blocks and 4-byte headers
#define INIT() callback ("init", NULL, 0, auxdata)
#define DONE() callback ("done", NULL, 0, auxdata)

#define MALLOC(type, ptr, size)                                            \
{                                                                          \
    (ptr) = (type*) malloc ((size) * sizeof(type));                        \
    if ((ptr) == NULL) {                                                   \
        errcode = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;                       \
        goto finished;                                                     \
    }                                                                      \
}

#define BIGALLOC(type, ptr, size)                                          \
{                                                                          \
    (ptr) = (type*) BigAlloc ((size) * sizeof(type));                      \
    if ((ptr) == NULL) {                                                   \
        errcode = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;                       \
        goto finished;                                                     \
    }                                                                      \
}

#define READ(buf, size)                                                    \
{                                                                          \
    void *localBuf = (buf);                                                \
    int localSize  = (size);                                               \
    int localErrCode;                                                                                \
    if (localSize  &&  (localErrCode=callback("read",localBuf,localSize,auxdata)) != localSize) {    \
        errcode = localErrCode<0? localErrCode : FREEARC_ERRCODE_READ;                               \
        goto finished;                                                     \
    }                                                                      \
}

#define READ_LEN(len, buf, size)                                           \
{                                                                          \
    int localErrCode;                                                      \
    if ((localErrCode=(len)=callback("read",buf,size,auxdata)) < 0) {      \
        errcode = localErrCode;                                            \
        goto finished;                                                     \
    }                                                                      \
}

#define READ_LEN_OR_EOF(len, buf, size)                                    \
{                                                                          \
    int localErrCode;                                                      \
    if ((localErrCode=(len)=callback("read",buf,size,auxdata)) <= 0) {     \
        errcode = localErrCode;                                            \
        goto finished;                                                     \
    }                                                                      \
}

#define WRITE(buf, size)                                                   \
{                                                                          \
    void *localBuf = (buf);                                                \
    int localSize  = (size);                                               \
    int localErrCode;                                                                   \
    /* "write" callback on success guarantees to write all the data and may return 0 */ \
    if (localSize && (localErrCode=callback("write",localBuf,localSize,auxdata))<0) {   \
        errcode = localErrCode;                                                         \
        goto finished;                                                     \
    }                                                                      \
}

#define READ4(var)                                                         \
{                                                                          \
    unsigned char localHeader[4];                                          \
    READ (localHeader, 4);                                                 \
    (var) = value32 (localHeader);                                         \
}

#define READ4_OR_EOF(var)                                                  \
{                                                                          \
    int localHeaderSize;                                                   \
    unsigned char localHeader[4];                                          \
    READ_LEN_OR_EOF (localHeaderSize, localHeader, 4);                     \
    if (localHeaderSize!=4)  {errcode=FREEARC_ERRCODE_READ; goto finished;}\
    (var) = value32 (localHeader);                                         \
}

#define WRITE4(value)                                                      \
{                                                                          \
    unsigned char localHeader[4];                                          \
    setvalue32 (localHeader, value);                                       \
    WRITE (localHeader, 4);                                                \
}

#define QUASIWRITE(size)                                                   \
{                                                                          \
    int64 localSize = (size);                                              \
    callback ("quasiwrite", &localSize, (size), auxdata);                  \
}

#define PROGRESS(insize,outsize)                                           \
{                                                                          \
    int64 localSize[2] = {(insize),(outsize)};                             \
    callback ("progress", localSize, 0, auxdata);                          \
}

#define ReturnErrorCode(err)                                               \
{                                                                          \
    errcode = (err);                                                       \
    goto finished;                                                         \
}                                                                          \


// Buffered data output
#ifndef FREEARC_STANDALONE_TORNADO
#define FOPEN()   Buffer fbuffer(BUFFER_SIZE)
#define FWRITE(buf, size)                                                  \
{                                                                          \
    void *flocalBuf = (buf);                                               \
    int flocalSize = (size);                                               \
    int rem = fbuffer.remainingSpace();                                    \
    if (flocalSize>=4096) {                                                \
        FFLUSH();                                                          \
        WRITE(flocalBuf, flocalSize);                                      \
    } else if (flocalSize < rem) {                                         \
        fbuffer.put (flocalBuf, flocalSize);                               \
    } else {                                                               \
        fbuffer.put (flocalBuf, rem);                                      \
        FFLUSH();                                                          \
        fbuffer.put ((byte*)flocalBuf+rem, flocalSize-rem);                \
    }                                                                      \
}
#define FWRITESZ(value)                                                    \
{                                                                          \
    const char *flocalValue = (value);                                     \
    int flocalBytes = strlen(flocalValue) + 1;                             \
    FWRITE ((void*)flocalValue, flocalBytes);                              \
}
#define FWRITE4(value)                                                     \
{                                                                          \
    unsigned char flocalHeader[4];                                         \
    setvalue32 (flocalHeader, value);                                      \
    FWRITE (flocalHeader, 4);                                              \
}
#define FWRITE1(value)                                                     \
{                                                                          \
    unsigned char flocalHeader = (value);                                  \
    FWRITE (&flocalHeader, 1);                                             \
}
#define FFLUSH()  { WRITE (fbuffer.buf, fbuffer.len());  fbuffer.empty(); }
#define FCLOSE()  { FFLUSH(); }


// �����, ������������ ��� ����������� ���������� ����������� ������� ������
// � ���������. ����� ����� ���������� � ���� 8/16/32-��������� ����� � �����������
// ��� �������������. ������� ���������� ������ ������������ � �������� �����.
// ������������� ����� ������������ ������ ����� ���������� � ���� ������.
// ����� ���������� ����� ������ - ��� max(p,end), ��� p - ������� ���������,
// � end - ������������ ������� ����� ���������� ������.
struct Buffer
{
    byte*  buf;              // ����� ����������� ������
    byte*  p;                // ������� ��������� ������/������ ������ ����� ������
    byte*  end;              // ����� ����� ����� �����������/���������� ������
    byte*  bufend;           // ����� ������ ������

    Buffer (uint size=64*kb) { buf=p=end= (byte*) malloc(size);  bufend=buf+size; }
    ~Buffer()                { freebuf(); }
    void   freebuf()         { free(buf);  buf=p=end=NULL; }
    void   empty()           { p=end=buf; }
    int    len()             { return mymax(p,end)-buf; }

    void   put8 (uint x)     { reserve(sizeof(uint8 ));  *(uint8 *)p=x;    p+=sizeof(uint8 ); }
    void   put16(uint x)     { reserve(sizeof(uint16));  setvalue16(p,x);  p+=sizeof(uint16); }
    void   put32(uint x)     { reserve(sizeof(uint32));  setvalue32(p,x);  p+=sizeof(uint32); }

    void   put(void *b, int n)  { reserve(n);  memcpy(p,b,n);  p+=n; }
    void   puts (char *s)    { put (s, strlen(s)); }
    void   putsz(char *s)    { put (s, strlen(s)+1); }

    int    remainingSpace()  { return bufend-p; }
    void   reserve(uint n)   {
                               if (remainingSpace() < n)
                               {
                                 uint newsize = mymax(p+n-buf, (bufend-buf)*2);
                                 byte* newbuf = (byte*) realloc (buf, newsize);
                                 bufend = newbuf + newsize;
                                 p   += newbuf-buf;
                                 end += newbuf-buf;
                                 buf  = newbuf;
                               }
                             }

    void reverseBytes()      {
                               byte *lo = buf,  *hi = buf + len() - 1,  swap;
                               while (lo < hi)  { swap = *lo;  *lo++ = *hi;  *hi-- = swap; }
                             }
// ��� ������ ������
    void   rewind()          { end=mymax(p,end);  p=buf; }
    uint   get8 ()           { uint x = *(uint8 *)p;  p+=sizeof(uint8 );  return x; }
    uint   get16()           { uint x = value16(p);   p+=sizeof(uint16);  return x; }
    uint   get32()           { uint x = value32(p);   p+=sizeof(uint32);  return x; }
    int    get(void *b, int n)  { n = mymin(remainingData(), n);  memcpy(b,p,n);  p+=n;  return n;}
    int    remainingData()   { return p<end? end-p : 0; }
    bool   eof()             { return remainingData()==0; }
};

#endif // !FREEARC_STANDALONE_TORNADO


// ****************************************************************************************************************************
// ���������� CRC-32                                                                                                          *
// ****************************************************************************************************************************

#define INIT_CRC 0xffffffff

uint32 UpdateCRC (const void *Addr, size_t Size, uint32 StartCRC);     // �������� CRC ���������� ����� ������
uint32 CalcCRC   (const void *Addr, size_t Size);                      // ��������� CRC ����� ������


// ****************************************************************************************************************************
// ������� ********************************************************************************************************************
// ****************************************************************************************************************************

// �������� ��������� ������/����������
typedef char *CPARAM;

// �������� ������/����������, �������������� � ���� ������
typedef char *CMETHOD;

// ������������������ ���������� ������/����������, �������������� � ���� "exe+rep+lzma+aes"
typedef char *COMPRESSOR;

// ��������� ������ what ������ ������ method
int CompressionService (char *method, char *what, DEFAULT(int param,0), DEFAULT(void *data,NULL), DEFAULT(CALLBACK_FUNC *callback,NULL));

// ���������, ��� ������ ���������� �������� �������� ����������
int compressorIsEncrypted (COMPRESSOR c);
// ���������, ������� ������ ����� ��� ���������� ������, ������ ���� ������������
MemSize compressorGetDecompressionMem (COMPRESSOR c);

// Get/set number of threads used for (de)compression
int  __cdecl GetCompressionThreads (void);
void __cdecl SetCompressionThreads (int threads);

// Used in 4x4 only: read entire input buffer before compression begins, allocate output buffer large enough to hold entire compressed output
extern int compress_all_at_once;
void __cdecl Set_compress_all_at_once (int n);
struct Set_compress_all_at_once_Until_end_of_block
{
  int save;
  Set_compress_all_at_once_Until_end_of_block (int n)  {save = compress_all_at_once;  Set_compress_all_at_once(n);}
  ~Set_compress_all_at_once_Until_end_of_block()       {Set_compress_all_at_once(save);}
};

// Enable debugging output
extern int debug_mode;
void __cdecl Set_debug_mode (int n);

// Load accelerated function either from facompress.dll or facompress_mt.dll
FARPROC LoadFromDLL (char *funcname, DEFAULT(int only_facompress_mt, FALSE));

// Other compression methods may chain-redefine this callback in order to perform their own cleanup procedures
extern void (*BeforeUnloadDLL)();

// This function unloads DLLs containing accelerated compression functions
void UnloadDLL (void);

#ifdef FREEARC_WIN
extern HINSTANCE hinstUnarcDll;   // unarc.dll instance
#endif

// This function should cleanup Compression Library
void compressionLib_cleanup (void);


// ****************************************************************************************************************************
// ������� ������ � ���������� ������ *****************************************************************************************
// ****************************************************************************************************************************

enum COMPRESSION {COMPRESS, DECOMPRESS};  // Direction of operation

// ����������� ������, ����������� �������� ������� ��� �������� �������
int Decompress (char *method, CALLBACK_FUNC *callback, void *auxdata);
// ��������� �� �������� ������ ����������� ������ ������ � ����������� ������ ���� �������
int DecompressWithHeader (CALLBACK_FUNC *callback, void *auxdata);
// ����������� ������ � ������, ������� � �������� ����� �� ����� outputSize ����.
// ���������� ��� ������ ��� ���������� ����, ���������� � �������� �����
int DecompressMem (char *method, void *input, int inputSize, void *output, int outputSize);
int DecompressMemWithHeader     (void *input, int inputSize, void *output, int outputSize);

#ifndef FREEARC_DECOMPRESS_ONLY
// ��������� ������ �������� ������� ��� �������� �������
int Compress   (char *method, CALLBACK_FUNC *callback, void *auxdata);
// �������� � �������� ����� ����������� ������ ������ � ��������� ������ ���� �������
int CompressWithHeader (char *method, CALLBACK_FUNC *callback, void *auxdata);
// ��������� ������ � ������, ������� � �������� ����� �� ����� outputSize ����.
// ���������� ��� ������ ��� ���������� ����, ���������� � �������� �����
int CompressMem           (char *method, void *input, int inputSize, void *output, int outputSize);
int CompressMemWithHeader (char *method, void *input, int inputSize, void *output, int outputSize);
// ���������� � ������, ����������� ��� ��������/����������, ������� ������� � ������� �����.
MemSize GetCompressionMem      (char *method);
MemSize GetMinCompressionMem   (char *method);
MemSize GetMinDecompressionMem (char *method);
// ���������� � out_method ����� ����� ������, ����������� �� �������������
// ���������������� ���������� ������/�������/������� �����
int SetCompressionMem          (char *in_method, MemSize mem,  char *out_method);
int SetMinDecompressionMem     (char *in_method, MemSize mem,  char *out_method);
int SetDictionary              (char *in_method, MemSize dict, char *out_method);
int SetBlockSize               (char *in_method, MemSize bs,   char *out_method);
// ���������� � out_method ����� ����� ������, ��������, ���� ����������,
// ������������ ���������� ������ / ��� ������� / ������ �����
int LimitCompressionMem        (char *in_method, MemSize mem,  char *out_method);
int LimitMinDecompressionMem   (char *in_method, MemSize mem,  char *out_method);
int LimitDictionary            (char *in_method, MemSize dict, char *out_method);
int LimitBlockSize             (char *in_method, MemSize bs,   char *out_method);
#endif
MemSize GetDictionary          (char *method);
MemSize GetBlockSize           (char *method);
MemSize GetDecompressionMem    (char *method);
int     SetDecompressionMem    (char *in_method, MemSize mem,  char *out_method);
int     LimitDecompressionMem  (char *in_method, MemSize mem,  char *out_method);

// ������� � out_method ������������ ������������� ������ ������ in_method (��������� ParseCompressionMethod + ShowCompressionMethod)
//   purify!=0: ����������� ������������� method � ������ � ����� (��������, ������ :t:i ��� 4x4)
int CanonizeCompressionMethod (char *in_method, char *out_method, int purify);

// ������� "(���)�������", ���������� ������ ���� � ����
int copy_data   (CALLBACK_FUNC *callback, void *auxdata);


// ****************************************************************************************************************************
// �����, ����������� ��������� � ������ ������ *******************************************************************************
// ****************************************************************************************************************************

#ifdef __cplusplus

// ����������� ��������� � ������������� ������ ������
class COMPRESSION_METHOD
{
public:
  // ������� ���������� � ��������
  //   DeCompressMem can either compress or decompress, either from memory block `input` to `output` or calling `callback` for I/O.
  //   CodecState, unless NULL, points to the place for storing pointer to persistent codec state, such as allocated buffers
  //     and precomputed tables, that should be finally freed by the empty DeCompressMem() call.
  virtual int DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback=0, void *auxdata=0, void **CodecState=0);
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata) = 0;
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata) = 0;

  // ���������� � ������, ����������� ��� ��������/���������� (Min - ��� :t1:i0, �.�. ����������� ����� ������/������� - ��� ArcInfo � �.�.),
  // ������� ������� (�� ���� ��������� ������ ����������� �������� � ������ ������� ������ - ��� lz/bs ����),
  // � ������� ����� (�� ���� ������� �������� ������ ����� ����� �������� � ���� �����-���� - ��� bs ���� � lzp)
  virtual MemSize GetCompressionMem        (void)         = 0;
  virtual MemSize GetMinCompressionMem     (void)               {return GetCompressionMem();}
  virtual MemSize GetMinDecompressionMem   (void)               {return GetDecompressionMem();}
  // ��������� ����� ������ �� ������������� ��������� ���-�� ������, ������� ��� ������� �����
  virtual void    SetDictionary            (MemSize dict)       {}
  virtual void    SetBlockSize             (MemSize bs)         {}
  virtual void    SetCompressionMem        (MemSize mem)  = 0;
  virtual void    SetMinDecompressionMem   (MemSize mem)  = 0;  // ��� -ld ��� �������� (�.�. ��� :t1:i0): ����������� ����������� ����� ������, ��������� ��� ����������
  // ���������� ������������ ��� ��������/���������� ������, ��� ������� / ������ �����
  virtual void    LimitDictionary          (MemSize dict)       {if (dict>0 && GetDictionary()          > dict)  SetDictionary(dict);}
  virtual void    LimitBlockSize           (MemSize bs)         {if (bs>0   && GetBlockSize()           > bs)    SetBlockSize(bs);}
  virtual void    LimitCompressionMem      (MemSize mem)        {if (mem>0  && GetCompressionMem()      > mem)   SetCompressionMem(mem);}
  virtual void    LimitMinDecompressionMem (MemSize mem)        {if (mem>0  && GetMinDecompressionMem() > mem)   SetMinDecompressionMem(mem);}
#endif
  virtual MemSize GetDictionary            (void)               {return 0;}
  virtual MemSize GetBlockSize             (void)               {return 0;}
  virtual MemSize GetAlgoMem               (void);                            // ����� ������, ��������������� ��������
  virtual MemSize GetDecompressionMem      (void)         = 0;
  virtual void    SetDecompressionMem      (MemSize mem)        {}    // ��� -ld ��� ���������� (�.�. ������ ������ ��������� ���� :t:i, �������� ������������� � ������������ �������)
  virtual void    LimitDecompressionMem    (MemSize mem)        {if (mem>0  && GetDecompressionMem() > mem)   SetDecompressionMem(mem);}

  // Maximum possible inflation of incompressible input data
  virtual LongMemSize GetMaxCompressedSize (LongMemSize insize) {return insize + (insize/4) + 16*kb;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � ParseCompressionMethod)
  virtual void ShowCompressionMethod (char *buf, bool purify) = 0;

  // ������������� �����. ���������:
  //   what: "compress", "decompress", "setCompressionMem", "limitDictionary"...
  //   data: ������ ��� �������� � �������, ��������� �� ���������� ����������� ��������
  //   param&result: ������� �������� ��������, ��� ���������� ��� ������ �������������� ��������
  // �������������� ��������� ��������������� � NULL/0. result<0 - ��� ������
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);

  // Check boolean method property
  bool is (char *request)   {return doit (request, 0, NULL, NULL) > 0;}

  double addtime;  // �������������� �����, ����������� �� ������ (�� ������� ����������, �������������� threads � �.�.)
  COMPRESSION_METHOD() {addtime=0;}
  virtual ~COMPRESSION_METHOD() {}
//  Debugging code:  char buf[100]; ShowCompressionMethod(buf,FALSE); printf("%s : %u => %u\n", buf, GetCompressionMem(), mem);
};


// ****************************************************************************************************************************
// ������� COMPRESSION_METHOD *************************************************************************************************
// ****************************************************************************************************************************

// ��������������� ������ ������ - ���������� COMPRESSION_METHOD,
// ����������� ����� ������, �������� � ���� ������ `method`
COMPRESSION_METHOD *ParseCompressionMethod (char* method);

typedef COMPRESSION_METHOD* (*CM_PARSER) (char** parameters);
typedef COMPRESSION_METHOD* (*CM_PARSER2) (char** parameters, void *data);
int AddCompressionMethod         (CM_PARSER parser);  // �������� ������ ������ ������ � ������ �������������� ������� ������
int AddExternalCompressionMethod (CM_PARSER2 parser2, void *data);  // �������� ������ �������� ������ ������ � �������������� ����������, ������� ������ ���� ������� ����� �������
#endif  // __cplusplus
void ClearExternalCompressorsTable (void);                          // �������� ������� ������� �����������
#ifdef __cplusplus


// ****************************************************************************************************************************
// ����� "������" STORING *****************************************************************************************************
// ****************************************************************************************************************************

// ���������� ������ "������" STORING
class STORING_METHOD : public COMPRESSION_METHOD
{
public:
  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������
  virtual MemSize GetCompressionMem        (void)               {return BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize)            {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return BUFFER_SIZE;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ (�������, �������� � parse_STORING)
  virtual void ShowCompressionMethod (char *buf, bool purify)   {sprintf (buf, "storing");}
};

// ��������� ������ ������ ������ STORING
COMPRESSION_METHOD* parse_STORING (char** parameters);


// ****************************************************************************************************************************
// ����� "������" CRC: ������ ������ � ������ �� ����� ************************************************************************
// ****************************************************************************************************************************

// ���������� ������ "������" crc
class CRC_METHOD : public COMPRESSION_METHOD
{
public:
  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata) {return FREEARC_ERRCODE_INTERNAL;}
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������
  virtual MemSize GetCompressionMem        (void)               {return BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize)            {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return BUFFER_SIZE;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ (�������, �������� � parse_CRC)
  virtual void ShowCompressionMethod (char *buf, bool purify)   {sprintf (buf, "crc");}
};

// ��������� ������ ������ "������" crc
COMPRESSION_METHOD* parse_CRC (char** parameters);


// ****************************************************************************************************************************
// ����� "������" FAKE: �� ������ ������ � ������ �� ����� ********************************************************************
// ****************************************************************************************************************************

// ���������� ������ "������" fake
class FAKE_METHOD : public COMPRESSION_METHOD
{
public:
  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata) {return FREEARC_ERRCODE_INTERNAL;}
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata) {return FREEARC_ERRCODE_INTERNAL;}

  // ��������/���������� ����� ������, ������������ ��� ��������/����������
  virtual MemSize GetCompressionMem        (void)               {return BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize)            {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return BUFFER_SIZE;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ (�������, �������� � parse_FAKE)
  virtual void ShowCompressionMethod (char *buf, bool purify)   {sprintf (buf, "fake");}
};

// ��������� ������ ������ ������ STORING
COMPRESSION_METHOD* parse_FAKE (char** parameters);

#endif  // __cplusplus


// ****************************************************************************************************************************
// (De)compress data from memory buffer (input) to another memory buffer (output)                                             *
// ****************************************************************************************************************************

// ���������, �������� ������� � ������� ������/������ ��� ��������/���������� � ������
struct MemBuf
{
  MemBuf (void *input, int inputSize, void *output, int outputSize, CALLBACK_FUNC *_callback=0, void *_auxdata=0)
  {
    readPtr=(BYTE*)input, readLeft=inputSize, writePtr=(BYTE*)output, writeLeft=writeBufferSize=outputSize, callback=_callback, auxdata=_auxdata;
  }

  // ������� ������ ���� �������� � �����
  int written()  {return writeBufferSize-writeLeft;}

  BYTE *readPtr;          // ������� ������� �������� ������ (NULL, ���� ���� ������ ������ ����� callback)
  int   readLeft;         // ������� ���� ��� �������� �� ������� ������
  BYTE *writePtr;         // ������� ������� ������������ ������ (NULL, ���� ���� ���������� ������ ����� callback)
  int   writeLeft;        // ������� ���� ��� �������� � �������� ������
  int   writeBufferSize;  // ������ ������ ��������� ������
  CALLBACK_FUNC *callback;
  void *auxdata;
};

// Callback-������� ������/������ ��� (���)������� � ������
int ReadWriteMem (const char *what, void *buf, int size, void *_membuf);


// ****************************************************************************************************************************
// ENCRYPTION ROUTINES *****************************************************************************************************
// ****************************************************************************************************************************

// Generates key based on password and salt using given number of hashing iterations
void Pbkdf2Hmac (const BYTE *pwd, int pwdSize, const BYTE *salt, int saltSize,
                 int numIterations, BYTE *key, int keySize);

int fortuna_size (void);


#ifdef __cplusplus
}       // extern "C"
#endif

#endif  // FREEARC_COMPRESSION_H
