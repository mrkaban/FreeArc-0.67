// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Content-defined chunking: searching for chunk boundaries  ****************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Find chunk boundaries for -m1 in 3 streams (moving this code into separate routine improves speed)
template <class HashType, class RollingHash, int WINSIZE, int PIECE>
BYTE **fast_find_chunks_in_3_streams (BYTE *ptr, BYTE **lastmark, HashType maxhash, unsigned MIN_MATCH, RollingHash &hash)
{
  BYTE *lastp1=ptr, *lastp2=ptr+PIECE, *lastp3=ptr+2*PIECE;
  RollingHash  hash1 = hash;  hash1.moveto (lastp1);
  RollingHash  hash2 = hash;  hash2.moveto (lastp2);
  RollingHash  hash3 = hash;  hash3.moveto (lastp3);

  for (BYTE *p=ptr+WINSIZE, *pend=ptr+PIECE;  likely (p<pend);  p++)
  {
    hash1.update (p[       -WINSIZE], p[      0]);   if (unlikely(hash1>maxhash) && p        -lastp1>=MIN_MATCH)  *lastmark++ = lastp1 = p;
    hash2.update (p[  PIECE-WINSIZE], p[  PIECE]);   if (unlikely(hash2>maxhash) && p+PIECE  -lastp2>=MIN_MATCH)  *lastmark++ = lastp2 = p+  PIECE;
    hash3.update (p[2*PIECE-WINSIZE], p[2*PIECE]);   if (unlikely(hash3>maxhash) && p+2*PIECE-lastp3>=MIN_MATCH)  *lastmark++ = lastp3 = p+2*PIECE;
  }
  return lastmark;
}


// Find chunk boundaries using fast method (-m1): compute rolling hash of last WINSIZE bytes, selecting points with hash<UINT_MAX/L
template <class HashType, class RollingHash, int WINSIZE, int STRIPE>
BYTE **fast_find_chunks (BYTE* &ptr, BYTE* pend, BYTE* buf, BYTE* bufend, BYTE** marks, unsigned L, unsigned MIN_MATCH, RollingHash &hash)
{
  const HashType HashTypeMax = HashType(-1),  maxhash = HashTypeMax - HashTypeMax/L;  // probability of "x>maxhash" for random "HashType x" should be 100%/L

  BYTE **lastmark = marks;
  if (pend-ptr >= STRIPE/3*3)
  {
    // Either we process STRIPE bytes in 3 streams and then sort found marks ...
    lastmark = fast_find_chunks_in_3_streams<HashType,RollingHash,WINSIZE,STRIPE/3> (ptr, lastmark, maxhash, MIN_MATCH, hash);
    std::sort(marks,lastmark);
    ptr += STRIPE/3*3;
  }
  else
  {
    // ... or sequentially process last remaining bytes in the buf
    if (pend-ptr >= WINSIZE)
    {
      BYTE *lastp1=ptr;  RollingHash hash1 = hash;  hash1.moveto (lastp1);
      for (BYTE *p=ptr+WINSIZE;  likely (p<pend);  p++) {
        hash1.update (p[-WINSIZE], p[0]);   if (unlikely(hash1>maxhash) && p-lastp1>=MIN_MATCH)  *lastmark++ = lastp1 = p;
      }
    }
    ptr = pend;
  }

  if (pend==bufend)  *lastmark++ = bufend;
  return lastmark;
}


// Find chunk boundaries using zpaq method (-m2): compute hash of window that includes last 32 bytes mispredicted by order-1 model; and select points with hash<UINT_MAX/L
template <class HashType>
BYTE **zpaq_find_chunks (BYTE* &ptr, BYTE* pend, BYTE* buf, BYTE* bufend, BYTE** lastmark, unsigned L, unsigned MIN_MATCH)
{
  const HashType HashTypeMax = HashType(-1),  maxhash = HashTypeMax - HashTypeMax/L;  // probability of "x>maxhash" for random "HashType x" should be 100%/L

  HashType hash = 0;       // rolling hash for segmentation
  BYTE     c1 = 0;         // last input byte
  BYTE     o1[256] = {0};  // order-1 predicted byte

  // Modeling starts at 8000 bytes prior to stripe beginning in order to accomodate preceding data
  for (BYTE *p = (ptr-buf>8000? ptr-8000 : buf), *lastp=p;  likely (p<pend);  p++)
  {
    BYTE c = *p;
    hash   = (hash+c+1) * (c!=o1[c1]? 271828182 : 314159265);
    o1[c1] = c;
    c1     = c;
    if (unlikely(hash>maxhash) && p-lastp>=MIN_MATCH)
    {
      if (p>ptr)  *lastmark++ = p;
      lastp = p;
      c1 = hash = 0;
      my_memset (o1, 0, sizeof(o1));
    }
  }
  ptr = pend;
  if (pend==bufend)  *lastmark++ = bufend;
  return lastmark;
}



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Content-defined chunking: multithreading  ********************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const int STRIPE = 116*kb;  // STRIPE bytes plus two VHASH multipliers table should fit into the last per-core cache (512 kb for AMD, 256kb/2 for Intel due to HyperThreading)
const int MAX_CHUNKS = STRIPE/MINIMAL_MIN_MATCH+2;
const int WINSIZE = 48;   // Hash of last WINSIZE bytes used to select points for chunk ends

// CDC thread local storage
struct CDC_Thread
{
  BYTE *marks[MAX_CHUNKS], **lastmark;                   // chunk pointers
  VHash vhash1, vhash2;                                  // hashing engines
  BYTE vhashes[2*VMAC_TAG_LEN_BYTES*MAX_CHUNKS];         // chunk hashes (should be the last field to ensure that different cpu threads don't use the same 64-byte memory lines)

  void init (VHash &_vhash1, VHash &_vhash2)  {vhash1=_vhash1; vhash2=_vhash2;}

  void compute_single_chunk_hash (BYTE *begin, BYTE *end, BYTE *vhash)
  {
    vhash1.compute (begin, end-begin, vhash);
    vhash2.compute (begin, end-begin, vhash+VMAC_TAG_LEN_BYTES);
  }

  void compute_chunk_hashes()
  {
    BYTE *vhash  =  vhashes + 2*VMAC_TAG_LEN_BYTES;     // the first vhashes[] slot is reserved for the chunk split between current and previous stripes
    for (BYTE **curmark=marks; curmark+1<lastmark; curmark++)
      compute_single_chunk_hash (*curmark, curmark[1], vhash),  vhash += 2*VMAC_TAG_LEN_BYTES;
  }
};

// Single job performed by CDC thread
struct CDC_Job
{
  CDC_Thread *t;
  bool ZPAQ_CDC;
  BYTE *ptr, *pend, *buf, *bufend;
  unsigned L, MIN_MATCH;
  PolynomialRollingHash<size_t> *hash1;
  CrcRollingHash       <uint32> *hash2;

  void process()
  {
    t->lastmark = ZPAQ_CDC? zpaq_find_chunks<uint32>                                                 (ptr, pend, buf, bufend, t->marks, L, MIN_MATCH)
                 :crc32c()? fast_find_chunks<uint32, CrcRollingHash<uint32>,        WINSIZE, STRIPE> (ptr, pend, buf, bufend, t->marks, L, MIN_MATCH, *hash2)
                          : fast_find_chunks<size_t, PolynomialRollingHash<size_t>, WINSIZE, STRIPE> (ptr, pend, buf, bufend, t->marks, L, MIN_MATCH, *hash1);
#ifndef FIND_ONLY
    t->compute_chunk_hashes();
#endif
  }
};

// CDC multi-threading global context
struct CDC_Global
{
  CDC_Thread *ThreadStorage;
  MultipleProcessingThreads<CDC_Job> Threads;
  VHash vhash1, vhash2;
  int errcode;

  CDC_Global (bool CONTENT_DEFINED_CHUNKING, int NumThreads)
  {
    errcode = NO_ERRORS;  if (!CONTENT_DEFINED_CHUNKING)  return;
    ThreadStorage = new CDC_Thread[NumThreads];  Threads.MaxJobs = NumThreads;  Threads.NumThreads = NumThreads;
    if (!ThreadStorage || Threads.start()!=0)  {errcode=ERROR_MEMORY; return;}   // Error: not enough memory

    vhash1.init(); vhash2.init();
    for (int i=0; i<NumThreads; i++)
      ThreadStorage[i].init (vhash1, vhash2);
  }

  ~CDC_Global()  {if(errcode==0)  Threads.finish();}
};



// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Content-defined chunking: single block compressor  ***********************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Compress buf[] using content-defined chunking and return compressed data in outbuf[] and statbuf[]
void compress_CDC (bool ZPAQ_CDC, unsigned L, unsigned MIN_MATCH, Offset block_start, HashTable &h, CDC_Global &g, char *_buf, int block_size, unsigned &literal_bytes, STAT *statbuf, STAT *&stat)
{
  BYTE *buf = (BYTE *)_buf,  *bufend = buf+block_size;  literal_bytes = 0;  stat = statbuf;
  BYTE *last_match = buf,  *last_chunk = buf;  if (MIN_MATCH < MINIMAL_MIN_MATCH)   MIN_MATCH = MINIMAL_MIN_MATCH;
  PolynomialRollingHash<size_t>  hash1 (WINSIZE, PRIME1);
  CrcRollingHash       <uint32>  hash2 (WINSIZE, Crc32CastagnoliPolynom);

  int free_jobs=g.Threads.NumThreads, num_thread=0;
  for (BYTE *ptr=buf; free_jobs<g.Threads.NumThreads || ptr<bufend; free_jobs++)
  {
    // Push to queue jobs for processing each stripe
    while (free_jobs>0 && ptr<bufend)
    {
      BYTE *pend  =  bufend-ptr<STRIPE? bufend : ptr+STRIPE;
      CDC_Job job = {g.ThreadStorage+num_thread, ZPAQ_CDC, ptr, pend, buf, bufend, L, MIN_MATCH, &hash1, &hash2};
      g.Threads.Put (job);
      ptr = pend;
      free_jobs--; num_thread=(num_thread+1)%g.Threads.NumThreads;
    }
    CDC_Job job = g.Threads.Get();

#ifndef FIND_ONLY
    // Check chunks for duplicates
    BYTE *vhash = job.t->vhashes;
    if (job.t->marks < job.t->lastmark)   // compute hash of chunk split between cuurent and preceding stripes (or first chunk of the block)
      job.t->compute_single_chunk_hash (last_chunk, job.t->marks[0], vhash);
    for (BYTE **curmark = job.t->marks;  curmark < job.t->lastmark;  curmark++)
    {
      // Save the block to the hash and search for match
      int len = *curmark-last_chunk;
      Offset match_offset = h.find_match_CDC (block_start+(last_chunk-buf), last_chunk, len, vhash);  vhash += 2*VMAC_TAG_LEN_BYTES;
      if (match_offset && len>=MIN_MATCH) {
        ENCODE_LZ_MATCH(stat, false, MIN_MATCH,  last_chunk-last_match, match_offset, len);
        last_match = *curmark;
      } else {
        literal_bytes += len;             // no match, so just count literal bytes
      }
      last_chunk = *curmark;
    }
#endif
  }
}
