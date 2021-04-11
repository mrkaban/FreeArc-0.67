/* LzFindMt.c -- multithreaded Match finder for LZ algorithms
(c) 2009-05-26 Igor Pavlov
(c) 2008,2009,2013 Bulat Ziganshin
This code made available under GPL license.
For a commercial license write to Bulat.Ziganshin@gmail.com
*/

#include "LzHash.h"

#include "LzFindMt.h"

void MtSync_Construct(CMtSync *p)
{
  p->wasCreated = False;
  p->csWasInitialized = False;
  p->csWasEntered = False;
  Thread_Construct(&p->thread);
  Event_Construct(&p->canStart);
  Event_Construct(&p->wasStarted);
  Event_Construct(&p->wasStopped);
  Semaphore_Construct(&p->freeSemaphore);
  Semaphore_Construct(&p->filledSemaphore);
}

void MtSync_GetNextBlock(CMtSync *p)
{
  if (p->needStart)
  {
    p->numProcessedBlocks = 1;
    p->needStart = False;
    p->stopWriting = False;
    p->exit = False;
    Event_Reset(&p->wasStarted);
    Event_Reset(&p->wasStopped);

    Event_Set(&p->canStart);
    Event_Wait(&p->wasStarted);
  }
  else
  {
    CriticalSection_Leave(&p->cs);
    p->csWasEntered = False;
    p->numProcessedBlocks++;
    Semaphore_Release1(&p->freeSemaphore);
  }
  Semaphore_Wait(&p->filledSemaphore);
  CriticalSection_Enter(&p->cs);
  p->csWasEntered = True;
}

/* MtSync_StopWriting must be called if Writing was started */

void MtSync_StopWriting(CMtSync *p)
{
  UInt32 myNumBlocks = p->numProcessedBlocks;
  if (!Thread_WasCreated(&p->thread) || p->needStart)
    return;
  p->stopWriting = True;
  if (p->csWasEntered)
  {
    CriticalSection_Leave(&p->cs);
    p->csWasEntered = False;
  }
  Semaphore_Release1(&p->freeSemaphore);

  Event_Wait(&p->wasStopped);

  while (myNumBlocks++ != p->numProcessedBlocks)
  {
    Semaphore_Wait(&p->filledSemaphore);
    Semaphore_Release1(&p->freeSemaphore);
  }
  p->needStart = True;
}

void MtSync_Destruct(CMtSync *p)
{
  if (Thread_WasCreated(&p->thread))
  {
    MtSync_StopWriting(p);
    p->exit = True;
    if (p->needStart)
      Event_Set(&p->canStart);
    Thread_Wait(&p->thread);
    Thread_Close(&p->thread);
  }
  if (p->csWasInitialized)
  {
    CriticalSection_Delete(&p->cs);
    p->csWasInitialized = False;
  }

  Event_Close(&p->canStart);
  Event_Close(&p->wasStarted);
  Event_Close(&p->wasStopped);
  Semaphore_Close(&p->freeSemaphore);
  Semaphore_Close(&p->filledSemaphore);

  p->wasCreated = False;
}

#define RINOK_THREAD(x) { if ((x) != 0) return SZ_ERROR_THREAD; }

static SRes MtSync_Create2(CMtSync *p, unsigned (MY_STD_CALL *startAddress)(void *), void *obj, UInt32 numBlocks)
{
  if (p->wasCreated)
    return SZ_OK;

  RINOK_THREAD(CriticalSection_Init(&p->cs));
  p->csWasInitialized = True;

  RINOK_THREAD(AutoResetEvent_CreateNotSignaled(&p->canStart));
  RINOK_THREAD(AutoResetEvent_CreateNotSignaled(&p->wasStarted));
  RINOK_THREAD(AutoResetEvent_CreateNotSignaled(&p->wasStopped));

  RINOK_THREAD(Semaphore_Create(&p->freeSemaphore, numBlocks, numBlocks));
  RINOK_THREAD(Semaphore_Create(&p->filledSemaphore, 0, numBlocks));

  p->needStart = True;

  RINOK_THREAD(Thread_Create(&p->thread, startAddress, obj));
  p->wasCreated = True;
  return SZ_OK;
}

static SRes MtSync_Create(CMtSync *p, unsigned (MY_STD_CALL *startAddress)(void *), void *obj, UInt32 numBlocks)
{
  SRes res = MtSync_Create2(p, startAddress, obj, numBlocks);
  if (res != SZ_OK)
    MtSync_Destruct(p);
  return res;
}

void MtSync_Init(CMtSync *p) { p->needStart = True; }

#define kMtMaxValForNormalize(p) kMaxValForNormalize(p)

#define DEF_GetHeads2(name, v, action)                                                                \
static void GetHeads ## name(const Byte *p, UInt32 pos, UInt32 *hash, UInt32 hashMask,                \
                             UInt32 *heads, UInt32 numHeads, const UInt32 *crc, UInt32 cutValue)      \
{                                                                                                     \
  cutValue=cutValue;                                                                                  \
  action                                                                                              \
  for (; numHeads != 0; numHeads--)                                                                   \
    {const UInt32 value = (v); p++; *heads++ = pos - hash[value]; hash[value] = pos++;}               \
}


#define DEF_GetHeads(name, v) DEF_GetHeads2(name, v, ;)

DEF_GetHeads2(2,  (p[0] | ((UInt32)p[1] << 8)), hashMask = hashMask; crc = crc; )
DEF_GetHeads(3,  (crc[p[0]] ^ p[1] ^ ((UInt32)p[2] << 8)) & hashMask)
DEF_GetHeads(4,  (crc[p[0]] ^ p[1] ^ ((UInt32)p[2] << 8) ^ (crc[p[3]] << 5)) & hashMask)
DEF_GetHeads(4b, (crc[p[0]] ^ p[1] ^ ((UInt32)p[2] << 8) ^ ((UInt32)p[3] << 16)) & hashMask)
/* DEF_GetHeads(5,  (crc[p[0]] ^ p[1] ^ ((UInt32)p[2] << 8) ^ (crc[p[3]] << 5) ^ (crc[p[4]] << 3)) & hashMask) */

static void GetHeadsHt4(const Byte *p, UInt32 pos, UInt32 *hash, UInt32 hashMask,
                        UInt32 *heads, UInt32 numHeads, const UInt32 *crc, UInt32 cutValue)
{
  int shiftBits = 32 - lb(hashMask+1);
  for (; numHeads != 0; numHeads--)
  {
    const UInt32 value = (*(UInt32*)p * 1234567891) >> shiftBits;
    p++;
#ifdef FREEARC_64BIT
    *heads++ = (UInt32) (value*cutValue);          // First entry in hash table to check, index
#else
    *heads++ = (UInt32) &(hash[value*cutValue]);   // First entry in hash table to check, address
#endif
  }
}

// Тред, сканирующий входной блок, и запоминающий в heads[] значение хеш-функции для каждой позиции
void HashThreadFunc(CMatchFinderMt *mt)
{
  SetCompressionThreadPriority();  // уменьшим приоритет для треда сжатия (функция из Common.cpp)
  CMtSync *p = &mt->hashSync;
  for (;;)
  {
    UInt32 numProcessedBlocks = 0;
    Event_Wait(&p->canStart);
    Event_Set(&p->wasStarted);
    for (;;)
    {
      if (p->exit)
        return;
      if (p->stopWriting)
      {
        p->numProcessedBlocks = numProcessedBlocks;
        Event_Set(&p->wasStopped);
        break;
      }

      {
        CMatchFinder *mf = mt->MatchFinder;
        if (MatchFinder_NeedMove(mf))
        {
          CriticalSection_Enter(&mt->btSync.cs);
          CriticalSection_Enter(&mt->hashSync.cs);
          {
            const Byte *beforePtr = MatchFinder_GetPointerToCurrentPos(mf);
            const Byte *afterPtr;
            MatchFinder_MoveBlock(mf);
            afterPtr = MatchFinder_GetPointerToCurrentPos(mf);
            mt->pointerToCurPos -= beforePtr - afterPtr;
            mt->buffer -= beforePtr - afterPtr;
          }
          CriticalSection_Leave(&mt->btSync.cs);
          CriticalSection_Leave(&mt->hashSync.cs);
          continue;
        }

        Semaphore_Wait(&p->freeSemaphore);

        MatchFinder_ReadIfRequired(mf);
        if (mf->pos > (kMtMaxValForNormalize(mf) - kMtHashBlockSize))
        {
          UInt32 subValue = (mf->pos - mf->historySize - 1);
          MatchFinder_ReduceOffsets(mf, subValue);
          if (mf->btMode != MF_HashTable)  // HT matchfinder uses MatchFinder->hash in other thread
            MatchFinder_Normalize3(mf, subValue, mf->hash + mf->fixedHashSize, mf->hashSizeSum - mf->fixedHashSize, mf->btMode);
        }
        {
          UInt32 *heads = mt->hashBuf + ((numProcessedBlocks++) & kMtHashNumBlocksMask) * kMtHashBlockSize;
          UInt32 num = mf->streamPos - mf->pos;
          heads[0] = 2;
          heads[1] = num;
          if (num >= mf->numHashBytes)
          {
            num = num - mf->numHashBytes + 1;
            if (num > kMtHashBlockSize - 2)
              num = kMtHashBlockSize - 2;
            mt->GetHeadsFunc(mf->buffer, mf->pos, mf->hash + mf->fixedHashSize, mf->hashMask, heads + 2, num, mf->crc, mf->cutValue);
            heads[0] += num;
          }
          mf->pos += num;
          mf->buffer += num;
        }
      }

      Semaphore_Release1(&p->filledSemaphore);
    }
  }
}

void MatchFinderMt_GetNextBlock_Hash(CMatchFinderMt *p)
{
  MtSync_GetNextBlock(&p->hashSync);
  p->hashBufPosLimit = p->hashBufPos = ((p->hashSync.numProcessedBlocks - 1) & kMtHashNumBlocksMask) * kMtHashBlockSize;
  p->hashBufPosLimit += p->hashBuf[p->hashBufPos++];
  p->hashNumAvail = p->hashBuf[p->hashBufPos++];
}

#define kEmptyHashValue 0

#define MFMT_GM_INLINE

#ifdef MFMT_GM_INLINE

UInt32 *GetMatchesSpecN(CMatchFinderMt *p, UInt32 lenLimit, UInt32 pos, const Byte *cur, CLzRef *son,
    UInt32 _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 _cutValue,
    UInt32 *distances, UInt32 *maxDistances, UInt32 _maxLen, const UInt32 *hash, UInt32 size, UInt32 *posRes)
{
  struct Context  { CLzRef *ptr0, *ptr1, *pair;  const Byte *cur, *pb;  UInt32 pos, cyclicBufferPos, curMatch, len, len0, len1, cutValue, maxLen, delta; };
  UInt32 local_distances_buf[900];  UInt32 *local_distances = local_distances_buf,  *local_distances_head = NULL;

  do
  {
    Context x,y;  y.pb = NULL;  UInt32 *distances_head = distances++;

    x.cyclicBufferPos = _cyclicBufferPos++;  x.pos = pos++;  x.cur = cur++;
    x.ptr0 = son + ((size_t)x.cyclicBufferPos << 1) + 1;
    x.ptr1 = son + ((size_t)x.cyclicBufferPos << 1);
    x.delta = *hash++;
    if (x.delta >= _cyclicBufferSize-100)    { *x.ptr0 = *x.ptr1 = kEmptyHashValue;  *distances_head = 0;  continue; }

    x.pb  = x.cur - x.delta;
    x.len = 0;  prefetch(x.pb[x.len]);

    {size_t index = (x.cyclicBufferPos - x.delta + ((x.delta > x.cyclicBufferPos) ? _cyclicBufferSize : 0));
    x.pair = son + (index << 1);  prefetch(*x.pair);}

    x.curMatch = x.pos - x.delta;
    x.len0 = x.len1 = 0;  x.cutValue = _cutValue;  x.maxLen = _maxLen;


ynext:  // verify that we have match to check, it doesn't belong to the same hash bucket as x.pos, and there is enough space in distances[] & local_distances[]
    if (size == 1  ||  (y.curMatch = pos - (y.delta = *hash))  ==  x.pos
                   ||  pos >= x.pos+90
                   ||  maxDistances - distances  <  (p->matchMaxLen * 4) + (local_distances-local_distances_buf)  // *4 because we save len&dist for x&y
                   ||  local_distances_buf+sizeof(local_distances_buf)/sizeof(*local_distances_buf) - local_distances  <  (p->matchMaxLen * 2))
    {
      y.pb = NULL;
    }
    else
    {
      size--;  hash++;  local_distances_head = local_distances++;
      y.cyclicBufferPos = _cyclicBufferPos++;  y.pos = pos++;  y.cur = cur++;

      y.ptr0 = son + ((size_t)y.cyclicBufferPos << 1) + 1;
      y.ptr1 = son + ((size_t)y.cyclicBufferPos << 1);
      if (y.delta >= _cyclicBufferSize-100)    { *y.ptr0 = *y.ptr1 = kEmptyHashValue;  *local_distances_head = 0;  goto ynext; }

      y.pb  = y.cur - y.delta;
      y.len = 0;  prefetch(y.pb[y.len]);

      {size_t index = (y.cyclicBufferPos - y.delta + ((y.delta > y.cyclicBufferPos) ? _cyclicBufferSize : 0));
      y.pair = son + (index << 1);  prefetch(*y.pair);}

      y.len0 = y.len1 = 0;  y.cutValue = _cutValue;  y.maxLen = _maxLen;
    }

    for(;;)
    {
      if (x.pb[x.len] == x.cur[x.len])
      {
        if (++x.len != lenLimit  &&  x.pb[x.len] == x.cur[x.len])
          while (++x.len != lenLimit)
            if (x.pb[x.len] != x.cur[x.len])
              break;
        if (x.maxLen < x.len)
        {
          *distances++ = x.maxLen = x.len;
          *distances++ = x.delta - 1;
          if (x.len == lenLimit)       { *x.ptr1 = x.pair[0];  *x.ptr0 = x.pair[1];  goto xdone; }
        }
      }
      if (x.pb[x.len] < x.cur[x.len])    *x.ptr1 = x.curMatch,  x.ptr1 = x.pair+1,  x.curMatch = *x.ptr1,  x.len1 = x.len;
      else                               *x.ptr0 = x.curMatch,  x.ptr0 = x.pair,    x.curMatch = *x.ptr0,  x.len0 = x.len;

      x.delta = x.pos - x.curMatch;
      if (--x.cutValue == 0  ||  x.delta >= _cyclicBufferSize-100)    { *x.ptr0 = *x.ptr1 = kEmptyHashValue;  goto xdone; }
      {size_t index = (x.cyclicBufferPos - x.delta + ((x.delta > x.cyclicBufferPos) ? _cyclicBufferSize : 0));
      x.pair = son + (index << 1);  prefetch(*x.pair);}
      x.pb   = x.cur - x.delta;
      x.len  = (x.len0 < x.len1 ? x.len0 : x.len1);  prefetch(x.pb[x.len]);
//-----------------------------------------------------------------------------
      if (y.pb == NULL)  continue;
      if (y.pb[y.len] == y.cur[y.len])
      {
        if (++y.len != lenLimit  &&  y.pb[y.len] == y.cur[y.len])
          while (++y.len != lenLimit)
            if (y.pb[y.len] != y.cur[y.len])
              break;
        if (y.maxLen < y.len)
        {
          *local_distances++ = y.maxLen = y.len;
          *local_distances++ = y.delta - 1;
          if (y.len == lenLimit)       { *y.ptr1 = y.pair[0];  *y.ptr0 = y.pair[1];  goto ydone; }
        }
      }
      if (y.pb[y.len] < y.cur[y.len])    *y.ptr1 = y.curMatch,  y.ptr1 = y.pair+1,  y.curMatch = *y.ptr1,  y.len1 = y.len;
      else                               *y.ptr0 = y.curMatch,  y.ptr0 = y.pair,    y.curMatch = *y.ptr0,  y.len0 = y.len;

      y.delta = y.pos - y.curMatch;
      if (--y.cutValue == 0  ||  y.delta >= _cyclicBufferSize-100)    { *y.ptr0 = *y.ptr1 = kEmptyHashValue;  goto ydone; }
      {size_t index = (y.cyclicBufferPos - y.delta + ((y.delta > y.cyclicBufferPos) ? _cyclicBufferSize : 0));
      y.pair = son + (index << 1);  prefetch(*y.pair);}
      y.pb   = y.cur - y.delta;
      y.len  = (y.len0 < y.len1 ? y.len0 : y.len1);  prefetch(y.pb[y.len]);
    }


ydone:
    *local_distances_head = UInt32(local_distances - local_distances_head) - 1;
    goto ynext;


xdone:
    *distances_head = UInt32(distances - distances_head) - 1;

    // Move y.len/y.delta values, temporarily stored in the local_distances_buf[], to the global distances[] buffer
    memcpy(distances, local_distances_buf, (local_distances - local_distances_buf)*sizeof(*local_distances_buf));
    distances_head = distances + (local_distances_head - local_distances_buf);
    distances += local_distances - local_distances_buf;
    local_distances = local_distances_buf;

    // If the second Context y is active - continue to process it
    if (y.pb)  { x = y;  goto ynext; }
  }
  while (distances < maxDistances  &&  --size != 0);


  *posRes = pos;
  return distances;
}

#endif

/* HT matchfinder helper */
UInt32 *getMatchPtr(CMatchFinderMt *p, const UInt32 *pos)
{
#ifdef FREEARC_64BIT
  CMatchFinder *mf = p->MatchFinder;
  UInt32 *hash = mf->hash + mf->fixedHashSize;
  return hash + *pos;
#else
  return (UInt32 *) *pos;    // First entry in hash table to check
#endif
}

UInt32 *Ht_GetMatchesSpecN(CMatchFinderMt *p, UInt32 lenLimit, UInt32 pos, const Byte *cur, CLzRef *son,
    UInt32 _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 _cutValue,
    UInt32 *distances, UInt32 *maxDistances, UInt32 _maxLen, const UInt32 *hash, UInt32 size, UInt32 *posRes)
{
  const int HASHES_PER_ROW = CACHE_ROW/sizeof(UInt32);
  const int PREFETCH_WINDOW = 200 / (_cutValue+1);   // 200 is the maximum memory latency (500-1000 cycles), divided by the minimum time required to check one match (3-4 cycles). OTOH, it's the maximum number of lines we are allowed to fill in the L1 cache: 200*64=12800 bytes
  const int HASH_ROWS = (_cutValue-1) / HASHES_PER_ROW + 1;
  do
  {
    if (size > PREFETCH_WINDOW)
    {
      // Prefetch head[] array
      UInt32 *mp = getMatchPtr(p, hash+PREFETCH_WINDOW);
      for (int i=0;  i<HASH_ROWS;  i++)  // fetch multiple cache rows if necessary
        prefetch(mp[i*HASHES_PER_ROW]);
    }
    UInt32 *curMatchPtr = getMatchPtr(p, hash++),  *distances_head = distances;
    distances = Ht_GetMatchesSpec(p->MatchFinder, lenLimit, curMatchPtr, pos, cur, son, _cyclicBufferPos, _cyclicBufferSize, _cutValue, distances+1, _maxLen);
    *distances_head = distances - distances_head - 1;
    pos++;  cur++;  _cyclicBufferPos++;
  }
  while (distances < maxDistances  &&  --size != 0);

  *posRes = pos;
  return distances;
}

void BtGetMatches(CMatchFinderMt *p, UInt32 *distances)
{
  UInt32 numProcessed = 0;
  UInt32 curPos = 2;
  UInt32 limit = kMtBtBlockSize - (p->matchMaxLen * 2);
  distances[1] = p->hashNumAvail;
  while (curPos < limit)
  {
    if (p->hashBufPos == p->hashBufPosLimit)
    {
      MatchFinderMt_GetNextBlock_Hash(p);
      distances[1] = numProcessed + p->hashNumAvail;
      if (p->hashNumAvail >= p->numHashBytes)
        continue;
      for (; p->hashNumAvail != 0; p->hashNumAvail--)
        distances[curPos++] = 0;
      break;
    }
    {
      UInt32 size = p->hashBufPosLimit - p->hashBufPos;
      UInt32 lenLimit = p->matchMaxLen;
      UInt32 pos = p->pos;
      UInt32 cyclicBufferPos = p->cyclicBufferPos;
      if (lenLimit >= p->hashNumAvail)
        lenLimit = p->hashNumAvail;
      {
        UInt32 size2 = p->hashNumAvail - lenLimit + 1;
        if (size2 < size)
          size = size2;
        size2 = p->cyclicBufferSize - cyclicBufferPos;
        if (size2 < size)
          size = size2;
      }

      if (p->MatchFinder->btMode == MF_BinaryTree)
      {
        #ifndef MFMT_GM_INLINE
        while (curPos < limit && size-- != 0)
        {
          UInt32 *startDistances = distances + curPos;
          UInt32 num = (UInt32)(GetMatchesSpec1(lenLimit, pos - p->hashBuf[p->hashBufPos++],
            pos, p->buffer, p->son, cyclicBufferPos, p->cyclicBufferSize, p->cutValue,
            startDistances + 1, p->numHashBytes - 1) - startDistances);
          *startDistances = num - 1;
          curPos += num;
          cyclicBufferPos++;
          pos++;
          p->buffer++;
        }
        #else
        {
          UInt32 posRes;
          curPos = GetMatchesSpecN(p, lenLimit, pos, p->buffer, p->son, cyclicBufferPos, p->cyclicBufferSize, p->cutValue,
                                   distances + curPos, distances + limit, p->numHashBytes - 1, p->hashBuf + p->hashBufPos, size, &posRes)  -  distances;
          p->hashBufPos += posRes - pos;
          cyclicBufferPos += posRes - pos;
          p->buffer += posRes - pos;
          pos = posRes;
        }
        #endif
      }
      else if (p->MatchFinder->btMode == MF_HashTable)
      {
        UInt32 posRes;
        curPos = Ht_GetMatchesSpecN(p, lenLimit, pos, p->buffer, p->son, cyclicBufferPos, p->cyclicBufferSize, p->cutValue,
                                    distances + curPos, distances + limit, p->numHashBytes - 1, p->hashBuf + p->hashBufPos, size, &posRes)  -  distances;
        p->hashBufPos += posRes - pos;
        cyclicBufferPos += posRes - pos;
        p->buffer += posRes - pos;
        pos = posRes;
      }
      else // if (p->MatchFinder->btMode == MF_HashChain)
      {
        while (curPos < limit && size-- != 0)
        {
          UInt32 *startDistances = distances + curPos;
          UInt32 num = (UInt32)(Hc_GetMatchesSpec(lenLimit, pos - p->hashBuf[p->hashBufPos++],
            pos, p->buffer, p->son, cyclicBufferPos, p->cyclicBufferSize, p->cutValue,
            startDistances + 1, p->numHashBytes - 1) - startDistances);
          *startDistances = num - 1;
          curPos += num;
          cyclicBufferPos++;
          pos++;
          p->buffer++;
        }
      }

      numProcessed += pos - p->pos;
      p->hashNumAvail -= pos - p->pos;
      p->pos = pos;
      if (cyclicBufferPos == p->cyclicBufferSize)
        cyclicBufferPos = 0;
      p->cyclicBufferPos = cyclicBufferPos;
    }
  }
  distances[0] = curPos;
}

void BtFillBlock(CMatchFinderMt *p, UInt32 globalBlockIndex)
{
  CMatchFinder *mf = p->MatchFinder;
  CMtSync *sync = &p->hashSync;
  if (!sync->needStart)
  {
    CriticalSection_Enter(&sync->cs);
    sync->csWasEntered = True;
  }

  BtGetMatches(p, p->btBuf + (globalBlockIndex & kMtBtNumBlocksMask) * kMtBtBlockSize);

  if (p->pos > kMtMaxValForNormalize(mf) - kMtBtBlockSize)
  {
    UInt32 subValue = p->pos - p->cyclicBufferSize;
    if (mf->btMode == MF_HashTable)   // HT matchfinder uses MatchFinder->hash in this thread
      MatchFinder_Normalize3(mf, subValue, mf->hash + mf->fixedHashSize, mf->hashSizeSum - mf->fixedHashSize, mf->btMode);
    MatchFinder_Normalize3(mf, subValue, p->son, mf->numSons, mf->btMode);
    p->pos -= subValue;
  }

  if (!sync->needStart)
  {
    CriticalSection_Leave(&sync->cs);
    sync->csWasEntered = False;
  }
}

void BtThreadFunc(CMatchFinderMt *mt)
{
  SetCompressionThreadPriority();  // уменьшим приоритет для треда сжатия (функция из Common.cpp)
  CMtSync *p = &mt->btSync;
  for (;;)
  {
    UInt32 blockIndex = 0;
    Event_Wait(&p->canStart);
    Event_Set(&p->wasStarted);
    for (;;)
    {
      if (p->exit)
        return;
      if (p->stopWriting)
      {
        p->numProcessedBlocks = blockIndex;
        MtSync_StopWriting(&mt->hashSync);
        Event_Set(&p->wasStopped);
        break;
      }
      Semaphore_Wait(&p->freeSemaphore);
      BtFillBlock(mt, blockIndex++);
      Semaphore_Release1(&p->filledSemaphore);
    }
  }
}

void MatchFinderMt_Construct(CMatchFinderMt *p)
{
  p->hashBuf = 0;
  MtSync_Construct(&p->hashSync);
  MtSync_Construct(&p->btSync);
}

void MatchFinderMt_FreeMem(CMatchFinderMt *p, ISzAlloc *alloc)
{
  alloc->Free(alloc, p->hashBuf);
  p->hashBuf = 0;
}

void MatchFinderMt_Destruct(CMatchFinderMt *p, ISzAlloc *alloc)
{
  MtSync_Destruct(&p->hashSync);
  MtSync_Destruct(&p->btSync);
  MatchFinderMt_FreeMem(p, alloc);
}

#define kHashBufferSize (kMtHashBlockSize * kMtHashNumBlocks)
#define kBtBufferSize (kMtBtBlockSize * kMtBtNumBlocks)

static unsigned MY_STD_CALL HashThreadFunc2(void *p) { HashThreadFunc((CMatchFinderMt *)p);  return 0; }
static unsigned MY_STD_CALL BtThreadFunc2(void *p)
{
  Byte allocaDummy[0x180];
  int i = 0;
  for (i = 0; i < 16; i++)
    allocaDummy[i] = (Byte)i;
  BtThreadFunc((CMatchFinderMt *)p);
  return 0;
}

SRes MatchFinderMt_Create(CMatchFinderMt *p, UInt32 historySize, UInt32 hashSize, UInt32 keepAddBufferBefore,
    UInt32 matchMaxLen, UInt32 keepAddBufferAfter, ISzAlloc *alloc)
{
  CMatchFinder *mf = p->MatchFinder;
  p->historySize = historySize;
  if (kMtBtBlockSize <= matchMaxLen * 4)
    return SZ_ERROR_PARAM;
  if (p->hashBuf == 0)
  {
    p->hashBuf = (UInt32 *)alloc->Alloc(alloc, (kHashBufferSize + kBtBufferSize) * sizeof(UInt32));
    if (p->hashBuf == 0)
      return SZ_ERROR_MEM;
    p->btBuf = p->hashBuf + kHashBufferSize;
  }
  keepAddBufferBefore += (kHashBufferSize + kBtBufferSize);
  keepAddBufferAfter += kMtHashBlockSize;
  if (!MatchFinder_Create(mf, historySize, hashSize, keepAddBufferBefore, matchMaxLen, keepAddBufferAfter, alloc))
    return SZ_ERROR_MEM;

  RINOK(MtSync_Create(&p->hashSync, HashThreadFunc2, p, kMtHashNumBlocks));
  RINOK(MtSync_Create(&p->btSync, BtThreadFunc2, p, kMtBtNumBlocks));
  return SZ_OK;
}

/* Call it after ReleaseStream / SetStream */
void MatchFinderMt_Init(CMatchFinderMt *p)
{
  CMatchFinder *mf = p->MatchFinder;
  p->btBufPos = p->btBufPosLimit = 0;
  p->hashBufPos = p->hashBufPosLimit = 0;
  MatchFinder_Init(mf);
  p->pointerToCurPos = MatchFinder_GetPointerToCurrentPos(mf);
  p->btNumAvailBytes = 0;
  p->lzPos = p->historySize + 1;

  p->hash = mf->hash;
  p->fixedHashSize = mf->fixedHashSize;
  p->crc = mf->crc;

  p->son = mf->son;
  p->matchMaxLen = mf->matchMaxLen;
  p->numHashBytes = mf->numHashBytes;
  p->pos = mf->pos;
  p->buffer = mf->buffer;
  p->cyclicBufferPos = mf->cyclicBufferPos;
  p->cyclicBufferSize = mf->cyclicBufferSize;
  p->cutValue = mf->cutValue;
}

/* ReleaseStream is required to finish multithreading */
void MatchFinderMt_ReleaseStream(CMatchFinderMt *p)
{
  MtSync_StopWriting(&p->btSync);
  /* p->MatchFinder->ReleaseStream(); */
}

void MatchFinderMt_Normalize(CMatchFinderMt *p)
{
  UInt32 subValue = (p->lzPos - p->historySize - 1);
  MatchFinder_Normalize3(p->MatchFinder, subValue, p->hash, p->fixedHashSize, MF_BinaryTree);
  p->lzPos -= subValue;
}

void MatchFinderMt_GetNextBlock_Bt(CMatchFinderMt *p)
{
  UInt32 blockIndex;
  MtSync_GetNextBlock(&p->btSync);
  blockIndex = ((p->btSync.numProcessedBlocks - 1) & kMtBtNumBlocksMask);
  p->btBufPosLimit = p->btBufPos = blockIndex * kMtBtBlockSize;
  p->btBufPosLimit += p->btBuf[p->btBufPos++];
  p->btNumAvailBytes = p->btBuf[p->btBufPos++];
  if (p->lzPos >= kMtMaxValForNormalize(p->MatchFinder) - kMtBtBlockSize)
    MatchFinderMt_Normalize(p);
}

const Byte * MatchFinderMt_GetPointerToCurrentPos(CMatchFinderMt *p)
{
  return p->pointerToCurPos;
}

#define GET_NEXT_BLOCK_IF_REQUIRED if (p->btBufPos == p->btBufPosLimit) MatchFinderMt_GetNextBlock_Bt(p);

UInt32 MatchFinderMt_GetNumAvailableBytes(CMatchFinderMt *p)
{
  GET_NEXT_BLOCK_IF_REQUIRED;
  return p->btNumAvailBytes;
}

Byte MatchFinderMt_GetIndexByte(CMatchFinderMt *p, Int32 index)
{
  return p->pointerToCurPos[index];
}

UInt32 * MixMatches2(CMatchFinderMt *p, UInt32 matchMinPos, UInt32 *distances)
{
  UInt32 hash2Value, curMatch2;
  UInt32 *hash = p->hash;
  const Byte *cur = p->pointerToCurPos;
  UInt32 lzPos = p->lzPos;
  MT_HASH2_CALC

  curMatch2 = hash[hash2Value];
  hash[hash2Value] = lzPos;

  if (curMatch2 >= matchMinPos)
    if (cur[(ptrdiff_t)curMatch2 - lzPos] == cur[0])
    {
      *distances++ = 2;
      *distances++ = lzPos - curMatch2 - 1;
    }
  return distances;
}

UInt32 * MixMatches3(CMatchFinderMt *p, UInt32 matchMinPos, UInt32 *distances)
{
  UInt32 hash2Value, hash3Value, curMatch2, curMatch3;
  UInt32 *hash = p->hash;
  const Byte *cur = p->pointerToCurPos;
  UInt32 lzPos = p->lzPos;
  MT_HASH3_CALC

  curMatch2 = hash[                hash2Value];
  curMatch3 = hash[kFix3HashSize + hash3Value];

  hash[                hash2Value] =
  hash[kFix3HashSize + hash3Value] =
    lzPos;

  if (curMatch2 >= matchMinPos && cur[(ptrdiff_t)curMatch2 - lzPos] == cur[0])
  {
    distances[1] = lzPos - curMatch2 - 1;
    if (cur[(ptrdiff_t)curMatch2 - lzPos + 2] == cur[2])
    {
      distances[0] = 3;
      return distances + 2;
    }
    distances[0] = 2;
    distances += 2;
  }
  if (curMatch3 >= matchMinPos && cur[(ptrdiff_t)curMatch3 - lzPos] == cur[0])
  {
    *distances++ = 3;
    *distances++ = lzPos - curMatch3 - 1;
  }
  return distances;
}

/*
UInt32 *MixMatches4(CMatchFinderMt *p, UInt32 matchMinPos, UInt32 *distances)
{
  UInt32 hash2Value, hash3Value, hash4Value, curMatch2, curMatch3, curMatch4;
  UInt32 *hash = p->hash;
  const Byte *cur = p->pointerToCurPos;
  UInt32 lzPos = p->lzPos;
  MT_HASH4_CALC

  curMatch2 = hash[                hash2Value];
  curMatch3 = hash[kFix3HashSize + hash3Value];
  curMatch4 = hash[kFix4HashSize + hash4Value];

  hash[                hash2Value] =
  hash[kFix3HashSize + hash3Value] =
  hash[kFix4HashSize + hash4Value] =
    lzPos;

  if (curMatch2 >= matchMinPos && cur[(ptrdiff_t)curMatch2 - lzPos] == cur[0])
  {
    distances[1] = lzPos - curMatch2 - 1;
    if (cur[(ptrdiff_t)curMatch2 - lzPos + 2] == cur[2])
    {
      distances[0] =  (cur[(ptrdiff_t)curMatch2 - lzPos + 3] == cur[3]) ? 4 : 3;
      return distances + 2;
    }
    distances[0] = 2;
    distances += 2;
  }
  if (curMatch3 >= matchMinPos && cur[(ptrdiff_t)curMatch3 - lzPos] == cur[0])
  {
    distances[1] = lzPos - curMatch3 - 1;
    if (cur[(ptrdiff_t)curMatch3 - lzPos + 3] == cur[3])
    {
      distances[0] = 4;
      return distances + 2;
    }
    distances[0] = 3;
    distances += 2;
  }

  if (curMatch4 >= matchMinPos)
    if (
      cur[(ptrdiff_t)curMatch4 - lzPos] == cur[0] &&
      cur[(ptrdiff_t)curMatch4 - lzPos + 3] == cur[3]
      )
    {
      *distances++ = 4;
      *distances++ = lzPos - curMatch4 - 1;
    }
  return distances;
}
*/

#define INCREASE_LZ_POS p->lzPos++; p->pointerToCurPos++;

UInt32 MatchFinderMt2_GetMatches(CMatchFinderMt *p, UInt32 *distances)
{
  const UInt32 *btBuf = p->btBuf + p->btBufPos;
  UInt32 len = *btBuf++;
  p->btBufPos += 1 + len;
  p->btNumAvailBytes--;
  {
    UInt32 i;
    for (i = 0; i < len; i += 2)
    {
      *distances++ = *btBuf++;
      *distances++ = *btBuf++;
    }
  }
  INCREASE_LZ_POS
  return len;
}

UInt32 MatchFinderMt_GetMatches(CMatchFinderMt *p, UInt32 *distances)
{
  const UInt32 *btBuf = p->btBuf + p->btBufPos;
  UInt32 len = *btBuf++;
  p->btBufPos += 1 + len;

  if (len == 0)
  {
    if (p->btNumAvailBytes-- >= 4)
      len = (UInt32)(p->MixMatchesFunc(p, p->lzPos - p->historySize, distances) - (distances));
  }
  else
  {
    /* Condition: there are matches in btBuf with length < p->numHashBytes */
    UInt32 *distances2;
    p->btNumAvailBytes--;
    distances2 = p->MixMatchesFunc(p, p->lzPos - btBuf[1], distances);
    do
    {
      *distances2++ = *btBuf++;
      *distances2++ = *btBuf++;
    }
    while ((len -= 2) != 0);
    len  = (UInt32)(distances2 - (distances));
  }
  INCREASE_LZ_POS
  return len;
}

#define SKIP_HEADER2  do { GET_NEXT_BLOCK_IF_REQUIRED
#define SKIP_HEADER(n) SKIP_HEADER2 if (p->btNumAvailBytes-- >= (n)) { const Byte *cur = p->pointerToCurPos; UInt32 *hash = p->hash;
#define SKIP_FOOTER } INCREASE_LZ_POS p->btBufPos += p->btBuf[p->btBufPos] + 1; } while (--num != 0);

void MatchFinderMt0_Skip(CMatchFinderMt *p, UInt32 num)
{
  SKIP_HEADER2 { p->btNumAvailBytes--;
  SKIP_FOOTER
}

void MatchFinderMt2_Skip(CMatchFinderMt *p, UInt32 num)
{
  SKIP_HEADER(2)
      UInt32 hash2Value;
      MT_HASH2_CALC
      hash[hash2Value] = p->lzPos;
  SKIP_FOOTER
}

void MatchFinderMt3_Skip(CMatchFinderMt *p, UInt32 num)
{
  SKIP_HEADER(3)
      UInt32 hash2Value, hash3Value;
      MT_HASH3_CALC
      hash[kFix3HashSize + hash3Value] =
      hash[                hash2Value] =
        p->lzPos;
  SKIP_FOOTER
}

/*
void MatchFinderMt4_Skip(CMatchFinderMt *p, UInt32 num)
{
  SKIP_HEADER(4)
      UInt32 hash2Value, hash3Value, hash4Value;
      MT_HASH4_CALC
      hash[kFix4HashSize + hash4Value] =
      hash[kFix3HashSize + hash3Value] =
      hash[                hash2Value] =
        p->lzPos;
  SKIP_FOOTER
}
*/

void MatchFinderMt_CreateVTable(CMatchFinderMt *p, IMatchFinder *vTable)
{
  vTable->Init = (Mf_Init_Func)MatchFinderMt_Init;
  vTable->GetIndexByte = (Mf_GetIndexByte_Func)MatchFinderMt_GetIndexByte;
  vTable->GetNumAvailableBytes = (Mf_GetNumAvailableBytes_Func)MatchFinderMt_GetNumAvailableBytes;
  vTable->GetPointerToCurrentPos = (Mf_GetPointerToCurrentPos_Func)MatchFinderMt_GetPointerToCurrentPos;
  vTable->GetMatches = (Mf_GetMatches_Func)MatchFinderMt_GetMatches;
  switch(p->MatchFinder->numHashBytes)
  {
    case 2:
      p->GetHeadsFunc = GetHeads2;
      p->MixMatchesFunc = (Mf_Mix_Matches)0;
      vTable->Skip = (Mf_Skip_Func)MatchFinderMt0_Skip;
      vTable->GetMatches = (Mf_GetMatches_Func)MatchFinderMt2_GetMatches;
      break;
    case 3:
      p->GetHeadsFunc = GetHeads3;
      p->MixMatchesFunc = (Mf_Mix_Matches)MixMatches2;
      vTable->Skip = (Mf_Skip_Func)MatchFinderMt2_Skip;
      break;
    default:
    /* case 4: */
      p->GetHeadsFunc = p->MatchFinder->btMode == MF_HashTable? GetHeadsHt4
                      : p->MatchFinder->bigHash               ? GetHeads4b
                                                              : GetHeads4;
      /* p->GetHeadsFunc = GetHeads4; */
      p->MixMatchesFunc = (Mf_Mix_Matches)MixMatches3;
      vTable->Skip = (Mf_Skip_Func)MatchFinderMt3_Skip;
      break;
    /*
    default:
      p->GetHeadsFunc = GetHeads5;
      p->MixMatchesFunc = (Mf_Mix_Matches)MixMatches4;
      vTable->Skip = (Mf_Skip_Func)MatchFinderMt4_Skip;
      break;
    */
  }
}
