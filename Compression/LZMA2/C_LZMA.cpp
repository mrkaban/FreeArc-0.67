// C_LZMA.cpp - интерфейс FreeArc к алгоритму сжатия LZMA

#ifdef WIN32
#include <windows.h>
#include <initguid.h>
#else
#define INITGUID
#endif

// Enable multithreading support
#define COMPRESS_MF_MT

// Match finder classes
#define MF_HashChain  0
#define MF_BinaryTree 1
#define MF_HashTable  2

extern "C" {
#include "C_LZMA.h"
}

enum
{
  kBT2,
  kBT3,
  kBT4,
  kHC4,
  kHT4
};

static const char *kMatchFinderIDs[] =
{
  "BT2",
  "BT3",
  "BT4",
  "HC4",
  "HT4"
};

static int FindMatchFinder(const char *s)
{
  for (int m = 0; m < (int)(sizeof(kMatchFinderIDs) / sizeof(kMatchFinderIDs[0])); m++)
    if (!strcasecmp(kMatchFinderIDs[m], s))
      return m;
  return -1;
}

// Включим в один .o файл все необходимые подпрограммы
#include "C/LzmaDec.c"
#undef kNumFullDistances
#ifndef FREEARC_DECOMPRESS_ONLY
namespace LzmaEncoder {
#include "C/LzFind.c"
#include "C/LzmaEnc.c"
#undef SKIP_HEADER
#undef SKIP_FOOTER
#ifdef COMPRESS_MF_MT
#include "C/Threads.c"
#include "C/LzFindMt.c"
#endif
}
using namespace LzmaEncoder;
#endif

static void *SzAlloc(void *p, size_t size) { p = p; return BigAlloc(size); }
static void SzFree(void *p, void *address) { p = p; BigFree(address); }
static ISzAlloc g_Alloc = { SzAlloc, SzFree };

int SRes_to_FreeArc (SRes res)
{
  if (res == SZ_ERROR_DATA)         return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
  if (res == SZ_ERROR_MEM)          return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  if (res == SZ_ERROR_UNSUPPORTED)  return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
  if (res == SZ_ERROR_PARAM)        return FREEARC_ERRCODE_INVALID_COMPRESSOR;
  if (res == SZ_ERROR_INPUT_EOF)    return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
  if (res == SZ_ERROR_OUTPUT_EOF)   return FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL;
  if (res == SZ_ERROR_READ)         return FREEARC_ERRCODE_READ;
  if (res == SZ_ERROR_WRITE)        return FREEARC_ERRCODE_WRITE;
  if (res != SZ_OK)
    //fprintf(stderr, "\nEncoder error = %X\n", (unsigned int)res);
    return FREEARC_ERRCODE_GENERAL;
  return FREEARC_OK;
}


#ifndef FREEARC_DECOMPRESS_ONLY

typedef struct
{
  SRes (*Read)(void *p, void *buf, size_t *size);
    /* if (input(*size) != 0 && output(*size) == 0) means end_of_stream.
       (output(*size) < input(*size)) is allowed */
  CALLBACK_FUNC *callback;
  void *auxdata;
  int errcode;
  UInt64 total;
  bool first_read;
} CallbackInStream;

typedef struct
{
  size_t (*Write)(void *p, const void *buf, size_t size);
    /* Returns: result - the number of actually written bytes.
       (result < size) means error */
  CALLBACK_FUNC *callback;
  void *auxdata;
  int errcode;
  UInt64 total;
} CallbackOutStream;

typedef struct
{
  SRes (*Progress)(void *p, UInt64 inSize, UInt64 outSize);
    /* Returns: result. (result != SZ_OK) means break.
       Value (UInt64)(Int64)-1 for size means unknown value. */
  CALLBACK_FUNC *callback;
  void *auxdata;
  int errcode;
  UInt64 lastInSize, lastOutSize;
} CompressProgress;


SRes CallbackRead (void *p, void *buf, size_t *size)
{
  CallbackInStream *s = (CallbackInStream*) p;
  // Read data by the BUFFER_SIZE chunks
  SRes res  =  s->callback ("read", buf, mymin(*size, BUFFER_SIZE), s->auxdata);
  if (res >= 0)  {*size = res; s->total  += res; return SZ_OK;}
  else           {*size = 0;   s->errcode = res; return res;}
}

size_t CallbackWrite (void *p, const void *buf, size_t size)
{
  CallbackOutStream *s = (CallbackOutStream*) p;
  int res = s->callback ("write", (void*)buf, size, s->auxdata);
  if (res >= 0)  {s->total  += res; return size;}
  else           {s->errcode = res; return 0;}
}

SRes CallbackProgress (void *p, UInt64 inSize, UInt64 outSize)
{
  CompressProgress *s = (CompressProgress*) p;
  UInt64 sizes[2] = {inSize - s->lastInSize, outSize - s->lastOutSize};
  s->lastInSize = inSize;  s->lastOutSize = outSize;
  int res = s->callback ("progress", sizes, 0, s->auxdata);
  if (res==FREEARC_OK || res==FREEARC_ERRCODE_NOT_IMPLEMENTED)
        return SZ_OK;
  else {s->errcode = res; return SZ_ERROR_PROGRESS;}
}


int lzma_compress2  (int dictionarySize,
                     int hashSize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     void *input, int inputSize, void *output, int *outputSize,
                     CALLBACK_FUNC *callback,
                     void *auxdata,
                     void **CodecState)
{
  // NULL input & callback is a special empty call just to release LzmaEnc handle stored in the *CodecState
  if (!input && !callback)   {LzmaEnc_Destroy(*CodecState, &g_Alloc, &g_Alloc);  *CodecState=NULL;  return FREEARC_OK;}

  CallbackInStream  inStream;    inStream.Read  = CallbackRead;      inStream.callback = callback;   inStream.auxdata = auxdata;   inStream.errcode = FREEARC_OK;   inStream.total     = 0;  inStream.first_read = TRUE;
  CallbackOutStream outStream;  outStream.Write = CallbackWrite;    outStream.callback = callback;  outStream.auxdata = auxdata;  outStream.errcode = FREEARC_OK;  outStream.total     = 0;
  CompressProgress  progress; progress.Progress = CallbackProgress;  progress.callback = callback;   progress.auxdata = auxdata;   progress.errcode = FREEARC_OK;  progress.lastInSize = 0;  progress.lastOutSize = 0;

  CLzmaEncHandle enc;
  SRes res = SZ_OK;
  const int writeEndMark = 1;

  // Create LzmaEnc handle if one isn't yet stored at *CodecState
  if (CodecState && *CodecState)
  {
    enc = *CodecState;
  }
  else
  {
    CLzmaEncProps props;
    LzmaEncProps_Init(&props);
    props.dictSize = dictionarySize;
    props.mc = matchFinderCycles;
    props.lc = litContextBits;
    props.lp = litPosBits;
    props.pb = posStateBits;
    props.algo = algorithm;
    props.fb = numFastBytes;
    props.hashSize = hashSize;
    switch (matchFinder)
    {
      case kHC4:  props.btMode = MF_HashChain ;  props.numHashBytes = 4; break;
      case kBT2:  props.btMode = MF_BinaryTree;  props.numHashBytes = 2; break;
      case kBT3:  props.btMode = MF_BinaryTree;  props.numHashBytes = 3; break;
      case kBT4:  props.btMode = MF_BinaryTree;  props.numHashBytes = 4; break;
      case kHT4:  props.btMode = MF_HashTable;   props.numHashBytes = 4; break;
    }
    props.numThreads = GetCompressionThreads();
    props.writeEndMark = writeEndMark;
    LzmaEncProps_Normalize(&props);

    enc = LzmaEnc_Create(&g_Alloc);
    if (enc == 0)   return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
    res = LzmaEnc_SetProps(enc, &props);
  }

  // MAIN CODE: COMPRESSION EITHER FROM BUF TO BUF OR USING CALLBACKS FOR I/O
  if (res == SZ_OK)
  {
    if (input && output && outputSize) {
      SizeT outsize = *outputSize;
      res = LzmaEnc_MemEncode(enc, (Byte*)output, &outsize, (const Byte*)input, inputSize, writeEndMark, (ICompressProgress*)&progress, &g_Alloc, &g_Alloc);
      *outputSize = outsize;
      CallbackProgress (&progress, inputSize, *outputSize);            // report bytes compressed after the last call to progress.Progress
    } else {
      res = LzmaEnc_Encode(enc, (ISeqOutStream*)&outStream, (ISeqInStream*)&inStream, (ICompressProgress*)&progress, &g_Alloc, &g_Alloc);
      CallbackProgress (&progress, inStream.total, outStream.total);   // report bytes compressed after the last call to progress.Progress
    }
  }

  // Store or destroy LzmaEnc, depending on CodecState!=NULL
  if (CodecState)
  {
    *CodecState = enc;
  }
  else
  {
    LzmaEnc_Destroy(enc, &g_Alloc, &g_Alloc);
  }

  // Вернуть код ошибки из колбэка или перекодировать код ошибки из 7z в fa
  if ( inStream.errcode != FREEARC_OK)    return  inStream.errcode;
  if (outStream.errcode != FREEARC_OK)    return outStream.errcode;
  if ( progress.errcode != FREEARC_OK)    return  progress.errcode;
  return SRes_to_FreeArc(res);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)



SRes LzmaDec_AllocateUsingProperties(CLzmaDec *p, const CLzmaProps propNew, ISzAlloc *alloc, bool Memory_to_memory_decompression)
{
  RINOK(LzmaDec_AllocateProbs2(p, &propNew, alloc));
  if (!Memory_to_memory_decompression)
  {
    SizeT dicBufSize = propNew.dicSize;
    if (p->dic == 0 || dicBufSize != p->dicBufSize)
    {
      LzmaDec_FreeDict(p, alloc);
      p->dic = (Byte *)alloc->Alloc(alloc, dicBufSize);
      if (p->dic == 0)
      {
        LzmaDec_FreeProbs(p, alloc);
        return SZ_ERROR_MEM;
      }
    }
    p->dicBufSize = dicBufSize;
  }
  p->prop = propNew;
  return SZ_OK;
}

// Input buffer size for decompressor
#define RANGE_DECODER_BUFFER_SIZE LARGE_BUFFER_SIZE


int lzma_decompress2(int dictionarySize,
                     int hashSize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     void *input, int inputSize, void *output, int *outputSize,
                     CALLBACK_FUNC *callback,
                     void *auxdata,
                     void **CodecState)
{
  bool Memory_to_memory_decompression = input && output && outputSize;
  int errcode = FREEARC_OK;  SRes res = SZ_OK;
  CLzmaDec *_state  =  CodecState?  (CLzmaDec*) *CodecState : NULL;

  // NULL input & callback is a special empty call just to release CLzmaDec state stored in the *CodecState
  if (!input && !callback)   {LzmaDec_Free(_state, &g_Alloc);  delete _state;  *CodecState=NULL;  return FREEARC_OK;}

  if (_state == NULL)
  {
    CLzmaProps LzmaProps;
    LzmaProps.pb = posStateBits;
    LzmaProps.lc = litContextBits;
    LzmaProps.lp = litPosBits;
    LzmaProps.dicSize = dictionarySize;

    _state = new CLzmaDec;
    LzmaDec_Construct(_state);
    SRes res = LzmaDec_AllocateUsingProperties(_state, LzmaProps, &g_Alloc, Memory_to_memory_decompression);
    if (res != SZ_OK)  {delete _state;  return SRes_to_FreeArc(res);}
  }
  LzmaDec_Init(_state);

  if (Memory_to_memory_decompression)
  {
    _state->dic = (Byte*) output;
    _state->dicBufSize = *outputSize;

    ELzmaStatus status;
    SizeT insize = inputSize;
    res = LzmaDec_DecodeToDic(_state, *outputSize, (const Byte*) input, &insize, LZMA_FINISH_ANY, &status);

    if (res == SZ_OK && status == LZMA_STATUS_NEEDS_MORE_INPUT)
      res = SZ_ERROR_INPUT_EOF;

    *outputSize = _state->dicPos;
    _state->dic = NULL;
  }
  else  // Stream-to-stream decompression
  {
    Byte  *_inBuf;  BIGALLOC (Byte, _inBuf, RANGE_DECODER_BUFFER_SIZE)
    for (UInt32 _inPos = 0, _inSize = 0;;)
    {
      if (_inPos == _inSize) {
        READ_LEN (_inSize, _inBuf, RANGE_DECODER_BUFFER_SIZE);                      // Read compressed data in 256kb chunks
        _inPos = 0;              // current position inside input data
      }

      SizeT oldDicPos = _state->dicPos;
      SizeT curSize = mymin (_state->dicBufSize - oldDicPos, HUGE_BUFFER_SIZE);     // Write decompressed data in 8mb chunks at most

      SizeT inSizeProcessed = _inSize - _inPos;
      ELzmaStatus status;
      res = LzmaDec_DecodeToDic (_state, oldDicPos + curSize, _inBuf + _inPos, &inSizeProcessed, LZMA_FINISH_ANY, &status);

      _inPos += (UInt32)inSizeProcessed;
      SizeT outSizeProcessed = _state->dicPos - oldDicPos;

      WRITE (_state->dic+oldDicPos, _state->dicPos-oldDicPos);
      PROGRESS (inSizeProcessed, outSizeProcessed);
      if (res != SZ_OK)  goto finished;

      // No more data to decompress
      if (inSizeProcessed==0 && outSizeProcessed==0)
        ReturnErrorCode(SRes_to_FreeArc(status==LZMA_STATUS_FINISHED_WITH_MARK? SZ_OK : SZ_ERROR_DATA));

      if (_state->dicPos == _state->dicBufSize)
        _state->dicPos = 0;
    }
finished:
    BigFree(_inBuf);
  }

  if (CodecState) {
    *CodecState = _state;
  } else {
    LzmaDec_Free(_state, &g_Alloc);
    delete _state;
  }
  return (res!=SZ_OK? SRes_to_FreeArc(res) : errcode);
}


/*-------------------------------------------------*/
/* Реализация класса LZMA_METHOD                  */
/*-------------------------------------------------*/

// Если строка str начинается со start, то возвратить адрес остатка, иначе - NULL
char* start_from (char* str, char* start)
{
  while (*start && *str==*start)  str++, start++;
  return *start? NULL : str;
}

// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
LZMA_METHOD::LZMA_METHOD()
{
  dictionarySize    = 64*mb;
  hashSize          = 0;
  algorithm         = 1;
  numFastBytes      = 32;
  matchFinder       = kHT4;
  matchFinderCycles = 0;    // библиотека LZMA определит количество циклов автоматически, исходя из matchFinder и numFastBytes
  posStateBits      = 2;
  litContextBits    = 3;
  litPosBits        = 0;
}

// Универсальный метод, отвечает на запрос "has_progress?"
int LZMA_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
    if (strequ (what, "has_progress?"))  return 1;               // Да, этот алгоритм поддерживает отчёт о прогрессе упаковки
    else                                 return COMPRESSION_METHOD::doit (what, param, data, callback);  // Передать остальные вызовы родительской процедуре
}

// Упаковка/распаковка в памяти
int LZMA_METHOD::DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback, void *auxdata, void **CodecState)
{
#ifndef FREEARC_DECOMPRESS_ONLY
  // Use faster lzma_compress() from DLL if possible
  static FARPROC c = LoadFromDLL ("lzma_compress2");
  if (!c) c = (FARPROC) lzma_compress2;

  if (direction==COMPRESS) {
    SetDictionary (dictionarySize);   // Ограничим размер словаря чтобы сжатие влезало в 4гб памяти :)
    // Если LZMA будет использовать multithreading алгоритм,
    // то нет смысла считать время работы по основному треду - вместо этого
    // следует использовать wall clock time всего процесса упаковки
    if (algorithm && GetCompressionThreads()>1)
        addtime = -1;   // это сигнал на использование wall clock time
  }
#else
  static FARPROC c = NULL;
  if (direction==COMPRESS)  return FREEARC_ERRCODE_ONLY_DECOMPRESS;
#endif

  // Use faster lzma_decompress() from DLL if possible
  static FARPROC d = LoadFromDLL ("lzma_decompress2");
  if (!d) d = (FARPROC) lzma_decompress2;

  return ((int (*)(int, int, int, int, int, int, int, int, int, void*, int, void*, int*, CALLBACK_FUNC*, void*, void**)) (direction==COMPRESS?c:d))
                         (dictionarySize,
                          hashSize,
                          algorithm,
                          numFastBytes,
                          matchFinder,
                          matchFinderCycles,
                          posStateBits,
                          litContextBits,
                          litPosBits,
                          input, inputSize, output, outputSize,
                          callback,
                          auxdata,
                          CodecState);
}

// Функция распаковки
int LZMA_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return DeCompressMem (DECOMPRESS, NULL, 0, NULL, NULL, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int LZMA_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return DeCompressMem (COMPRESS, NULL, 0, NULL, NULL, callback, auxdata);
}

// Посчитать, сколько памяти требуется для упаковки заданным методом
MemSize LZMA_METHOD::GetCompressionMem (void)
{
  MemSize mem;
  CalcCompressionMemories (&mem, NULL);
  return mem;
}

// Extra 6 mb are required for MT compressor (4.25mb for MT bufs plus 1.7mb added to the dictionary unless in compress_all_at_once mode)
static MemSize mtbuf (int algo, int threads)  {return  (algo && (threads!=1)?  (compress_all_at_once? 4*mb : 6*mb) : 0);}

// Вычисляет общий расход памяти и размер хеш-таблицы при упаковке
void LZMA_METHOD::CalcCompressionMemories (MemSize *pmem, MemSize *phashSize)
{
  CLzmaEncProps props;
  LzmaEncProps_Init(&props);
  props.dictSize = dictionarySize;
  props.mc = matchFinderCycles;
  props.lc = litContextBits;
  props.lp = litPosBits;
  props.pb = posStateBits;
  props.algo = algorithm;
  props.fb = numFastBytes;
  props.hashSize = hashSize;
  switch (matchFinder)
  {
    case kHC4:  props.btMode = MF_HashChain ;  props.numHashBytes = 4; break;
    case kBT2:  props.btMode = MF_BinaryTree;  props.numHashBytes = 2; break;
    case kBT3:  props.btMode = MF_BinaryTree;  props.numHashBytes = 3; break;
    case kBT4:  props.btMode = MF_BinaryTree;  props.numHashBytes = 4; break;
    case kHT4:  props.btMode = MF_HashTable;   props.numHashBytes = 4; break;
  }
  props.numThreads = GetCompressionThreads();
  props.writeEndMark = 1;
  LzmaEncProps_Normalize(&props);

  // Дополнительная область словаря, после заполнения которой происходит его сдвиг
  LongMemSize sizeReserv  =  matchFinder==kHT4?  (props.dictSize <= 768*mb           ?  props.dictSize/4  :  props.dictSize/8)
                                              :  (props.dictSize <= ((UInt32)2 << 30)?  props.dictSize/2  :  props.dictSize/4);
  // Количество ссылок в хеш-таблице на каждый байт входных данных
  LongMemSize sons  =  (matchFinder==kHT4? 0 : matchFinder==kHC4? 1 : 2);
  // Память для хеш-таблиц, структур упаковщика и выходных данных
  LongMemSize hash_memory  =  props.hashSize + sons*sizeof(CLzRef)*props.dictSize + RC_BUF_SIZE(props.dictSize) + 768*kb + mtbuf(props.algo,props.numThreads);
  // Память для входных данных (не выделяется при memory-to-memory compression)
  LongMemSize buf_memory  =  LongMemSize(props.dictSize) + sizeReserv + 1*mb;

  // Общий расход памяти (возращаем значение не более 4gb-1)
  if (pmem)  *pmem  =  MemSize (mymin (MEMSIZE_MAX, hash_memory + (compress_all_at_once? 0 : buf_memory)));

  // Размер хеш-таблицы
  if (phashSize)  *phashSize = props.hashSize;
}

// Вычисляет словарь, использующий не более mem памяти для сжатия заданным LZMA_METHOD. Метод не очень точен - он не учитывает тонкости
//   расчёта sizeReserv, округление props.hashSize/k вниз до степени 2 и другие детали (см. CalcCompressionMemories)
MemSize calcDictSize (LZMA_METHOD *p, MemSize oldDictionarySize, MemSize mem)
{
  double mem4 = mymax (double(mem) - p->hashSize - (compress_all_at_once? 0:1*mb) - mtbuf(p->algorithm,GetCompressionThreads()) - 1*mb, 0);
  double buf_k = compress_all_at_once? 0 : (p->matchFinder==kHT4? 1.25:1.5);
  switch (p->matchFinder) {
    case kBT2:    buf_k += 8;                    break;
    case kBT3:    buf_k += (p->hashSize? 8:10);  break;
    case kBT4:    buf_k += (p->hashSize? 8:10);  break;
    case kHC4:    buf_k += (p->hashSize? 4:6);   break;
    case kHT4:    buf_k += (p->hashSize? 0:1);   break;
  }
  return  (buf_k?  (MemSize)floor(mem4/buf_k)  :  oldDictionarySize);
}

// Ограничить использование памяти при упаковке
void LZMA_METHOD::SetCompressionMem (MemSize mem)
{
  if (mem<=0)  return;
  if (compress_all_at_once && matchFinder==kHT4) {
    hashSize = MemSize(floor(mymax(double(mem/mb)-mtbuf(algorithm,GetCompressionThreads())/mb-1,1)))*mb;   // Use all memory for hash because original and compressed data are held in memory buffers provided by caller
  } else {
    hashSize = 0;   // otherwise large hash may leave too little memory for the dictionary (to do: make a proportional cutoff and take into account rounding of hash in HT4 method)
    SetDictionary(calcDictSize(this,dictionarySize,mem));
  }
}

// Ограничить использование памяти при распаковке
void LZMA_METHOD::SetMinDecompressionMem (MemSize mem)
{
  if (mem<=0)  return;
  SetDictionary (mem);
}

// Установить размер словаря.
void LZMA_METHOD::SetDictionary (MemSize mem)
{
  if (mem<=0)  return;
  dictionarySize = mem;
#ifndef FREEARC_64BIT
  // Ограничим размер словаря чтобы сжатие влезало в 4гб памяти :)
  dictionarySize = mymin (dictionarySize, rounddown_mem (calcDictSize (this, dictionarySize, UINT_MAX)));
#endif
  // Словарь - минимум 32 кб, округлённый до килобайт/мегабайт
  dictionarySize = rounddown_mem (mymax (32*kb, dictionarySize));
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

MemSize LZMA_METHOD::GetDecompressionMem (void)
{
  // В режиме memory-to-memory оба буфера предоставляются извне
  return compress_all_at_once? 1*mb : dictionarySize+RANGE_DECODER_BUFFER_SIZE;
}


// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_LZMA)
void LZMA_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
  char DictionaryStr[100], HashStr[100], fcStr[100], pbStr[100], lcStr[100], lpStr[100], algStr[100];
  showMem (dictionarySize, DictionaryStr);

  MemSize hs = hashSize;
#ifndef FREEARC_DECOMPRESS_ONLY
  // Вычислим реальный размер хеш-таблицы (он может быть меньше hashSize из-за того, что каждый bucket должен содержать 2^n элементов)
  if (hashSize)  CalcCompressionMemories (NULL, &hs);
#endif
  showMem (hs, HashStr);

  LZMA_METHOD defaults;
  sprintf (fcStr, matchFinderCycles!=defaults.matchFinderCycles? ":mc%d" : "", matchFinderCycles);
  sprintf (pbStr, posStateBits     !=defaults.posStateBits     ? ":pb%d" : "", posStateBits);
  sprintf (lcStr, litContextBits   !=defaults.litContextBits   ? ":lc%d" : "", litContextBits);
  sprintf (lpStr, litPosBits       !=defaults.litPosBits       ? ":lp%d" : "", litPosBits);
  matchFinder==kHT4? (algorithm==2? sprintf (algStr, "a%d", algorithm)
                                  : sprintf (algStr, algorithm==0? "fast" : "normal"))
                   : sprintf (algStr, "%s:%s", algorithm==0? "fast": algorithm==1? "normal": "max", kMatchFinderIDs [matchFinder]);
  for (char *p=algStr; *p; p++)   *p = tolower(*p);  // strlwr(algStr);
  sprintf (buf, "lzma:%s%s%s:%s:%d%s%s%s%s",
                      DictionaryStr,
                      hashSize? ":h" : "",
                      hashSize? HashStr : "",
                      algStr,
                      numFastBytes,
                      fcStr,
                      pbStr,
                      lcStr,
                      lpStr);
}

// Конструирует объект типа LZMA_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_LZMA (char** parameters)
{
  if (strcmp (parameters[0], "lzma") == 0) {
    // Если название метода (нулевой параметр) - "lzma", то разберём остальные параметры

    LZMA_METHOD *p = new LZMA_METHOD;
    p->matchFinder = INT_MAX;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    // Переберём все параметры метода (или выйдем раньше при возникновении ошибки при разборе очередного параметра)
    while (*++parameters) {
      char *param = *parameters;  bool optional = param[0]=='*';  optional && param++;
           if (start_from (param, "d"))    p->dictionarySize    = parseMem (param+1, &error);
      else if (start_from (param, "h"))   {MemSize h            = parseMem (param+1, &error);
                                           if (error)  goto unnamed;  else p->hashSize = h;}
      else if (start_from (param, "a"))    p->algorithm         = parseInt (param+1, &error);
      else if (start_from (param, "fb"))   p->numFastBytes      = parseInt (param+2, &error);
      else if (start_from (param, "mc"))   p->matchFinderCycles = parseInt (param+2, &error);
      else if (start_from (param, "lc"))   p->litContextBits    = parseInt (param+2, &error);
      else if (start_from (param, "lp"))   p->litPosBits        = parseInt (param+2, &error);
      else if (start_from (param, "pb"))   p->posStateBits      = parseInt (param+2, &error);
      else if (start_from (param, "mf"))   p->matchFinder       = FindMatchFinder (param[2]=='='? param+3 : param+2);
      else if (strequ (param, "fastest"))  p->algorithm = 0,  p->matchFinder==INT_MAX && (p->matchFinder = kHT4),  p->numFastBytes = 5,    p->matchFinderCycles = 1;
      else if (strequ (param, "fast"))     p->algorithm = 0,  p->matchFinder==INT_MAX && (p->matchFinder = kHT4),  p->numFastBytes = 32,   p->matchFinderCycles = 0;
      else if (strequ (param, "normal"))   p->algorithm = 1,  p->matchFinder==INT_MAX && (p->matchFinder = kHT4),  p->numFastBytes = 32,   p->matchFinderCycles = 0;
      else if (strequ (param, "max"))      p->algorithm = 1,  p->matchFinder==INT_MAX && (p->matchFinder = kBT4),  p->numFastBytes = 128,  p->matchFinderCycles = 0;
      else if (strequ (param, "ultra"))    p->algorithm = 1,  p->matchFinder==INT_MAX && (p->matchFinder = kBT4),  p->numFastBytes = 128,  p->matchFinderCycles = 128;
      else {
unnamed:// Сюда мы попадаем, если в параметре опущено его наименование
        // Это может быть строка - имя MatchFinder'а, целое число - значение numFastBytes,
        // или обозначение памяти - значение dictionarySize
        error = 0;
        int n = FindMatchFinder (param);
        if (n>=0)
          p->matchFinder = n;
        else {
          n = parseInt (param, &error);
          if (!error)  p->numFastBytes = n;
          else         {error=0; MemSize n = parseMem (param, &error);
                        if (!error)  p->dictionarySize = n;}
        }
      }
      if (!optional)
        if (error || p->matchFinder<0)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    }
    if (p->matchFinder == INT_MAX)
      p->matchFinder = kHT4;   // default match finder
    return p;
  } else
    return NULL;   // Это не метод lzma
}

static int LZMA_x = AddCompressionMethod (parse_LZMA);   // Зарегистрируем парсер метода LZMA
