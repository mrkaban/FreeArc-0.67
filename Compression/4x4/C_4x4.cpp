// (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
// 4x4: multithreaded compression with overlapped I/O

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "C_4x4.h"
#include "../Compression.h"
#include "../MultiThreading.h"

#define _4x4_VERSION     0  /* version of compressed data format */
#define _4x4_HEADER_SIZE 8  /* size of compressed block header (two int32 values) */

// Размер одного входного/выходного буфера
MemSize _4x4_BUFFER_SIZE (COMPRESSION direction, COMPRESSION_METHOD *cm, MemSize BlockSize)
{
  return (direction==COMPRESS? BlockSize : (cm? cm->GetMaxCompressedSize(BlockSize):0)) + _4x4_HEADER_SIZE;
}


/*-------------------------------------------------*/
/* Multithreaded _4x4_compress/_4x4_decompress     */
/*-------------------------------------------------*/
struct _4x4MTCompressor;

// 4x4 compression job
struct _4x4Job : CompressionJob
{
    _4x4MTCompressor* Compressor;
    int               ReportedInSize;            // Number of in/out bytes processed already reported to host
    int               ReportedOutSize;           // -.-
};

// Multi-threaded 4x4 compressor
struct _4x4MTCompressor : MTCompressor <_4x4Job, void*>
{
    COMPRESSION         direction;               // Either COMPRESS or DECOMPRESS
    COMPRESSION_METHOD *cmethod;                 // Compression method used for every block
    CALLBACK_FUNC*      callback;                // I/O callback
    void*               auxdata;                 //   and his additional parameter
    int                 BlockSize;               // Size of chunks input split to
    double              MinOrder0Ratio;          // Minimal order0_compression_ratio() that allow us to don't try to compress the block at all

    // Copy compression parameters into class fields
    _4x4MTCompressor (COMPRESSION _direction, _4x4_METHOD *method, CALLBACK_FUNC *_callback, void *_auxdata)
        : MTCompressor<_4x4Job, void*> (method->GetNumThreads(), method->GetNumExtraBuffers())
    {
        direction = _direction;  callback = _callback;  auxdata = _auxdata;
        cmethod   =  ParseCompressionMethod (method->Method);
        BlockSize =  method->BlockSize || cmethod==NULL?  method->BlockSize  :  cmethod->GetDictionary();
        MinOrder0Ratio = method->MinOrder0Percents / 100.0;  // ratio = percents/100
    }

    ~_4x4MTCompressor()
    {
        delete cmethod;
    }

    int ReaderThread()
    {
        if (cmethod == NULL)  return FREEARC_ERRCODE_INVALID_COMPRESSOR;

        int errcode = FREEARC_OK;
        if (direction==COMPRESS)  {WRITE4(_4x4_VERSION); PROGRESS(0,4);}
        else                      {int version; READ4_OR_EOF(version); PROGRESS(4,0);   if (version != _4x4_VERSION)  return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;}

        // For compression, allocate buffers immediately
    	if (direction==COMPRESS)  {if ((errcode = AllocateBuffers(_4x4_BUFFER_SIZE(direction,cmethod,BlockSize)))  <  0)   return errcode;}

        // Read input until EOF or error, sending data to (de)compression jobs
        for(;;)
        {
            _4x4Job *job = FreeJobs.Get();                      // Acquire free job record
            if (job == NULL  ||  ErrCode)  break;               // Quit on error in other thread
            if (direction==COMPRESS) {
                job->InBuf = InputBuffers.Get();                // Acquire read buffer
                if (job->InBuf == NULL  ||  ErrCode)  break;    // Quit on error in other thread
                READ_LEN_OR_EOF (job->InSize, job->InBuf+_4x4_HEADER_SIZE, BlockSize);
            } else {
                READ4_OR_EOF (job->OutSize);
                READ4        (job->InSize);
                // Now BlockSize is known so we are going to allocate buffers (extra space is allocated since incompressible blocks may be inflated a bit)
                if (buf0==NULL)
                {
                    BlockSize = _4x4_BUFFER_SIZE (direction, cmethod, job->OutSize>0?job->OutSize:job->InSize);
                    if ((errcode = AllocateBuffers(BlockSize))  <  0)   return errcode;
                    BlockSize -= _4x4_HEADER_SIZE;
                }
                // We cannot handle blocks larger than the first one
                if (job->InSize > BlockSize  ||  job->OutSize > BlockSize)  return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
                job->InBuf = InputBuffers.Get();                // Acquire read buffer
                if (job->InBuf == NULL  ||  ErrCode)  break;    // Quit on error in other thread
                READ (job->InBuf, job->InSize);
            }
            if (ErrCode)  break;                                // Quit on error in other thread
            WorkerJobs.Put(job);
            WriterJobs.Put(job);
        }

        finished:  return errcode;
    }


    // CodecState initialization and destruction (see COMPRESSION_METHOD::DeCompressMem() description)
    virtual int  WorkerInit (void* &CodecState) {CodecState=NULL; return 0;}
    virtual void WorkerDone (void* &CodecState) {CodecState  &&  cmethod->DeCompressMem (direction, NULL, 0, NULL, NULL, NULL, NULL, &CodecState);}

    virtual void Process (_4x4Job &job, void* &CodecState);  // Perform one (de)compression operation
    virtual void Write   (_4x4Job &job);                     // Write output buffer and perform post-processing if required
};


// Compute compression ratio for order-0 byte-granular arithmetic coder
double order0_compression_ratio (void *buf, size_t bufsize)
{
  size_t count1[256] = {0};
  size_t count2[256] = {0};
  size_t count3[256] = {0};
  size_t count4[256] = {0};

  byte *p = (byte*) buf;  int i;
  for (i=0; i<bufsize-3; i+=4)
    count1[ p[i]   ]++,
    count2[ p[i+1] ]++,
    count3[ p[i+2] ]++,
    count4[ p[i+3] ]++;
  for (; i<bufsize; i++)
    count1[ p[i]   ]++;

  double compressed = 0;
  for (int i=0; i<256; i++)
  {
    size_t count  =  count1[i] + count2[i] + count3[i] + count4[i];
    if (count)
      compressed  +=  count * log2(double(bufsize)/count) / 8;
  }
  return compressed / bufsize;
}

// Pass through callback and count how many bytes was already reported to progress indicator (the rest will be reported once job outdata are written)
static int _4x4_callback (const char *what, void *data, int size, void *param)
{
    _4x4Job *job = (_4x4Job*) param;
    if (strequ(what,"progress"))
    {
        job->ReportedInSize  += ((int64*)data)[0];
        job->ReportedOutSize += ((int64*)data)[1];
    }
    return job->Compressor->callback (what, data, size, job->Compressor->auxdata);
}

// Perform one (de)compression operation
void _4x4MTCompressor::Process (_4x4Job &job, void* &CodecState)
{
    job.Compressor = this;  job.ReportedInSize = job.ReportedOutSize = 0;

    if (direction==COMPRESS) {
#ifndef FREEARC_DECOMPRESS_ONLY
        if (MinOrder0Ratio==0  ||  job.InSize < 32*kb  ||  order0_compression_ratio(job.InBuf+_4x4_HEADER_SIZE, job.InSize) < MinOrder0Ratio) {
          int OutputSize = job.InSize;
          job.result = cmethod->DeCompressMem (COMPRESS, job.InBuf+_4x4_HEADER_SIZE, job.InSize, job.OutBuf+_4x4_HEADER_SIZE, &OutputSize, _4x4_callback, &job, &CodecState);
          job.OutSize = OutputSize;
        } else {
          job.result = FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL;
        }
        if (job.result == FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL) {    // If we can't compress the block - just store it
            char *buf=job.InBuf; job.InBuf=job.OutBuf; job.OutBuf=buf;
            job.OutSize = job.InSize;
            job.result = FREEARC_OK;
            setvalue32 (job.OutBuf, uint32(-1));
        } else {
            setvalue32 (job.OutBuf, job.InSize);
        }
        setvalue32 (job.OutBuf+4, job.OutSize);
        job.OutSize += _4x4_HEADER_SIZE;
#else
        job.result = FREEARC_ERRCODE_ONLY_DECOMPRESS;
#endif
    } else {
        if (int32(job.OutSize) == -1) {                            // If this is stored block - just copy it intact
            char *buf=job.InBuf; job.InBuf=job.OutBuf; job.OutBuf=buf;
            job.OutSize = job.InSize;
            job.result = FREEARC_OK;
        } else {
            int OutputSize = job.OutSize;
            job.result = cmethod->DeCompressMem (DECOMPRESS, job.InBuf, job.InSize, job.OutBuf, &OutputSize, _4x4_callback, &job, &CodecState);
            //// check that OutputSize==job.OutSize
        }
    }
}

// Write output buffer and perform post-processing if required
void _4x4MTCompressor::Write (_4x4Job &job)
{
    int res = callback ("write", job.OutBuf, job.OutSize, auxdata);
    if (job.result >= 0)
    {
        job.result = res;
        PROGRESS((direction==COMPRESS? 0:_4x4_HEADER_SIZE) + job.InSize  - job.ReportedInSize,
                                                             job.OutSize - job.ReportedOutSize);
        //// check result
    }
}



/*-------------------------------------------------*/
/* Реализация класса _4x4_METHOD                   */
/*-------------------------------------------------*/
// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
_4x4_METHOD::_4x4_METHOD()
{
  strcpy(Method, "tor:3:1mb");
  NumThreads       =  0;
  NumExtraBuffers  = -1;
  BlockSize        =  0;
  MinOrder0Percents = 99;        // don't try to compress the data if order0 entropy > 99%
}

// Универсальный метод, отвечает на запрос "has_progress?"
int _4x4_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  if (strequ (what, "has_progress?"))  return 1;                                                       // Да, этот алгоритм поддерживает отчёт о прогрессе упаковки
  else                                 return COMPRESSION_METHOD::doit (what, param, data, callback);  // Передать остальные вызовы родительской процедуре
}

// Функция распаковки
int _4x4_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  Set_compress_all_at_once_Until_end_of_block _(1);
  _4x4MTCompressor Decompressor (DECOMPRESS, this, callback, auxdata);
  return Decompressor.Run();
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int _4x4_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  Set_compress_all_at_once_Until_end_of_block _(1);
  _4x4MTCompressor Compressor (COMPRESS, this, callback, auxdata);
  return Compressor.Run();
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// Get/Set amount of memory used for compression/decompression
MemSize _4x4_METHOD::GetSetDeCompressionMem (COMPRESSION direction, MemSize mem, bool MINMEM)
{
  Set_compress_all_at_once_Until_end_of_block _(1);
  int t  =  MINMEM? 1 : GetNumThreads();
  int i  =  MINMEM? 0 : GetNumExtraBuffers();
  COMPRESSION_METHOD *cmethod = ParseCompressionMethod (Method);

  // Сколько памяти требуется для одного треда упаковки/распаковки (при распаковке может быть вызван только GetDecompressionMem)
  LongMemSize tmem =
#ifndef FREEARC_DECOMPRESS_ONLY
                     direction==COMPRESS? (MINMEM? ::GetMinCompressionMem(Method)   : ::GetCompressionMem(Method))
                                        : (MINMEM? ::GetMinDecompressionMem(Method) : ::GetDecompressionMem(Method));
#else
                                                                                      ::GetDecompressionMem(Method);
#endif

  // Сколько памяти требуется для одного набора доп. буферов I/O
  LongMemSize imem = 2*LongMemSize(_4x4_BUFFER_SIZE(direction, cmethod, BlockSize? BlockSize : ::GetDictionary(Method)));

  tmem += imem;  // Для каждого треда упаковки выделяется набор I/O буферов

  if (mem==0)  {delete cmethod;  return mymin(t*tmem+i*imem, MEMSIZE_MAX);}   // Either Get(De)compressionMem() or Set(De)compressionMem(0) was called


  // Если дошли досюда - значит выполняем одну из операций SetXxxMem. В зависимости от конкретной операции мы можем менять следующие параметры метода:
  // SetCompressionMem        :t:i и submethod/BlockSize
  // SetMinCompressionMem     submethod/BlockSize
  // SetDecompressionMem      :t:i
  // SetMinDecompressionMem   submethod/BlockSize

  // Посчитаем количество тредов и буферов, которое мы можем себе позволить
  if (mem >= t*tmem+i*imem) {
    // Памяти достаточно - "расширим" алгоритм сжатия
    //::SetDeCompressionMem (Method, mem/GetNumBuffers(), Method);

  } else if (mem >= t*tmem && !MINMEM) {
    // Памяти достаточно для всех тредов сжатия, уменьшаем число дополнительных буферов
    NumExtraBuffers = (mem-t*tmem)/imem;

  } else if (mem >= tmem && !MINMEM) {
    // Памяти достаточно хотя бы для одного треда сжатия - уменьшаем число тредов сжатия и при необходимости число дополнительных буферов
    NumThreads = t = mem/tmem;
    int new_i = (mem-t*tmem)/imem;
    if (new_i<i)  NumExtraBuffers = new_i;

  } else {
    // Памяти не хватило даже для одного треда сжатия - оставим только один и ещё подожмём его

    // Урезаем число тредов в Set(De)CompressionMem
    if (!MINMEM) {
      NumThreads      = 1;
      NumExtraBuffers = 0;
    }
#ifndef FREEARC_DECOMPRESS_ONLY
    // Урезаем BlockSize в любой операции, кроме SetDecompressionMem
    // Во время распаковки это невозможно, поскольку потеряется совместимость с упакованными с этими параметрами данными
    if (direction==COMPRESS || MINMEM) {
      MemSize bs = rounddown_mem (mem/4);
      if (BlockSize==0 || BlockSize>bs)
          BlockSize = bs;
      mem -= 2*_4x4_BUFFER_SIZE(direction, cmethod, BlockSize);
      direction==COMPRESS? ::SetCompressionMem      (Method, mem, Method)    // SetMinCompressionMem doesn't exist
                         : ::SetMinDecompressionMem (Method, mem, Method);   // SetDecompressionMem: see above
    }
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)
  }
  delete cmethod;
  return 0;
}


// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_4x4)
void _4x4_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
  _4x4_METHOD defaults;
  char ThreadsStr[100], ExtraBuffersStr[100], BlockSizeStr[100], PercentsStr[100], PurifiedMethod[MAX_METHOD_STRLEN]="";
  sprintf (ThreadsStr,      ":t%d", NumThreads);
  sprintf (ExtraBuffersStr, ":i%d", NumExtraBuffers);
  sprintf (PercentsStr,     ":r%lg",MinOrder0Percents);
  showMem (BlockSize, BlockSizeStr);
  if (purify)
    if(CanonizeCompressionMethod (Method, PurifiedMethod, 1) != FREEARC_OK)
      *PurifiedMethod = 0;
  sprintf (buf, "4x4%s%s%s%s%s:%s", !purify && NumThreads     !=defaults.NumThreads?       ThreadsStr     :"",
                                    !purify && NumExtraBuffers!=defaults.NumExtraBuffers?  ExtraBuffersStr:"",
                                    BlockSize? ":b":"",  BlockSize? BlockSizeStr   :"",
                                    !purify && MinOrder0Percents !=defaults.MinOrder0Percents?   PercentsStr:"",
                                    *PurifiedMethod? PurifiedMethod : Method);
}

// Конструирует объект типа _4x4_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка при задании параметров
COMPRESSION_METHOD* parse_4x4 (char** parameters)
{
  if (strcmp (parameters[0], "4x4") == 0) {
    // Если название метода (нулевой параметр) - "4x4", то разберём остальные параметры

    _4x4_METHOD *p = new _4x4_METHOD;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    while (!error && *++parameters)  // Переберём все параметры метода
    {
      char *param = *parameters;

      // Если параметр начинается не с цифры или буквы плюс цифры - это начало описания метода сжатия
      if (!(isdigit(param[0]) || isdigit(param[1])))
      {
        join (parameters, COMPRESSION_METHOD_PARAMETERS_DELIMITER, p->Method, sizeof(p->Method));
        COMPRESSION_METHOD *cmethod = ParseCompressionMethod (p->Method);
        error = (cmethod==NULL);
        delete cmethod;
        break;
      }

      if (strlen(param)==1) switch (*param) {   // Однобуквенные параметры
      }
      else switch (*param) {                    // Параметры, содержащие значения
        case 'b':  p->BlockSize        =  parseMem (param+1, &error); continue;
        case 't':  p->NumThreads       =  parseInt (param+1, &error); continue;
        case 'i':  p->NumExtraBuffers  =  parseInt (param+1, &error); continue;
        case 'r':  p->MinOrder0Percents=  parseDouble (param+1, &error); continue;
      }

      // Сюда мы попадаем, если в параметре не указано его название
      // Если этот параметр удастся разобрать как целое число (т.е. в нём - только цифры),
      // то присвоим его значение полю Threads, иначе попробуем разобрать его как BlockSize
      int n = parseInt (param, &error);
      if (!error) p->NumThreads = n;
      else        error=0, p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    return p;
  } else
    return NULL;   // Это не метод 4x4
}

static int _4x4_x = AddCompressionMethod (parse_4x4);   // Зарегистрируем парсер метода 4x4
