#include "Compression.h"
#include "MultiThreading.h"

// ****************************************************************************************************************************
// Internal basic (de)compression routines                                                                                    *
// ****************************************************************************************************************************

// (De)compress data with supplied method and report operation time via "time" callback
static int timed_de_compress (COMPRESSION direction, COMPRESSION_METHOD *compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  //SET_JMP_POINT( FREEARC_ERRCODE_GENERAL);
  double time0 = GetThreadCPUTime();
#ifndef FREEARC_DECOMPRESS_ONLY
  int result = (direction==COMPRESS?  compressor->compress (callback, auxdata)  :  compressor->decompress (callback, auxdata));
#else
  int result = (direction==COMPRESS?  FREEARC_ERRCODE_ONLY_DECOMPRESS           :  compressor->decompress (callback, auxdata));
#endif
  double time1 = GetThreadCPUTime(), t;
  if (time0>=0 && time1>=0 && compressor->addtime>=0)
    t = compressor->addtime + time1 - time0;
  else
    t = -1;
  callback ("time", &t, 0, auxdata);
  return result;
}

// (De)compress data with method encoded as string
static int single_de_compress (COMPRESSION direction, char *method, CALLBACK_FUNC *callback, void *auxdata)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    int result = timed_de_compress (direction, compressor, callback, auxdata);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}



// ****************************************************************************************************************************
// Механизм передачи данных от одного алгоритма (рас)паковки другому                                                          *
// ****************************************************************************************************************************

struct AsOutput  {virtual int Write(BYTE *buf, int size) = 0;  virtual void NoMoreWrites(){} };
struct AsInput   {virtual int Read (BYTE *buf, int size) = 0;  virtual void NoMoreReads() {} };

struct Channels : AsOutput, AsInput
{
  BYTE               *buf;            // Buffer pointing to the data sent between methods
  int                 size;           // Amount of data in the buf
  Semaphore           read;           // Semaphores for inter-thread communication
  Semaphore           write;
  bool                NoMoreData;

  Channels()           {NoMoreData = false;  read.Create(0,1);  write.Create(0,1);}
  void NoMoreReads()   {NoMoreData = true;   write.Release();}                    // Tell the output thread that no more data required
  void NoMoreWrites()  {NoMoreData = true;   read .Release();}                    // Tell the input thread that no more data will be supplied to it

  // Послать _size байт из _buf
  int Write (BYTE *_buf, int _size)
  {
    buf  = _buf;
    size = _size;
    read.Release();                            // даём разрешение на чтение (в буфере появились данные)
    if (!NoMoreData)  write.Wait();            // ожидаем разрешения на выход (после того, как все данные будут прочитаны)
    return NoMoreData? FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED : _size;
  }

  // Получить _size байт в _buf. _size==0 используется для проверки готовности к чтению
  int Read (BYTE *_buf, int _size)
  {
    int prev=0;
loop:
    //if (_size==0)  return prev;
    read.Wait();             // ожидаем разрешения на чтение (появления данных в буфере)
    if (NoMoreData)          // данных больше не будет - предыдущий тред завершён
    {
      read.Release();        // возвращаем разрешение на чтение
      return prev;
    }
    else if (_size <= size)  // данных в буфере достаточно для выполнения чтения
    {
      memcpy (_buf, buf, _size);
      buf  += _size;
      size -= _size;
      read.Release();        // возвращаем разрешение на чтение
      return prev+_size;
    }
    else // size < _size     // данных в буфере недостаточно для выполнения чтения
    {
      memcpy (_buf, buf, size);
      _buf  += size;
      _size -= size;
      prev  += size;
      write.Release();       // даём разрешение на выход из записи (буфер пуст)
      goto loop;
    }
  }
};



// ****************************************************************************************************************************
// Multi-stream input/output classes ******************************************************************************************
// ****************************************************************************************************************************

// Формат многопоточного файла: [Заголовок] [Данные]
//   Заголовок ::= [Смещение к началу потока 1]..[Смещение к началу потока N-1]
//   Данные ::= [Кусочек потока 0] [Кусочек потока i] [Кусочек потока j]...
//   Кусочек потока k ::= [Длина кусочка] [Данные кусочка] [Смещение к следующему кусочку того же потока]
// Длины/смещения записываются 4-байтными числами в Intel-формате. 0 используется как спецфлаг, реальные значения перед записью увеличиваются на 10.
// Спецфлаг в поле смещения озгначает, что кусочков этого потока больше нет. Спецфлаг в поле длины означает, что этот кусочек длится до конца файла.

// Перейти на заданную позицию во входном/выходном файле
#define SEEK_OUTPUT(pos)     SEEK("seek_output",pos)
#define SEEK_INPUT(pos)      {if (!(pos).empty && (pos).n!=curpos)  {SEEK("seek_input", (pos).n);  curpos=(pos).n;}}
#define SEEK(request,pos)                                                         \
{                                                                                 \
    int64 localPos = (pos);                                                       \
    int localErrCode;                                                             \
    if ((localErrCode=callback(request,&localPos,(pos),auxdata)) < 0) {           \
        errcode = localErrCode;                                                   \
        goto finished;                                                            \
    }                                                                             \
}

// Пара RESERVE/OVERWRITE резервирует в выходном потоке место под запись числа и затем заполняет его.  Пока под запись любого числа используются ровно 4 байта.
// LOAD загружает из входного потока записанное таким способом значение.  Зарезервированное, но не заполненное значение считывается как place.empty==true.
#define RESERVE(place)                                          \
{                                                               \
    (place).pos = curpos;                                       \
    WRITE4(SPECIAL_VALUE_EMPTY);                                \
    curpos += (place).len = 4;                                  \
}
#define OVERWRITE(place,base)                                   \
{                                                               \
    if ((place).pos != INVALID_FILESIZE) {                      \
        SEEK_OUTPUT((place).pos);                               \
        WRITE4(SPECIAL_VALUES+(base)-(place).pos-(place).len);  \
     }                                                          \
}
#define LOAD(place)                                             \
{                                                               \
    READ4 ((place).n);                                          \
    curpos += 4;                                                \
    if (0<(place).n  &&  (place).n<SPECIAL_VALUES) {            \
        errcode = FREEARC_ERRCODE_READ;                         \
        goto finished;                                          \
    }                                                           \
    (place).empty  =  ((place).n == SPECIAL_VALUE_EMPTY);       \
    (place).n     -=  SPECIAL_VALUES;                           \
}
#define SPECIAL_VALUES        10
#define SPECIAL_VALUE_EMPTY   0
#define INVALID_FILESIZE      (-1)


// Число, записанное в выходной файл - его позиция и кол-во использованных байт
struct PLACE {FILESIZE pos; int len;};

// FILESIZE or empty value
struct MAYBE_FILESIZE {FILESIZE n; bool empty;};

// Реализация многопоточной записи в один файл путём интерливинга данных от отдельных потоков
struct MultiWriter
{
  bool                first_time;     // TRUE - structure wasn't yet initialized
  Mutex               exclusive_access; // Ensure single-threaded access to the structure and underlying output file
  int                 nStreams;       // Number of output streams going to be interleaved
  CALLBACK_FUNC      *callback;       // Original callback (function that provides access to the underlying files)
  void               *auxdata;        // Original callback parameter
  FILESIZE            curpos;         // Current position in the output file
  int                 last_n;         // Номер текущего записываемого в файл потока (при переключении на другой поток мы должны завершить кусочек старого)
  PLACE              *ofs;            // ofs[N] - позиция в файле, куда надо записать смещение к следующему кусочку потока N, как только мы его начнём
  PLACE              *len;            // len[N] - позиция в файле, куда надо записать длину кусочка потока N, как только мы его завершим

  MultiWriter (int _nStreams, CALLBACK_FUNC *_callback, void *_auxdata)  {nStreams = _nStreams;  callback = _callback;  auxdata = _auxdata;
                                                                          first_time = TRUE;  ofs = new PLACE[nStreams];  len = new PLACE[nStreams];}
  int Write (int n, BYTE *buf, int size);
 ~MultiWriter() {delete[] len; delete[] ofs;}
};


// Реализация многопоточного чтения из одного файла путём деинтерливинга данных отдельных потоков
struct MultiReader
{
  bool                first_time;     // TRUE - structure wasn't yet initialized
  Mutex               exclusive_access; // Ensure single-threaded access to the structure and underlying input file
  int                 nStreams;       // Number of input streams going to be deinterleaved
  CALLBACK_FUNC      *callback;       // Original callback (function that provides access to the underlying files)
  void               *auxdata;        // Original callback parameter
  FILESIZE            curpos;         // Current position in the input file
  int                 last_n;         // Номер текущего читаемого из файла потока (при переключении на другой поток мы должны сделать seek на его текущий кусочек)
  MAYBE_FILESIZE     *pos;            // pos[N] - позиция во входном файле, откуда будут читаться следующие данные потока N
  MAYBE_FILESIZE     *len;            // len[N] - кол-во байт, оставшихся в текущем кусочке потока N

  MultiReader (int _nStreams, CALLBACK_FUNC *_callback, void *_auxdata)  {nStreams = _nStreams;  callback = _callback;  auxdata = _auxdata;
                                                                          first_time = TRUE;  pos = new MAYBE_FILESIZE[nStreams];  len = new MAYBE_FILESIZE[nStreams];}
  int Read (int n, BYTE *buf, int size);
 ~MultiReader() {delete[] len; delete[] pos;}
};


// Записать содержимое буфера в поток n
int MultiWriter::Write (int n, BYTE *buf, int size)
{
  // Writing to sole output don't require the interleaving
  if (n==0 && nStreams==1)  return callback ("write", buf, size, auxdata);

  Lock _(exclusive_access);                                          // Prohibit simultaneous writing from multiple OS threads
  int errcode = FREEARC_OK;
  if (n<0 || n>=nStreams || nStreams<=1)  return FREEARC_ERRCODE_GENERAL;

  if (first_time) {                                                  // Initialization of multi-stream writing
    first_time = FALSE;  curpos = 0;
    for(int i=1; i<nStreams; i++) {
      RESERVE(ofs[i]);                                               // Зарезервируем в выходном файле место для записи смещения к первому кусочку потока i
      len[i].pos = INVALID_FILESIZE;
    }
    last_n = 0;                                                      // Начнём запись в поток 0
    ofs[0].pos = INVALID_FILESIZE;
    RESERVE(len[0]);                                                 // Зарезервируем в выходном файле место для записи длины начинающегося здесь кусочка потока 0
  }

  if (n != last_n) {                                                 // Switching from writing to stream last_n into writing to stream n
    FILESIZE data_end = curpos;        RESERVE (ofs[last_n]);        // Зарезервируем в выходном файле место для записи смещения к след. кусочку потока last_n
    FILESIZE between_blocks = curpos;  RESERVE (len[n]);             // Зарезервируем в выходном файле место для записи длины начинающегося здесь кусочка потока n

    OVERWRITE (ofs[n], between_blocks);                              // Вернёмся назад и запишем смещение из пред. кусочка потока n к вновь начинающемуся
    OVERWRITE (len[last_n], data_end);                               // Вернёмся назад и запишем длину только что закончившегося кусочка потока last_n

    SEEK_OUTPUT (curpos);
    last_n = n;
  }

  WRITE (buf, size);
  curpos += size;
  return FREEARC_OK;

  finished:  return errcode;
}


// Fill buf with data read from stream n
int MultiReader::Read (int n, BYTE *buf, int size)
{
  // Reading from sole input don't need the deinterleaving
  if (n==0 && nStreams==1)  return callback ("read", buf, size, auxdata);

  Lock _(exclusive_access);                                          // Prohibit simultaneous reading from multiple OS threads
  int errcode = FREEARC_OK,  orig_size = size;
  if (n<0 || n>=nStreams || nStreams<=1)  return FREEARC_ERRCODE_GENERAL;

  if (first_time) {                                                  // Initialization of multi-stream reading
    first_time = FALSE;  curpos = 0;
    for (int i=1; i<nStreams; i++) {                                 // Starting positions of streams 1..N-1 were saved at the file beginning
      LOAD (pos[i]);  pos[i].n += curpos;                            // Decode position of the first chunk of the stream i
      len[i].n = 0;   len[i].empty = false;
    }
    last_n = 0;                                                      // Начнём чтение из потока 0
    pos[0].n = curpos;  pos[0].empty = false;                        // Stream 0 starts right after the saved positions of other streams
    len[0].n = 0;       len[0].empty = false;                        // Length of first chunk of stream 0 isn't yet loaded
  }

  if (n != last_n) {                                                 // Switching from reading stream last_n to reading of stream n
    SEEK_INPUT (pos[n]);
    last_n = n;
  }

  while (size) {                                                     // While we need to read more data to the buf
    if (!len[n].empty && len[n].n==0) {                              // We should jump to the next chunk of stream n
      if (pos[n].empty)  break;                                      // End-Of-Stream: there are no more chunks in stream n
      SEEK_INPUT (pos[n]);
      LOAD (len[n]);                                                 // Read length of being started chunk of stream n
      pos[n].n = curpos;                                             // Advance pos[n] to the first byte after the length encoding
    }
    int  b,  bytes  =  len[n].empty?  size : mymin (size, len[n].n); // How much bytes we can read
    if (bytes) {
      READ_LEN (b, buf, bytes);  if (b==0)  break;
      curpos   += b;
      pos[n].n += b;
      len[n].n -= b;
      buf      += b;
      size     -= b;
    }
    if (!len[n].empty && len[n].n==0) {                              // At the end of chunk
      LOAD (pos[n]);  pos[n].n += curpos;                            // Decode position of the next chunk of the same stream
    }
  }
  return orig_size-size;

  finished:  return errcode;
}


struct MultiReaderAsInput : AsInput
{
  int n;  MultiReader *multi_reader;
  void Bind (int _n, MultiReader *_multi_reader)   { n = _n;  multi_reader = _multi_reader;}
  int  Read (BYTE *buf, int size)                  {return multi_reader->Read (n, buf, size);}
};

struct MultiWriterAsOutput : AsOutput
{
  int n;  MultiWriter *multi_writer;
  void Bind (int _n, MultiWriter *_multi_writer)   { n = _n;  multi_writer = _multi_writer;}
  int Write (BYTE *buf, int size)                  {return multi_writer->Write (n, buf, size);}
};



// ****************************************************************************************************************************
// Упаковка и распаковка данных деревом методов                                                                               *
// ****************************************************************************************************************************

// Глобальные данные процесса (рас)паковки
struct Globals
{
  COMPRESSION         direction;      // COMPRESS or DECOMPRESS - that's the question!
  CALLBACK_FUNC      *callback;       // Original callback (function that reads data in the first method and writes data in the last one)
  void               *auxdata;        // Original callback parameter
  int                 retcode;        // multi_de_compress() return code
  Mutex               retcode_cs;     // Ensure single-threaded access to retcode

  Globals (COMPRESSION _direction, CALLBACK_FUNC* _callback, void* _auxdata)  {direction=_direction; callback=_callback; auxdata=_auxdata; retcode=0;}

  // Set overall multi_de_compress() exit code
  void SetExitCode (int code)
  {
    Lock _(retcode_cs);
    // Save into retcode first error code signalled (subsequent error codes may be sequels of the first one)
    if (retcode==0 && code!=FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED)
      retcode = code;
  }
};

// Локальные данные одного метода
struct Locals
{
  Globals            *Global;
  Thread              thread;    // OS thread executing this (de)compression algorithm
  CMETHOD             method;    // String denoting (de)compression method with its parameters
  int                 NumInputs;
  int                 NumOutputs;
  AsInput**           Input;
  AsOutput**          Output;
};

static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE multi_de_compress_thread (void *paramPtr);
static int multi_de_compress_callback (const char *what, void *buf, int size, void *paramPtr);


// Упаковать/распаковать данные деревом методов
static int multi_de_compress (COMPRESSION direction, char *compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  // Мы будем работать с копией строки, потому что split() модифицирует её
  char new_compressor [MAX_COMPRESSOR_STRLEN],  output_compressors_buf[MAX_COMPRESSOR_STRLEN*2],  *out_ptr = output_compressors_buf;
  strncopy (new_compressor, compressor, MAX_COMPRESSOR_STRLEN);

  // Разобьём компрессор на отдельные алгоритмы и запустим для каждого из них отдельный тред
  CMETHOD cm[MAX_METHODS_IN_COMPRESSOR];
  int N = split (new_compressor, COMPRESSION_METHODS_DELIMITER, cm, MAX_METHODS_IN_COMPRESSOR);

  // Теперь мы знаем, что дерево включает N методов сжатия, создадим локальные структуры для них
  Globals  Global  (direction, callback, auxdata);
  Locals   Local   [MAX_METHODS_IN_COMPRESSOR];
  Channels Channel [MAX_METHODS_IN_COMPRESSOR];
  MultiReaderAsInput  Reader [MAX_METHODS_IN_COMPRESSOR];
  MultiWriterAsOutput Writer [MAX_METHODS_IN_COMPRESSOR];
  int InChan=1, OutChan=1;

  // Arrays storing inputs/ouputs for all methods
  AsInput  *LocalInputsArray [MAX_METHODS_IN_COMPRESSOR],  **LocalInputs  = LocalInputsArray;
  AsOutput *LocalOutputsArray[MAX_METHODS_IN_COMPRESSOR],  **LocalOutputs = LocalOutputsArray;
#define AllocInputs(Loc,n)   {Loc.Input  = LocalInputs;   Loc.NumInputs  = n;  LocalInputs  += n;}
#define AllocOutputs(Loc,n)  {Loc.Output = LocalOutputs;  Loc.NumOutputs = n;  LocalOutputs += n;}


  // Сформируем взаимосвязи входов и выходов алгоритмов в соответствии с деревом методов
  for (int i=N-1; i>=0; i--)
  {
    int nStreams  =  CompressionService (cm[i], "NumOutStreams");
    if (nStreams <= 0)  return FREEARC_ERRCODE_INVALID_COMPRESSOR;

    if (direction==COMPRESS)  // СОЗДАДИМ ДЕРЕВО ВЫХОДОВ ДЛЯ УПАКОВКИ ============================================================
    {
      AllocInputs  (Local[i], 1);          Local[i].Input[0]   =  i>0?    (AsInput*)  &Channel[i-1] : &Reader[0];
      AllocOutputs (Local[i], nStreams);   Local[i].Output[0]  =  i<N-1?  (AsOutput*) &Channel[i]   : &Writer[0];
      for (int j=1; j<nStreams; j++) {
        *out_ptr = '\0';                                                           // буфер для запроса метода упаковки j-го выхода алгоритма
        if (CompressionService (cm[i], "GetOutStreamCompressor", j, out_ptr)  <  0)  return FREEARC_ERRCODE_INVALID_COMPRESSOR;
        if (*out_ptr) {   // ============================================ На выходе стоит метод сжатия ==========================
                                       Local[i].Output[j] = &Channel[N];           // Используем канал N для передачи данных между этими методами
          AllocInputs  (Local[N], 1);  Local[N].Input[0]  = &Channel[N];
          AllocOutputs (Local[N], 1);  Local[N].Output[0] = &Writer[OutChan++];    // Выход нового метода пойдёт в новый поток в выходном файле
          cm[N++] = out_ptr;           out_ptr += strlen(out_ptr)+1;               // Добавим новый метод сжатия в дерево
        } else {          // ============================================ Выход напрямую записывается в выходной файл ===========
                                       Local[i].Output[j] = &Writer[OutChan++];    // Этот выход метода пойдёт в новый поток в выходном файле
        }
      }
    }
    else  // СОЗДАДИМ ДЕРЕВО ВХОДОВ ДЛЯ РАСПАКОВКИ ===============================================================================
    {
      AllocOutputs (Local[i], 1);          Local[i].Output[0]  =  i>0?    (AsOutput*) &Channel[i-1] : &Writer[0];
      AllocInputs  (Local[i], nStreams);   Local[i].Input[0]   =  i<N-1?  (AsInput*)  &Channel[i]   : &Reader[0];
      for (int j=1; j<nStreams; j++) {
        *out_ptr = '\0';                                                           // буфер для запроса упаковщика j-го выхода алгоритма
        if (CompressionService (cm[i], "GetOutStreamCompressor", j, out_ptr)  <  0)  return FREEARC_ERRCODE_INVALID_COMPRESSOR;
        if (*out_ptr) {   // ============================================ На входе стоит метод сжатия ===========================
                                       Local[i].Input[j]  = &Channel[N];           // Используем канал N для передачи данных между этими методами
          AllocOutputs (Local[N], 1);  Local[N].Output[0] = &Channel[N];
          AllocInputs  (Local[N], 1);  Local[N].Input[0]  = &Reader[InChan++];     // Выход нового метода пойдёт в новый поток в выходном файле
          cm[N++] = out_ptr;           out_ptr += strlen(out_ptr)+1;               // Добавим новый метод сжатия в дерево
        } else {          // ============================================ Вход напрямую читается из выходного файла =============
                                       Local[i].Input[j] = &Reader[InChan++];      // Этот выход метода пойдёт в новый поток в выходном файле
        }
      }
    }
  }
  if (N==1 && InChan==1 && OutChan==1)  return single_de_compress (direction, new_compressor, callback, auxdata);  // multi-threading isn't required

  // Объекты ввода/вывода для входных и выходных потоков
  MultiReader multi_reader (InChan,  callback, auxdata);    // Worker object implementing multi-stream reading for decompression
  MultiWriter multi_writer (OutChan, callback, auxdata);    // Worker object implementing multi-stream writing for compression

  iterate (InChan,   Reader[i].Bind (i, &multi_reader) );
  iterate (OutChan,  Writer[i].Bind (i, &multi_writer) );

  // Start N threads
  for (int i=0; i<N; i++) {
    Local[i].Global = &Global;
    Local[i].method = cm[i];
    Local[i].thread.Create (multi_de_compress_thread, &Local[i]);
  }

  // Wait until all threads will be finished and return errcode or 0 at success
  for (int i=0; i<N; i++)
    Local[i].thread.Wait();
  return Global.retcode;
}


// Один тред распаковки в multi_de_compress
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE multi_de_compress_thread (void *paramPtr)
{
  Locals  *Local  = (Locals*) paramPtr;
  Globals *Global = Local->Global;
  // Не запускать этот thread, пока не начался вывод из предыдущего (для экономии памяти)
  Local->Input[0]->Read(NULL,0);    // ожидаем появления данных в буфере
  SetCompressionThreadPriority();                                                   // понизить приоритет треда
  int ret = single_de_compress (Global->direction, Local->method, multi_de_compress_callback, paramPtr);
  if (ret<0)                                 Global->SetExitCode (ret);             // Set global error code if single_de_compress() returned with error
  for (int i=0; i<Local->NumInputs; i++)     Local->Input[i] ->NoMoreReads();       // Tell the input threads that no more data are required
  for (int i=0; i<Local->NumOutputs; i++)    Local->Output[i]->NoMoreWrites();      // Tell the output threads that no more data will be supplied to them
  return 0;
}


// Callback-функция для multi_de_compress_thread(), перехватывающая операции чтения и записи
static int multi_de_compress_callback (const char *what, void *_buf, int size, void *paramPtr)
{
  Locals  *Local  = (Locals*) paramPtr;
  Globals *Global = Local->Global;
  BYTE    *buf    = (BYTE*)_buf;

  // readXXX: прочитать данные для обработки из входного потока номер XXX, 0 по умолчанию
  if (start_with(what,"read")) {
    int xxx  =  what[4]? atoi(what+4) : 0;
    if (xxx >= Local->NumInputs)  return FREEARC_ERRCODE_READ;
    return Local->Input[xxx]->Read (buf, size); }

  // writeXXX: записать результат обработки в выходной поток номер XXX, 0 по умолчанию
  else if (start_with(what,"write")) {
    int xxx  =  what[5]? atoi(what+5) : 0;
    if (xxx >= Local->NumOutputs)  return FREEARC_ERRCODE_WRITE;
    return Local->Output[xxx]->Write (buf, size); }

  // progress: пропустить поскольку мы пока не умеем комбинировать "progress" от цепочки упаковщиков
  else if (strequ(what,"progress")) {
    return FREEARC_ERRCODE_NOT_IMPLEMENTED; }

  // Все остальные запросы передаются на выполнение в оригинальный callback
  else return Global->callback (what, buf, size, Global->auxdata);
}



// ****************************************************************************************************************************
// Compression                                                                                                                *
// ****************************************************************************************************************************

#ifndef FREEARC_DECOMPRESS_ONLY
// Compress data with given compressor (compression method or chain of compression methods)
int Compress (char *compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  return multi_de_compress (COMPRESS, compressor, callback, auxdata);
}

// Put to the output stream string describing the compressor and then the compressed data
int CompressWithHeader (char *orig_compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  // Split compressor into methods
  char new_compressor [MAX_COMPRESSOR_STRLEN],  canonical_methods [MAX_COMPRESSOR_STRLEN],  *p = canonical_methods;
  CMETHOD method[MAX_METHODS_IN_COMPRESSOR];
  strncopy (new_compressor, orig_compressor, MAX_COMPRESSOR_STRLEN);
  int N = split (new_compressor, COMPRESSION_METHODS_DELIMITER, method, MAX_METHODS_IN_COMPRESSOR);

  // Generate canonical, pure representation of each compression method (in order to improve compatibility with future decompressors and omit compression-stage specific hints
  for (int i=0; i<N; i++)
  {
    CanonizeCompressionMethod (method[i], p, TRUE);
    method[i] = p;  p += strlen(p)+1;
  }
  method[N] = NULL;

  // Join canonized method strings back together and write canonicalized compressor string to output stream
  join (method, COMPRESSION_METHODS_DELIMITER, new_compressor, MAX_COMPRESSOR_STRLEN);
  int result = callback ("write", new_compressor, strlen(new_compressor)+1, auxdata);

  if (result>=0)
    result = Compress (orig_compressor, callback, auxdata);   // Use original compressor string since it may include hints omitted by CanonizeCompressionMethod(purify=TRUE)
  return result;
}
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)



// ****************************************************************************************************************************
// Decompression                                                                                                              *
// ****************************************************************************************************************************

// Decompress data with given compressor (compression method or chain of compression methods)
int Decompress (char *compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  return multi_de_compress (DECOMPRESS, compressor, callback, auxdata);
}

// Read string, describing the compressor, from input stream and decompress rest of stream with this compressor
int DecompressWithHeader (CALLBACK_FUNC *callback, void *auxdata)
{
  char compressor [MAX_COMPRESSOR_STRLEN];
  for (int i=0; i<MAX_COMPRESSOR_STRLEN; i++)
  {
    // Посимвольно читаем входные данные, пока не прочтём символ конца строки
    callback ("read", &compressor[i], 1, auxdata);
    if (compressor[i]=='\0')
      return Decompress (compressor, callback, auxdata);
  }
  return FREEARC_ERRCODE_INVALID_COMPRESSOR;  // Сюда мы попадаем, если в первых MAX_COMPRESSOR_STRLEN символах входных данных не нашлось символа '\0'
}



// ****************************************************************************************************************************
// Getters and Setters: query or set memory usage, dictionary, block size for compresssion method                             *
// ****************************************************************************************************************************

#define Generate_Getter(GETTER)                                              \
  MemSize GETTER (char *method)                                              \
  {                                                                          \
    COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);        \
    if (compressor){                                                         \
      MemSize bytes = compressor->GETTER();                                  \
      delete compressor;                                                     \
      return bytes;}                                                         \
    else                                                                     \
      return (MemSize)FREEARC_ERRCODE_INVALID_COMPRESSOR;                    \
  }                                                                          \

#define Generate_Setter(SETTER)                                              \
  int SETTER (char *in_method, MemSize bytes, char *out_method)              \
  {                                                                          \
    COMPRESSION_METHOD *compressor = ParseCompressionMethod (in_method);     \
    if (compressor){                                                         \
      compressor->SETTER (bytes);                                            \
      compressor->ShowCompressionMethod (out_method, FALSE);                 \
      delete compressor;                                                     \
      return FREEARC_OK;}                                                    \
    else                                                                     \
      return FREEARC_ERRCODE_INVALID_COMPRESSOR;                             \
  }                                                                          \

// Информация о памяти, необходимой для упаковки/распаковки, размере словаря и размере блока
Generate_Getter(GetDictionary)
Generate_Getter(GetBlockSize)
Generate_Getter(GetDecompressionMem)
Generate_Setter(SetDecompressionMem)
Generate_Setter(LimitDecompressionMem)

#ifndef FREEARC_DECOMPRESS_ONLY
// Информация о памяти, необходимой для упаковки/распаковки
Generate_Getter(GetCompressionMem)
Generate_Getter(GetMinCompressionMem)
Generate_Getter(GetMinDecompressionMem)

// Возвратить в out_method новый метод сжатия, настроенный на использование
// соответствующего количества памяти при упаковке/распаковке или словаря/размера блока
Generate_Setter(SetDictionary)
Generate_Setter(SetBlockSize)
Generate_Setter(SetCompressionMem)
Generate_Setter(SetMinDecompressionMem)

// Возвратить в out_method новый метод сжатия, уменьшив, если необходимо,
// используемую алгоритмом память / его словарь / размер блока
Generate_Setter(LimitDictionary)
Generate_Setter(LimitBlockSize)
Generate_Setter(LimitCompressionMem)
Generate_Setter(LimitMinDecompressionMem)
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Объём памяти, характеризующий алгоритм
MemSize COMPRESSION_METHOD::GetAlgoMem()
{
  MemSize dict = GetDictionary();
  if (dict)  return dict;
  MemSize BlockSize = GetBlockSize();
  if (BlockSize)  return BlockSize;
  return 0;
}

// Универсальный метод. Параметры:
//   what: "compress", "decompress", "setCompressionMem", "limitDictionary"...
//   data: данные для операции в формате, зависящем от конкретной выполняемой операции
//   param&result: простой числовой параметр, что достаточно для многих информационных операций
// Неиспользуемые параметры устанавливайте в NULL/0. result<0 - код ошибки
int COMPRESSION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
       if (strequ (what, "encryption?"))           return 0;                            // Это алгоритм шифрования?
#ifndef FREEARC_DECOMPRESS_ONLY
  else if (strequ (what, "GetCompressionMem"))     return GetCompressionMem();          // Объём памяти, необходимый для упаковки
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)
  else if (strequ (what, "GetDecompressionMem"))   return GetDecompressionMem();        // Объём памяти, необходимый для распаковки
  else if (strequ (what, "NumOutStreams"))         return 1;                            // Кол-во выходных потоков упаковщика
  else if (strequ (what, "GetAlgoMem"))            return GetAlgoMem();                 // Объём памяти, характеризующий алгоритм
  else                                             return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// Вывести в canonical_method каноническое представление метода сжатия in_method
int CanonizeCompressionMethod (char *method, char *canonical_method, int purify)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    compressor->ShowCompressionMethod (canonical_method, purify!=0);
    delete compressor;
    return FREEARC_OK;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}


// ****************************************************************************************************************************
// (De)compress data from memory buffer (input) to another memory buffer (output)                                             *
// ****************************************************************************************************************************

// Callback-функция чтения/записи для (рас)паковки в памяти
int ReadWriteMem (const char *what, void *buf, int size, void *_membuf)
{
  MemBuf *membuf = (MemBuf*)_membuf;
  if (strequ(what,"read")  &&  membuf->readPtr) {
    // Скопировать данные из readPtr в buf и продвинуть указатель чтения
    int read_bytes = membuf->readLeft<size ? membuf->readLeft : size;
    memcpy (buf, membuf->readPtr, read_bytes);
    membuf->readPtr  += read_bytes;
    membuf->readLeft -= read_bytes;
    return read_bytes;
  } else if (strequ(what,"write")  &&  membuf->writePtr) {
    // Скопировать данные из buf в writePtr и продвинуть указатель записи
    if (size > membuf->writeLeft)  return FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL;
    memcpy (membuf->writePtr, buf, size);
    membuf->writePtr  += size;
    membuf->writeLeft -= size;
    return size;
  } else {
    // Остальные операции отдаём "наверх"
    return (membuf->callback? membuf->callback (what, buf, size, membuf->auxdata)
                            : FREEARC_ERRCODE_NOT_IMPLEMENTED);
  }
}

// Реализация DeCompressMem по умолчанию: через compress/decompress и MemBuf. Затратный, но универсальный вариант
int COMPRESSION_METHOD::DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback, void *auxdata, void **CodecState)
{
  MemBuf membuf(input, inputSize, output, *outputSize, callback, auxdata);
  int result  =  (direction==COMPRESS?
#ifndef FREEARC_DECOMPRESS_ONLY
                                       compress   (ReadWriteMem, &membuf)
#else
                                       FREEARC_ERRCODE_ONLY_DECOMPRESS
#endif
                                     : decompress (ReadWriteMem, &membuf));
  *outputSize = membuf.written();
  return result;
}

// Распаковать данные в памяти, записав в выходной буфер не более outputSize байт.
// Возвращает код ошибки или количество байт, записанных в выходной буфер
int DecompressMem (char *compressor, void *input, int inputSize, void *output, int outputSize)
{
  MemBuf membuf(input, inputSize, output, outputSize);
  int result = Decompress (compressor, ReadWriteMem, &membuf);
  return result<0 ? result : membuf.written();
}

// Аналог DecompressMem с чтением алгоритма сжатия из самого входного буфера
int DecompressMemWithHeader (void *input, int inputSize, void *output, int outputSize)
{
  MemBuf membuf(input, inputSize, output, outputSize);
  int result = DecompressWithHeader (ReadWriteMem, &membuf);
  return result<0 ? result : membuf.written();
}

#ifndef FREEARC_DECOMPRESS_ONLY
// Упаковать данные в памяти, записав в выходной буфер не более outputSize байт.
// Возвращает код ошибки или количество байт, записанных в выходной буфер
int CompressMem (char *compressor, void *input, int inputSize, void *output, int outputSize)
{
  MemBuf membuf(input, inputSize, output, outputSize);
  int result = Compress (compressor, ReadWriteMem, &membuf);
  return result<0 ? result : membuf.written();
}

// Упаковать данные в памяти, записав в выходной буфер не более outputSize байт.
// Возвращает код ошибки или количество байт, записанных в выходной буфер
int CompressMemWithHeader (char *compressor, void *input, int inputSize, void *output, int outputSize)
{
  MemBuf membuf(input, inputSize, output, outputSize);
  int result = CompressWithHeader (compressor, ReadWriteMem, &membuf);
  return result<0 ? result : membuf.written();
}
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)



// ****************************************************************************************************************************
// УТИЛИТЫ                                                                                                                    *
// ****************************************************************************************************************************

// Разбить COMPRESSOR на отдельные алгоритмы сжатия/шифрования
//void splitCompressor (COMPRESSOR c, ARRAY<CMETHOD> &cm)

// Запросить сервис what метода сжатия method
int CompressionService (char *method, char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    int result = compressor->doit (what, param, data, callback);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Проверить, что данный компрессор включает алгоритм шифрования
int compressorIsEncrypted (COMPRESSOR c)
{
  char compressor[MAX_COMPRESSOR_STRLEN];  strcpy (compressor, c);

  // Разобьём компрессор на отдельные алгоритмы и поищем среди них алгоритм шифрования
  CMETHOD arr[MAX_METHODS_IN_COMPRESSOR];
  split (compressor, COMPRESSION_METHODS_DELIMITER, arr, MAX_METHODS_IN_COMPRESSOR);
  for (CMETHOD *cm=arr; *cm; cm++)
    if (CompressionService (*cm, "encryption?") == 1)  return TRUE;
  return FALSE;
}

// Вычислить, сколько памяти нужно для распаковки данных, сжатых этим компрессором (портит c)
MemSize compressorGetDecompressionMem (COMPRESSOR c)
{
  // Разобьём компрессор на отдельные алгоритмы и просуммируем их требования к памяти
  CMETHOD arr[MAX_METHODS_IN_COMPRESSOR];
  split (c, COMPRESSION_METHODS_DELIMITER, arr, MAX_METHODS_IN_COMPRESSOR);
  MemSize sum=0;
  for (CMETHOD *cm=arr; *cm; cm++)
    sum += CompressionService (*cm, "GetDecompressionMem");
  return sum;
}



// ****************************************************************************************************************************
// ПОДДЕРЖКА ТАБЛИЦЫ ЗАРЕГИСТРИРОВАННЫХ РАЗБОРЩИКОВ МЕТОДОВ СЖАТИЯ И ПОИСК В ЭТОЙ ТАБЛИЦЕ РЕАЛИЗАЦИИ ЧИСТО КОНКРЕТНОГО МЕТОДА *
// ****************************************************************************************************************************

template <class PARSER>
struct Parser
{
  PARSER  parser;
  void*   data;
};


int cmCount = 0;                                       // Кол-во зарегистрированных методов сжатия
Parser<CM_PARSER>  cmTable[MAX_COMPRESSION_METHODS];   // Таблица, в которую записываются все зарегистрированные парсеры методов сжатия

// Добавить новый метод в список поддерживаемых методов сжатия
int AddCompressionMethod (CM_PARSER parser)
{
  CHECK (FREEARC_ERRCODE_INTERNAL,  cmCount < elements(cmTable),  (s,"INTERNAL ERROR: Overflow of compression methods table"));
  cmTable[cmCount++].parser = parser;
  return 0;
}


int cmExternalCount = 0;                                       // Кол-во зарегистрированных внешних методов сжатия
Parser<CM_PARSER2> cmExternalTable[MAX_COMPRESSION_METHODS];   // Таблица, в которую записываются все зарегистрированные парсеры внешних методов сжатия

// Очистить таблицу внешних упаковщиков
void ClearExternalCompressorsTable (void)
{
  static int builtins = -1;  if (builtins<0)  builtins=cmExternalCount;
  cmExternalCount = builtins;  // Оставим только встроенные описания внешних упаковщиков
}

// Добавить парсер метода с дополнительным параметром, который должен быть передан этому парсеру
int AddExternalCompressionMethod (CM_PARSER2 parser, void *data)
{
  CHECK (FREEARC_ERRCODE_GENERAL,  cmExternalCount < elements(cmExternalTable),  (s,"Too much external compression methods defined"));
  cmExternalTable[cmExternalCount].parser = parser;
  cmExternalTable[cmExternalCount].data   = data;
  cmExternalCount++;
  return 0;
}


// Сконструировать объект класса COMPRESSION_METHOD, реализующий метод, заданный в виде строки `method`
COMPRESSION_METHOD *ParseCompressionMethod (char* method)
{
  // Превратим строку метода сжатия в массив строк `parameters`, хранящий его название и параметры
  char* parameters [MAX_PARAMETERS];
  char  local_method [MAX_METHOD_STRLEN];
  strncopy (local_method, method, sizeof (local_method));
  split (local_method, COMPRESSION_METHOD_PARAMETERS_DELIMITER, parameters, MAX_PARAMETERS);

  // Переберём все зарегистрированные парсеры методов сжатия и найдём тот, который сможет опознать `parameters`
  iterate_var (i, cmExternalCount)  {
     COMPRESSION_METHOD *m = (*cmExternalTable[i].parser) (parameters, cmExternalTable[i].data);
     if (m)  return m;
  }
  iterate_var (i, cmCount)  {
     COMPRESSION_METHOD *m = (*cmTable[i].parser) (parameters);
     if (m)  return m;
  }
  return NULL;   // Полученный метод сжатия не опознан ни одним из парсеров
}



// ****************************************************************************************************************************
// Loading compression methods from external DLLs                                                                             *
// ****************************************************************************************************************************

#ifdef FREEARC_WIN
HINSTANCE hinstUnarcDll = NULL;   // unarc.dll instance
static bool loaded = FALSE;
static HMODULE dll = NULL,  mt_dll = NULL;
#endif

// Load accelerated function either from facompress.dll or facompress_mt.dll
FARPROC LoadFromDLL (char *funcname, int only_facompress_mt)
{
#ifdef FREEARC_WIN  // Non-Windows platforms aren't yet supported
  if (!loaded)
  {
    loaded = TRUE;

    // Get program's executable filename
    wchar_t path[MY_FILENAME_MAX];
    GetModuleFileNameW (hinstUnarcDll, path, MY_FILENAME_MAX);

    // Load facompress.dll from the same directory as executable/unarc.dll
    wchar_t *basename = _tcsrchr (path,L'\\')+1;
    _tcscpy (basename, L"facompress.dll");
    dll = LoadLibraryW(path);

    // Load facompress_mt.dll from the same directory as executable/unarc.dll
    _tcscpy (basename, L"facompress_mt.dll");
    mt_dll = LoadLibraryW(path);
  }

  FARPROC f = GetProcAddress (dll, funcname);
  return f && !only_facompress_mt? f : GetProcAddress (mt_dll, funcname);
#else
  return NULL;
#endif
}


// Other compression methods may chain-redefine this callback in order to perform their own cleanup procedures
static void NOP(){}
void (*BeforeUnloadDLL)() = &NOP;

// This function unloads DLLs containing accelerated compression functions
void UnloadDLL (void)
{
  (*BeforeUnloadDLL)();
#ifdef FREEARC_WIN
  loaded = FALSE;
  FreeLibrary(dll);        dll = NULL;
  FreeLibrary(mt_dll);  mt_dll = NULL;
#endif
}

// This function cleans up the Compression Library
void compressionLib_cleanup (void)
{
  removeTemporaryFiles();
}



// ****************************************************************************************************************************
// Compression Library global settings                                                                                        *
// ****************************************************************************************************************************

// Get/set number of threads used for (de)compression. 0 means "autodetect"
static int CompressionThreads = 1;
int  __cdecl GetCompressionThreads (void)         {return CompressionThreads;}
void __cdecl SetCompressionThreads (int threads)
{
  CompressionThreads = threads==0? 1 : threads;
#ifndef FREEARC_COMPRESSION_DLL
  static FARPROC f  = LoadFromDLL ("SetCompressionThreads");
  static FARPROC f2 = LoadFromDLL ("SetCompressionThreads", TRUE);
  if (f)   ((void (__cdecl *)(int)) f)  (threads);
  if (f2)  ((void (__cdecl *)(int)) f2) (threads);
#endif
}


// Used in 4x4 only: read entire input buffer before compression begins, allocate output buffer large enough to hold entire compressed output
int compress_all_at_once = 0;
void __cdecl Set_compress_all_at_once (int n)
{
  compress_all_at_once = n;
#ifndef FREEARC_COMPRESSION_DLL
  static FARPROC f  = LoadFromDLL ("Set_compress_all_at_once");
  static FARPROC f2 = LoadFromDLL ("Set_compress_all_at_once", TRUE);
  if (f)   ((void (__cdecl *)(int)) f)  (n);
  if (f2)  ((void (__cdecl *)(int)) f2) (n);
#endif
}


// Enable debugging output
int debug_mode = 0;
void __cdecl Set_debug_mode (int n)
{
  debug_mode = n;
#ifndef FREEARC_COMPRESSION_DLL
  static FARPROC f  = LoadFromDLL ("Set_debug_mode");
  static FARPROC f2 = LoadFromDLL ("Set_debug_mode", TRUE);
  if (f)   ((void (__cdecl *)(int)) f)  (n);
  if (f2)  ((void (__cdecl *)(int)) f2) (n);
#endif
}



// ***********************************************************************************************************************
// Реализация класса STORING_METHOD                                                                                      *
// ***********************************************************************************************************************

// Функция "(рас)паковки", копирующая данные один в один
int copy_data (CALLBACK_FUNC *callback, void *auxdata)
{
  char buf[BUFFER_SIZE]; int len;
  while ((len = callback ("read", buf, BUFFER_SIZE, auxdata)) > 0) {
    if ((len = callback ("write", buf, len, auxdata)) < 0)  break;
  }
  return len;
}

// Функция распаковки
int STORING_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return copy_data (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int STORING_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return copy_data (callback, auxdata);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Конструирует объект типа STORING_METHOD или возвращает NULL, если это другой метод сжатия
COMPRESSION_METHOD* parse_STORING (char** parameters)
{
  if (strcmp (parameters[0], "storing") == 0
      &&  parameters[1]==NULL )
    // Если название метода - "storing" и параметров у него нет, то это наш метод
    return new STORING_METHOD;
  else
    return NULL;   // Это не метод storing
}

static int STORING_x = AddCompressionMethod (parse_STORING);   // Зарегистрируем парсер метода STORING_METHOD



// ***********************************************************************************************************************
// Реализация класса CRC_METHOD                                                                                          *
// ***********************************************************************************************************************

#ifndef FREEARC_DECOMPRESS_ONLY
// Функция упаковки, просто "съедающая" входные жанные
int CRC_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  char buf[BUFFER_SIZE]; int len;
  while ((len = callback ("read", buf, BUFFER_SIZE, auxdata)) > 0);
  return len;
}
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Конструирует объект типа CRC_METHOD или возвращает NULL, если это другой метод сжатия
COMPRESSION_METHOD* parse_CRC (char** parameters)
{
  if (strcmp (parameters[0], "crc") == 0
      &&  parameters[1]==NULL )
    // Если название метода - "crc" и параметров у него нет, то это наш метод
    return new CRC_METHOD;
  else
    return NULL;   // Это не метод crc
}

static int CRC_x = AddCompressionMethod (parse_CRC);   // Зарегистрируем парсер метода CRC_METHOD


// ***********************************************************************************************************************
// Реализация класса FAKE_METHOD                                                                                          *
// ***********************************************************************************************************************

// Конструирует объект типа FAKE_METHOD или возвращает NULL, если это другой метод сжатия
COMPRESSION_METHOD* parse_FAKE (char** parameters)
{
  if (strcmp (parameters[0], "fake") == 0
      &&  parameters[1]==NULL )
    // Если название метода - "fake" и параметров у него нет, то это наш метод
    return new FAKE_METHOD;
  else
    return NULL;   // Это не метод fake
}

static int FAKE_x = AddCompressionMethod (parse_FAKE);   // Зарегистрируем парсер метода FAKE_METHOD



// ****************************************************************************************************************************
// ВЫЧИСЛЕНИЕ CRC-32                                                                                                          *
// ****************************************************************************************************************************

#ifdef FREEARC_COMPRESSION_DLL

// cdecl shells around 7-zip's fastcall assembler routines
extern "C" uint32 __fastcall CrcUpdateT8 (uint32 StartCRC, void *data, size_t size, const uint32 *table);
uint32 CrcUpdate (uint32 StartCRC, void *data, size_t size, const uint32 *table) {return CrcUpdateT8 (StartCRC, data, size, table);}

extern "C" void (__fastcall AesCtr_Code_Intel)(UInt32 *ivAes, Byte *data, size_t numBlocks);
void Fast_AesCtr_Code (UInt32 *ivAes, Byte *data, size_t numBlocks) {AesCtr_Code_Intel (ivAes, data, numBlocks);}

#else

#define kCrcPoly 0xEDB88320
#define CRC_NUM_TABLES 8
uint32 g_CrcTable[256 * CRC_NUM_TABLES];

void CrcGenerateTable()
{
  uint32 i;
  for (i = 0; i < 256; i++)
  {
    uint32 r = i;
    int j;
    for (j = 0; j < 8; j++)
      r = (r >> 1) ^ (kCrcPoly & ~((r & 1) - 1));
    g_CrcTable[i] = r;
  }
  #if CRC_NUM_TABLES > 1
  for (; i < 256 * CRC_NUM_TABLES; i++)
  {
    uint32 r = g_CrcTable[i - 256];
    g_CrcTable[i] = g_CrcTable[r & 0xFF] ^ (r >> 8);
  }
  #endif
}

#define CRC_UPDATE_BYTE_2(crc, b) (g_CrcTable[((crc) ^ (b)) & 0xFF] ^ ((crc) >> 8))

uint32 UpdateCRC (const void *data, size_t size, UInt32 v)
{
  if (g_CrcTable[elements(g_CrcTable)-1] == 0)
    CrcGenerateTable();

  static FARPROC f  =  LoadFromDLL ("CrcUpdate");
  if (f)   return ((uint32 (__cdecl *)(uint32, const void*, uint, const uint32*)) f) (v,data,size,g_CrcTable);

  const Byte *p = (const Byte *)data;
#if defined(FREEARC_INTEL_BYTE_ORDER)
  for (; size > 0 && ((unsigned)(ptrdiff_t)p & 3) != 0; size--, p++)
    v = CRC_UPDATE_BYTE_2(v, *p);
  for (; size >= 4; size -= 4, p += 4)
  {
    v ^= *(const UInt32 *)p;
    v =
      g_CrcTable[0x300 + (v & 0xFF)] ^
      g_CrcTable[0x200 + ((v >> 8) & 0xFF)] ^
      g_CrcTable[0x100 + ((v >> 16) & 0xFF)] ^
      g_CrcTable[0x000 + ((v >> 24))];
  }
#endif
  for (; size > 0; size--, p++)
    v = CRC_UPDATE_BYTE_2(v, *p);
  return v;
}

// Вычислить CRC блока данных
uint32 CalcCRC (const void *Addr, size_t Size)
{
  return UpdateCRC(Addr, Size, INIT_CRC) ^ INIT_CRC;
}

#endif
