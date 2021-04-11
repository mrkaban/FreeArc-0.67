#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include "../Compression.h"
#include "../MultiThreading.h"

extern "C" {
#include "C_PPMD.h"
}
#include "PPMdType.h"


#define INSERT(routine,locking)                                                                                               \
                                                                                                                              \
extern "C" {                                                                                                                  \
int routine (int ENCODE, int order, MemSize mem, int MRMethod, MemSize, CALLBACK_FUNC *callback, void *auxdata)               \
{                                                                                                                             \
  locking;                                                                                                                    \
  _PPMD_FILE fpIn (callback, auxdata, BUFFER_SIZE);                                                                           \
  _PPMD_FILE fpOut(callback, auxdata, BUFFER_SIZE);                                                                           \
  if ( !StartSubAllocator(mem) ) {                                                                                            \
    return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;                                                                                 \
  }                                                                                                                           \
  ENCODE? EncodeFile (&fpOut, &fpIn, order, MR_METHOD(MRMethod))                                                              \
        : DecodeFile (&fpOut, &fpIn, order, MR_METHOD(MRMethod));                                                             \
  StopSubAllocator();                                                                                                         \
  fpOut.flush();                                                                                                              \
  return mymin(_PPMD_ERROR_CODE(&fpIn), _PPMD_ERROR_CODE (&fpOut));                                                           \
}                                                                                                                             \
}                                                                                                                             \
                                                                                                                              \
void _STDCALL PrintInfo (_PPMD_FILE* DecodedFile, _PPMD_FILE* EncodedFile)                                                    \
{                                                                                                                             \
}                                                                                                                             \



/*-------------------------------------------------*/
/* Реализация ppmd_*_compress                      */
/*-------------------------------------------------*/
#define _USE_THREAD_KEYWORD

namespace PPMD_de_compression {
#if defined(PPMD_MT) || defined(FREEARC_UNIX)
#include "Model.cpp"
INSERT(ppmd_de_compress,(void)0)
#undef _PPMD_H_
#else
static FARPROC ppmd_de_compress = NULL;
#endif
}


#undef _USE_THREAD_KEYWORD

namespace PPMD_decompression {
#include "Model.cpp"
INSERT(ppmd_decompress2, static Mutex mutex; Lock _(mutex))
}
#undef _PPMD_H_

namespace PPMD_compression {
#ifndef FREEARC_DECOMPRESS_ONLY
#include "Model.cpp"
INSERT(ppmd_compress2, static Mutex mutex; Lock _(mutex))
#else
static FARPROC ppmd_compress2 = NULL;
#endif // FREEARC_DECOMPRESS_ONLY
}
#undef _PPMD_H_


/*-------------------------------------------------*/
/* Реализация класса PPMD_METHOD                  */
/*-------------------------------------------------*/

// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
PPMD_METHOD::PPMD_METHOD()
{
  order    = 10;
  mem      = 48*mb;
  MRMethod = 0;
  chunk    = 0;
}

// Диспетчеризатор функций упаковки/распаковки
int ppmd_dispatch (int ENCODE, int order, MemSize mem, int MRMethod, MemSize chunk, CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC    compress = LoadFromDLL ("ppmd_compress2");
  static FARPROC  decompress = LoadFromDLL ("ppmd_decompress2");
  static FARPROC de_compress = LoadFromDLL ("ppmd_de_compress");

  // Use m/t function for calls from 4x4 if possible
  FARPROC f = compress_all_at_once? (de_compress? de_compress : (FARPROC) PPMD_de_compression::ppmd_de_compress) : (FARPROC) NULL;
  if (!f) f = ENCODE? (FARPROC) compress                         : (FARPROC) decompress;
  if (!f) f = ENCODE? (FARPROC) PPMD_compression::ppmd_compress2 : (FARPROC) ppmd_decompress2;
  if (!f)   return FREEARC_ERRCODE_INVALID_COMPRESSOR;
  return ((int (*)(int, int, MemSize, int, MemSize, CALLBACK_FUNC*, void*)) f) (ENCODE, order, mem, MRMethod, chunk, callback, auxdata);
}

// Функция распаковки
int PPMD_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return ppmd_dispatch (FALSE, order, mem, MRMethod, chunk, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int PPMD_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return ppmd_dispatch (TRUE, order, mem, MRMethod, chunk, callback, auxdata);
}

// Изменить потребность в памяти, заодно оттюнинговав order
void PPMD_METHOD::SetCompressionMem (MemSize _mem)
{
  if (_mem==0)  return;
  _mem = (_mem>1*mb+MIN_MEM? _mem-1*mb : MIN_MEM);
  order  =  mymax(MIN_O, mymin(MAX_O, order + int (log(double(_mem)/mem) / log(double(2)) * 4)));
  mem = _mem;
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_PPMD)
void PPMD_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
  char MemStr[100], ChunkStr[100];
  showMem (mem, MemStr);
  showMem (chunk, ChunkStr);
  sprintf (buf, "ppmd:%d:%s%s%s%s", order, MemStr, MRMethod==2? ":r2": (MRMethod==1? ":r":""), chunk?":c":"", chunk?ChunkStr:"");
}

// Конструирует объект типа PPMD_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_PPMD (char** parameters)
{
  if (strcmp (parameters[0], "ppmd") == 0) {
    // Если название метода (нулевой параметр) - "ppmd", то разберём остальные параметры

    PPMD_METHOD *p = new PPMD_METHOD;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    // Переберём все параметры метода (или выйдем раньше при возникновении ошибки при разборе очередного параметра)
    while (*++parameters && !error)
    {
      char *param = *parameters;
      if (start_with (param, "mem")) {
        param+=2;  // Обработать "mem..." как "m..."
      }
      if (strlen(param)==1) switch (*param) {    // Однобуквенные параметры
        case 'r':  p->MRMethod = 1; continue;
      }
      else switch (*param) {                    // Параметры, содержащие значения
        case 'm':  p->mem      = parseMem (param+1, &error); continue;
        case 'o':  p->order    = parseInt (param+1, &error); continue;
        case 'r':  p->MRMethod = parseInt (param+1, &error); continue;
        case 'c':  p->chunk    = parseMem (param+1, &error); continue;
      }
      // Сюда мы попадаем, если в параметре не указано его название
      // Если этот параметр удастся разобрать как целое число (т.е. в нём - только цифры),
      // то присвоим его значение полю order, иначе попробуем разобрать его как mem
      int n = parseInt (param, &error);
      if (!error) p->order = n;
      else        error=0, p->mem = parseMem (param, &error);
    }
    if (error || p->mem<MIN_MEM || p->order<MIN_O || p->order>MAX_O)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода или значение параметра за пределами допустимого
    return p;
  } else
    return NULL;   // Это не метод ppmd
}

static int PPMD_x = AddCompressionMethod (parse_PPMD);   // Зарегистрируем парсер метода PPMD
