extern "C" {
#include "C_DisPack.h"
}


#define DISPACK_LIBRARY
#include "DisPack.cpp"

/*-------------------------------------------------*/
/* Реализация класса DISPACK_METHOD                */
/*-------------------------------------------------*/

// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
DISPACK_METHOD::DISPACK_METHOD()
{
    BlockSize      = 8*mb;
    ExtendedTables = 0;
}

enum {TAG_DATA = 0xC71B3AE1, TAG_EXE};
bool is_tag (unsigned x)  {return (x^TAG_DATA) < 0x10;}

// Функция распаковки
int DISPACK_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    int   errcode = FREEARC_OK;     // Error code returned by last operation or FREEARC_OK
    BYTE *In = NULL,  *Out = NULL;  // Указатели на входные и выходные данные, соответственно
    uint  BaseAddress = 1u<<30;
    int   CHUNK_SIZE, InBufferSize = BlockSize+BlockSize/4+1024;
    READ4_OR_EOF (CHUNK_SIZE);
    if (CHUNK_SIZE > BlockSize)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    BIGALLOC (BYTE, In,  InBufferSize+2);
    BIGALLOC (BYTE, Out, BlockSize+2);
    for(;;) {
        int tag;
        READ4_OR_EOF (tag);
        if (!is_tag(tag) || tag==TAG_DATA) {
            // скопируем неупакованные данные, из них 4 байта мы уже возможно прочитали ;)
            int done = 0, len;
            if (tag==TAG_DATA) {
              READ4 (len);
            } else {
              done = 4;
              len = CHUNK_SIZE;
              setvalue32 (In, tag);
            }
            READ  (In+done, len-done);
            WRITE (In, len);
            BaseAddress += len;
        } else if (tag==TAG_EXE) {
            int InSize, OutSize;     // количество байт во входном и выходном буфере, соответственно
            // Произвести декодирование и получить размер выходных данных
            READ4 (OutSize);
            READ4 (InSize);
            if (OutSize > BlockSize  ||  InSize > InBufferSize)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            READ (In, InSize);
            bool success = DisUnFilter (In, InSize, Out, OutSize, BaseAddress);
            if (!success)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            WRITE (Out, OutSize);
            BaseAddress += OutSize;
        } else {
            ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
        }
        if (BaseAddress >= 3u<<30)  BaseAddress -= 2u<<30;
    }
finished:
    BigFreeAndNil(In); BigFreeAndNil(Out);
    return errcode;
}

#ifndef FREEARC_DECOMPRESS_ONLY

enum EXETYPE {EXETYPE_UNKNOWN, EXETYPE_DATA, EXETYPE_EXE};

EXETYPE detect (BYTE *buf, int len)
{
  int e8=0, exe=0, obj=0;
  for (BYTE *p=buf; p+5<buf+len; p++)
  {
    if (*p == 0xE8)
    {
      e8++;
      if (p[4]==0xFF && p[5]!=0xFF)
        exe++;
      if (p[4]==0    && p[5]!=0)
        obj++;
    }
  }
  // printf("  e8 %d, exe %d, obj %d, len %d\n", e8, exe, obj, len);
  return double(e8)/len >= 0.002   &&   double(exe+obj)/e8 >= 0.20  &&   double(exe)/e8 >= 0.01?  EXETYPE_EXE : EXETYPE_DATA;
}

// Функция упаковки
int DISPACK_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    int   errcode = FREEARC_OK;     // Error code returned by last operation or FREEARC_OK
    BYTE *In = NULL,  *Out = NULL;  // Указатели на входные и выходные данные, соответственно
    int   InSize;  uint32 OutSize;  // Количество байт во входном и выходном буфере, соответственно
    uint  BaseAddress = 1u<<30;
    const int CHUNK_SIZE = 16*kb;
    bool  first_time = TRUE;
    BIGALLOC (BYTE, In, BlockSize+2);
    for(;;)
    {
        // Читаем файл блоками по 16 кб, пока не кончится исполняемый код
        BYTE *p = In;  int len;
        do {
            READ_LEN (len, p, CHUNK_SIZE);
            if (len==0) break;
            EXETYPE exe_type = detect (p, len);
            if (exe_type!=EXETYPE_EXE) break;
            p += len, len = 0;
        } while (p-In <= BlockSize-CHUNK_SIZE);

        InSize = p-In;
        if (InSize+len == 0)  break;
        if (first_time)   WRITE4 (CHUNK_SIZE);  first_time = FALSE;

        if (InSize)
        {
            // Кодируем исполняемый код
            Out = DisFilter(In, InSize, BaseAddress, OutSize);
            if (Out==NULL)  ReturnErrorCode(FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);
            WRITE4 (TAG_EXE);
            WRITE4 (InSize);
            WRITE4 (OutSize);
            WRITE  (Out, OutSize);
            free (Out);
        }
        if (len)
        {
            // Кодируем прочие данные
            if (len!=CHUNK_SIZE  ||  is_tag(value32(p))) {
                WRITE4 (TAG_DATA);
                WRITE4 (len);
            }
            WRITE (p, len);
        }
        if ((BaseAddress += InSize+len)  >=  3u<<30)   BaseAddress -= 2u<<30;
    }
finished:
    BigFreeAndNil(In); //BigFreeAndNil(Out);
    return errcode;
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_DISPACK)
void DISPACK_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
    DISPACK_METHOD defaults; char BlockSizeStr[100]=":";
    showMem (BlockSize, BlockSizeStr+1);
    sprintf (buf, "dispack070%s%s", BlockSize!=defaults.BlockSize? BlockSizeStr:"", ExtendedTables? ":x":"");
}

// Конструирует объект типа DISPACK_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_DISPACK (char** parameters)
{
  if (strcmp (parameters[0], "dispack") == 0
   || strcmp (parameters[0], "dispack070") == 0) {
    // Если название метода (нулевой параметр) - "dispack", то разберём остальные параметры

    DISPACK_METHOD *p = new DISPACK_METHOD;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    // Переберём все параметры метода (или выйдем раньше при возникновении ошибки при разборе очередного параметра)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      if (strlen(param)==1) switch (*param) {    // Однобуквенные параметры
        case 'x':  p->ExtendedTables = 1; continue;
      }
      switch (*param) {                    // Параметры, содержащие значения
        case 'b':  p->BlockSize = parseMem (param+1, &error); continue;
      }
      // Сюда мы попадаем, если в параметре не указано его название
      // Если этот параметр удастся разобрать как объём памяти,
      // то присвоим его значение полю BlockSize
      p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    return p;
  } else
    return NULL;   // Это не метод DISPACK
}

static int DISPACK_x = AddCompressionMethod (parse_DISPACK);   // Зарегистрируем парсер метода DISPACK

