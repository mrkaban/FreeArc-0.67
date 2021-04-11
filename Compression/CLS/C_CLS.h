#include "../Compression.h"
#include "../_CLS/cls.h"


#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class CLS_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  char     name[MAX_METHOD_STRLEN];            // Имя метода (pmm, ccm...)        ////
  char     params[MAX_METHOD_STRLEN];          // Доп. параметры метода           ////
  CLS_MAIN *ClsMain;
  CALLBACK_FUNC *callback;
  void *auxdata;

  CLS_METHOD(char *_name, CLS_MAIN *_ClsMain)
    { strcpy(name, _name); ClsMain = _ClsMain; strcpy(params, ""); }

  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void)               {return 0;}
  virtual void    SetCompressionMem        (MemSize)            {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return 0;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_CLS)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки препроцессора CLS
COMPRESSION_METHOD* parse_CLS (char** parameters);

#endif  // __cplusplus
