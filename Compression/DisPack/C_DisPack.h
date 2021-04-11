#include "../Compression.h"

#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class DISPACK_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  MemSize BlockSize;        // Размер блока данных, обрабатываемых за один раз
  int     ExtendedTables;   // Искать таблицы с размером элемента, отличным от 2/4

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  DISPACK_METHOD();

  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void)               {return 3*BlockSize+BlockSize/4+1024;}
  virtual void    SetCompressionMem        (MemSize mem)        {if (mem>0)   BlockSize = mymax(mem/13*4,64*kb);}
  virtual void    SetMinDecompressionMem   (MemSize mem)        {if (mem>0)   BlockSize = mymax(mem/ 9*4,64*kb);}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return 2*BlockSize+BlockSize/4+1024;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_DISPACK)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода сжатия DISPACK
COMPRESSION_METHOD* parse_DISPACK (char** parameters);

#endif  // __cplusplus
