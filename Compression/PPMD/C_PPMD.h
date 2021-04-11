#include "../Compression.h"

int ppmd_compress2   (int ENCODE, int order, MemSize mem, int MRMethod, MemSize chunk, CALLBACK_FUNC *callback, void *auxdata);
int ppmd_decompress2 (int ENCODE, int order, MemSize mem, int MRMethod, MemSize chunk, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class PPMD_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  int     order;     // Порядок модели (по скольким последним сивмолам предсказывается следующий)
  MemSize mem;       // Объём памяти, используемой для хранения модели
  int     MRMethod;  // Что делать, когда память, выделенная для хранения модели, исчерпана
  MemSize chunk;     // Размер сжимаемого куска при compress_all_at_once

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  PPMD_METHOD();

  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void)               {return mem+1*mb;}
  virtual void    SetCompressionMem        (MemSize _mem);
  virtual void    SetMinDecompressionMem   (MemSize _mem)       {SetCompressionMem(_mem);}
#endif
  virtual MemSize GetAlgoMem               (void)               {return mem;}
  virtual MemSize GetDecompressionMem      (void)               {return mem+1*mb;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_PPMD)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода сжатия PPMD
COMPRESSION_METHOD* parse_PPMD (char** parameters);

#endif  // __cplusplus
