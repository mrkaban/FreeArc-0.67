#include "../Compression.h"

int tor_compress   (PackMethod m, CALLBACK_FUNC *callback, void *auxdata);
int tor_decompress (CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class TORNADO_METHOD : public COMPRESSION_METHOD
{
public:
  struct PackMethod m;      // Параметры этого метода сжатия

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  TORNADO_METHOD();
  // Универсальный метод: даём положительный ответ на запросы "VeryFast?" для режимов сжатия 1-4, а ткже на запрос "has_progress?"
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
         if (strequ (what, "VeryFast?"))          return m.hash_row_width<=2;
    else if (strequ (what, "has_progress?"))      return 1;                        // Да, этот алгоритм поддерживает отчёт о прогрессе упаковки
    else                                          return COMPRESSION_METHOD::doit (what, param, data, callback);  // Передать остальные вызовы родительской процедуре
  }

  // Упаковка/распаковка в памяти
  virtual int DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback=0, void *auxdata=0, void **CodecState=0);

  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void);
  virtual void    SetCompressionMem        (MemSize mem);
  virtual void    SetMinDecompressionMem   (MemSize mem)        {SetDictionary (mem);}
  virtual void    SetDictionary            (MemSize dict);
#endif
  virtual MemSize GetDictionary            (void)               {return m.buffer;}
  virtual MemSize GetDecompressionMem      (void);
  virtual LongMemSize GetMaxCompressedSize (LongMemSize insize);

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_TORNADO)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода сжатия TORNADO
COMPRESSION_METHOD* parse_TORNADO (char** parameters);

#endif  // __cplusplus
