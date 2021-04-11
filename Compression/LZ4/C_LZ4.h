#include "../Compression.h"

#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class LZ4_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  int     Compressor;       // >0: use LZ4_compressHC()
  MemSize BlockSize;        // Размер блока, обрабатываемого за раз. Совпадения ищутся только внутри этого блока.
  MemSize HashSize;         // Размер хеш-таблицы.
  int     MinCompression;   // Минимальный процент сжатия. Если выходные данные больше, то вместо них будут записаны оригинальные (несжатые) данные.

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  LZ4_METHOD();

  // Упаковка/распаковка в памяти
  virtual int DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback=0, void *auxdata=0, void **CodecState=0);
  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void);
  virtual MemSize GetDictionary            (void)               {return BlockSize;}
  virtual void    SetCompressionMem        (MemSize mem)        {}
  virtual void    SetMinDecompressionMem   (MemSize mem)        {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return compress_all_at_once? 0 : BlockSize*2;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_LZMA)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода сжатия LZMA
COMPRESSION_METHOD* parse_LZMA (char** parameters);

#endif  // __cplusplus
