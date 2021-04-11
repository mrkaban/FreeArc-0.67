#include "../Compression.h"

#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class LZMA_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  MemSize dictionarySize;
  MemSize hashSize;
  int     algorithm;
  int     numFastBytes;
  int     matchFinder;
  int     matchFinderCycles;
  int     posStateBits;
  int     litContextBits;
  int     litPosBits;

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  LZMA_METHOD();

  // Универсальный метод, отвечает на запрос "has_progress?"
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);
  // Упаковка/распаковка в памяти
  virtual int DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback=0, void *auxdata=0, void **CodecState=0);
  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Вычисляет общий расход памяти и размер хеш-таблицы при упаковке
  void CalcCompressionMemories (MemSize *pmem, MemSize *phashSize);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void);
  virtual void    SetCompressionMem        (MemSize mem);
  virtual void    SetMinDecompressionMem   (MemSize mem);
  virtual void    SetDictionary            (MemSize dict);
#endif
  virtual MemSize GetDictionary            (void)               {return dictionarySize;}
  virtual MemSize GetDecompressionMem      (void);
  virtual LongMemSize GetMaxCompressedSize (LongMemSize insize) {return insize + (insize/40) + 512;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_LZMA)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода сжатия LZMA
COMPRESSION_METHOD* parse_LZMA (char** parameters);

#endif  // __cplusplus
