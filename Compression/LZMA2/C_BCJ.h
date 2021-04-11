#include "../Compression.h"
int bcj_x86_de_compress (int encoding, CALLBACK_FUNC *callback, void *auxdata);

#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class BCJ_X86_METHOD : public COMPRESSION_METHOD
{
public:
  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void)               {return LARGE_BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize mem)        {}
  virtual void    SetMinDecompressionMem   (MemSize mem)        {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return LARGE_BUFFER_SIZE;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия (функция, обратная к parse_BCJ_X86)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода сжатия BCJ_X86
COMPRESSION_METHOD* parse_BCJ_X86 (char** parameters);

#endif  // __cplusplus
