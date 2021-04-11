#include "../Compression.h"

#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class _4x4_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  COMPRESSION direction;                    // Either COMPRESS or DECOMPRESS
  char    Method[MAX_METHOD_STRLEN];        // Compression method used for every block
  MemSize BlockSize;                        // Size of chunks input is split to
  int     NumThreads;                       // Number of compression threads
  int     NumExtraBuffers;                  // Number of additional compression buffers used to optimize I/O
  double  MinOrder0Percents;                // Minimal order0_compression_ratio() that allow us to don't try to compress the block at all

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  _4x4_METHOD();
  int GetNumThreads()       {return NumThreads>0? NumThreads : GetCompressionThreads();}   // Number of (de)compression threads
  int GetNumExtraBuffers()  {return NumExtraBuffers>=0? NumExtraBuffers : 2;}              // Number of additional compression buffer pairs used for background I/O

  // Универсальный метод, отвечает на запрос "has_progress?"
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);
  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить (минимальный) объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem     (void)         {return GetSetDeCompressionMem (COMPRESS, 0);}
  virtual MemSize GetMinCompressionMem  (void)         {return GetSetDeCompressionMem (COMPRESS, 0, true);}
  virtual MemSize GetMinDecompressionMem(void)         {return GetSetDeCompressionMem (DECOMPRESS, 0, true);}
  virtual void    SetCompressionMem     (MemSize mem)  {GetSetDeCompressionMem (COMPRESS, mem);}
  virtual void    SetMinDecompressionMem(MemSize mem)  {GetSetDeCompressionMem (DECOMPRESS, mem, true);}
  virtual void    SetDictionary         (MemSize dict) {::SetDictionary (Method, dict, Method);}
#endif
  virtual MemSize GetDictionary         (void)         {return ::GetDictionary(Method);}
  virtual MemSize GetDecompressionMem   (void)         {return GetSetDeCompressionMem (DECOMPRESS, 0);}
  virtual void    SetDecompressionMem   (MemSize mem)  {GetSetDeCompressionMem (DECOMPRESS, mem);}
          MemSize GetSetDeCompressionMem(COMPRESSION direction, MemSize mem, bool MINMEM = false);  // Get/Set amount of memory used for compression/decompression

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_4x4)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода сжатия 4x4
COMPRESSION_METHOD* parse_4x4 (char** parameters);

#endif  // __cplusplus
