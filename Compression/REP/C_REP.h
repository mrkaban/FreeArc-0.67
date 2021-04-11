#include "../Compression.h"

int rep_compress   (unsigned BlockSize, int MinCompression, int ChunkSize, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, void *auxdata);
int rep_decompress (unsigned BlockSize, int MinCompression, int ChunkSize, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class REP_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  MemSize BlockSize;        // Размер буфера. Совпадения ищутся только в пределах этой дистанции. Расход памяти - BlockSize+BlockSize/4
  int     MinCompression;   // Минимальный процент сжатия. Если выходные данные больше, то вместо них будут записаны оригинальные (несжатые) данные
  int     ChunkSize;        // Размер блоков, КС которых заносится в хеш
  int     MinMatchLen;      // Минимальная длина строки, при которой она будет заменяться ссылкой на предыдущее вхождение
  int     Barrier;          // Граница, после которой допускается использовать совпадения меньшего размера (поскольку lzma/ppmd всё равно пропустит их)
  int     SmallestLen;      // Этот меньший размер
  int     HashSizeLog;      // Логарифм размера хеша (в 4-байтовых словах). Большие значения увеличивают сжатие, но замедляют его. При нулевом значении оптимальный размер вычисляется автоматически
  int     Amplifier;        // Коэффициент "усиления" поиска

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  REP_METHOD();

  // Универсальный метод: возвращаем различные простые характеристики метода сжатия
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
      if      (strequ (what,"SparseDecompression?"))  return 1;
      else return COMPRESSION_METHOD::doit (what, param, data, callback);
  }

  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void);
  virtual void    SetCompressionMem        (MemSize mem);
  virtual void    SetMinDecompressionMem   (MemSize mem)        {if (mem>0)   BlockSize = mem;}
  virtual void    SetDictionary            (MemSize dict)       {if (dict>0)  BlockSize = dict;}
#endif
  virtual MemSize GetDictionary            (void)               {return BlockSize;}
  virtual MemSize GetDecompressionMem      (void)               {return BlockSize;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_REP)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки метода сжатия REP
COMPRESSION_METHOD* parse_REP (char** parameters);

#endif  // __cplusplus
