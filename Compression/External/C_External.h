#include "../Compression.h"

// Добавить в таблицу методов сжатия описанный пользователем в arc.ini внешний упаковщик.
// params содержит описание упаковщика из arc.ini. Возвращает 1, если описание корректно.
int AddExternalCompressor (char *params);

// Синхронизация доступа к консоли
void SynchronizeConio_Enter (void);
void SynchronizeConio_Leave (void);


#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class EXTERNAL_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  char    *name;            // Имя метода (pmm, ccm...)
  bool     can_set_mem;     // Доступно изменение требований к памяти?
  MemSize  cmem;            // Объём памяти, используемой для сжатия
  MemSize  dmem;            // Объём памяти, используемой для распаковки
  char    *datafile;        // Наименование файла с неупакованными данными
  char    *packedfile;      // Наименование файла с упакованными данными
  char    *packcmd;         // Команда упаковки данных (datafile -> packedfile)
  char    *unpackcmd;       // Команда распаковки данных (packedfile -> datafile)
  char    *options[MAX_PARAMETERS];             // Доп. параметры метода
  char     option_strings[MAX_METHOD_STRLEN];   // Текстовый буфер для хранения текста параметров
  char    *defaultopt;      // Значения параметров по умолчанию
  int      solid;           // Разрешено делать солид-блоки?
  int      useHeader;       // TRUE, если в начало сжатого потока записывается 0/1 - данные несжаты/сжаты; FALSE - при подмене внутренних методов

  // Параметры, специфичные для PPMonstr
  int     order;            // Порядок модели (по скольким последним сивмолам предсказывается следующий)
  int     MRMethod;         // Что делать, когда память, выделенная для хранения модели, исчерпана
  int     MinCompression;   // Минимальный процент сжатия. Если выходные данные больше, то вместо них будут записаны оригинальные (несжатые) данные

  EXTERNAL_METHOD() {};

  // Универсальный метод: возвращаем различные простые характеристики метода сжатия
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);

  // Упаковка/распаковка
  int DeCompress (COMPRESSION direction, CALLBACK_FUNC *callback, void *auxdata);

  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata)     {return DeCompress (DECOMPRESS, callback, auxdata);}
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata)     {return DeCompress (COMPRESS, callback, auxdata);}

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem        (void)               {return cmem;}
  virtual void    SetCompressionMem        (MemSize _mem);
  virtual void    SetMinDecompressionMem   (MemSize _mem)       {SetCompressionMem(_mem);}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return dmem;}

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_EXTERNAL)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// Разборщик строки препроцессора EXTERNAL
COMPRESSION_METHOD* parse_EXTERNAL (char** parameters);

#endif  // __cplusplus
