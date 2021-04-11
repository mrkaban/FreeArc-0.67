#define TORNADO_LIBRARY
#include "Tornado.cpp"
extern "C" {
#include "C_Tornado.h"
}

/*-------------------------------------------------*/
/* Реализация класса TORNADO_METHOD                */
/*-------------------------------------------------*/

// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
TORNADO_METHOD::TORNADO_METHOD()
{
  m = std_Tornado_method [default_Tornado_method];
}

// Упаковка/распаковка в памяти
int TORNADO_METHOD::DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback, void *auxdata, void **CodecState)
{
  MemBuf membuf(input, inputSize, output, *outputSize, callback, auxdata);
  int result  =  (direction==COMPRESS?
#ifndef FREEARC_DECOMPRESS_ONLY
                                       tor_compress (m, ReadWriteMem, &membuf, NULL, -1)   // "input, inputSize" can't be used yet because it needs to restore diffed tables for the case of storing original data
#else
                                       FREEARC_ERRCODE_ONLY_DECOMPRESS
#endif
                                     : tor_decompress (ReadWriteMem, &membuf, NULL, -1));
  *outputSize = membuf.written();
  return result;
}

// Функция распаковки
int TORNADO_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return tor_decompress (callback, auxdata, NULL, -1);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int TORNADO_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return tor_compress (m, callback, auxdata, NULL, -1);
}


// Посчитать, сколько памяти требуется для упаковки заданным методом
MemSize TORNADO_METHOD::GetCompressionMem()
{                                                                                                                     // chunk*2
  return m.buffer + m.hashsize + m.auxhash_size + tornado_compressor_outbuf_size(m.buffer) + (compress_all_at_once? 0:LARGE_BUFFER_SIZE*2) + (m.hash3?1*mb:0)
         + (m.match_parser==OPTIMAL? OPTIMAL_WINDOW*(REPDIST_CODES+3)*sizeof(DISTANCE) : 0)
         + (m.match_finder>=BT_MF4? m.buffer*MemSize(2)*sizeof(UINT) : 0);
}

// Modifies compression method to use specified amount of memory
void TORNADO_METHOD::SetCompressionMem (MemSize mem)
{
  if (mem>0) {
    int koeff = compress_all_at_once? 5:3;
    m.hashsize = round_to_nearest_hashsize (1<<lb(mem/koeff), m.hash_row_width);   // Отведём хешу примерно 1/3-1/5 объёма памяти, тогда hashsize ~= buffer/2
    mem       -= m.hashsize;
    m.buffer   = tornado_compressor_calc_dict (mem>128*kb? mem-64*kb:64*kb);       // Calculate dictionary size depending on memory left and compress_all_at_once
  }
}

// Установить размер словаря и откорректировать размер хеша
void TORNADO_METHOD::SetDictionary (MemSize dict)
{
  if (dict>0) {
    if (dict < m.buffer)
      // При уменьшении словаря: уменьшить размер хэша, если он слишком велик для такого маленького блока
      m.hashsize  =  sizeof(PtrVal)  *  mymin (m.hashsize/sizeof(PtrVal), roundup_to_power_of(dict,2));
    else
      // При увеличении словаря: пропорционально увеличить размер хеша
      if (m.hashsize > 1*mb)
      {
        if (m.hashsize<8*mb && m.hashsize<m.buffer/2)   m.hashsize = m.buffer/2;  // Во-первых, увеличим размер хеша, если он был подогнан под кеш Core2
        // Вычислим идеальный размер нового хеша и округлим его с учётом row_width
        m.hashsize = round_to_nearest_hashsize (LongMemSize(dict) / (m.buffer/64) * (m.hashsize/64), m.hash_row_width);
      }
    m.buffer = dict;
  }
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Посчитать, сколько памяти требуется для распаковки данных, сжатых заданным методом
MemSize TORNADO_METHOD::GetDecompressionMem()
{
  return tornado_decompressor_outbuf_size(m.buffer) + LARGE_BUFFER_SIZE + 256*kb;
}

// Maximum possible inflation of incompressible input data
LongMemSize TORNADO_METHOD::GetMaxCompressedSize (LongMemSize insize)
{
  switch (m.encoding_method)
  {
    case STORING:   return insize + 512;
    case BYTECODER: return insize + (insize/4) + 512;
    case BITCODER:  return insize + (insize/8) + 512;
    case HUFCODER:
    case ARICODER:  return insize + (insize/40) + 512;
    default:        return COMPRESSION_METHOD::GetMaxCompressedSize(insize);
  }
}


// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_TORNADO)
void TORNADO_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
    struct PackMethod defaults = std_Tornado_method[m.number];  char NumStr[100], BufferStr[100], HashSizeStr[100], TempHashSizeStr[100], RowStr[100], EncStr[100], ParserStr[100], StepStr[100], TableStr[100], TempAuxHashSizeStr[100], AuxHashSizeStr[100], AuxRowStr[100], FastBytesStr[100], Hash3Str[100], MatchFinderStr[100];
    showMem (m.buffer,       BufferStr);
    showMem (m.hashsize,     TempHashSizeStr);
    showMem (m.auxhash_size, TempAuxHashSizeStr);
    sprintf (NumStr,                    m.number            != default_Tornado_method?     ":%d"    : "", m.number);
    sprintf (HashSizeStr,               m.hashsize          != defaults.hashsize?          ":h%s"   : "", TempHashSizeStr);
    sprintf (RowStr,                    m.hash_row_width    != defaults.hash_row_width?    ":l%d"   : "", m.hash_row_width);
    sprintf (EncStr,                    m.encoding_method   != defaults.encoding_method?   ":c%d"   : "", m.encoding_method);
    sprintf (ParserStr,      !purify && m.match_parser      != defaults.match_parser?      ":p%d"   : "", m.match_parser);
    sprintf (StepStr,                   m.update_step       != defaults.update_step?       ":u%d"   : "", m.update_step);
    sprintf (TableStr,                  m.find_tables       != defaults.find_tables?       ":t%d"   : "", m.find_tables);
    sprintf (Hash3Str,       !purify && m.hash3             != defaults.hash3?             ":s%d"   : "", m.hash3);
    sprintf (MatchFinderStr, !purify && m.match_finder      != defaults.match_finder?      ":x%d"   : "", m.match_finder);
    sprintf (AuxHashSizeStr,            m.auxhash_size      != defaults.auxhash_size?      ":ah%s"  : "", TempAuxHashSizeStr);
    sprintf (AuxRowStr,                 m.auxhash_row_width != defaults.auxhash_row_width? ":al%d"  : "", m.auxhash_row_width);
    sprintf (FastBytesStr,   !purify && m.fast_bytes        != defaults.fast_bytes?        ":fb%d"  : "", m.fast_bytes);
    sprintf (buf, "tor%s:%s%s%s%s%s%s%s%s%s%s%s%s", NumStr, BufferStr, HashSizeStr, RowStr, EncStr, ParserStr, MatchFinderStr, StepStr, TableStr, AuxHashSizeStr, AuxRowStr, Hash3Str, FastBytesStr);
}

// Конструирует объект типа TORNADO_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_TORNADO (char** parameters)
{
  if (strcmp (parameters[0], "tor") == 0) {
    // Если название метода (нулевой параметр) - "tor", то разберём остальные параметры

    TORNADO_METHOD *p = new TORNADO_METHOD;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    // Переберём все параметры метода (или выйдем раньше при возникновении ошибки при разборе очередного параметра)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      switch (*param) {                    // Параметры, содержащие значения
        case 'b': p->m.buffer          = parseMem (param+1, &error); continue;
        case 'h': p->m.hashsize        = parseMem (param+1, &error); continue;
        case 'l': p->m.hash_row_width  = parseInt (param+1, &error); continue;
        case 'c': p->m.encoding_method = parseInt (param+1, &error); continue;
        case 'p': p->m.match_parser    = parseInt (param+1, &error); continue;
        case 'x': p->m.match_finder    = parseInt (param+1, &error); continue;
        case 's': p->m.hash3           = parseInt (param+1, &error); continue;
        case 'u': p->m.update_step     = parseInt (param+1, &error); continue;
        case 't': p->m.find_tables     = parseInt (param+1, &error); continue;
        case 'a': switch (param[1]) {      // Параметры ah/al
                    case 'h': p->m.auxhash_size       = parseMem (param+2, &error); continue;
                    case 'l': p->m.auxhash_row_width  = parseInt (param+2, &error); continue;
                  }
      }
      if (start_with(param,"fb"))   {p->m.fast_bytes = parseInt (param+2, &error); continue;}
      // Сюда мы попадаем, если в параметре не указано его название
      // Если этот параметр удастся разобрать как целое число (т.е. в нём - только цифры),
      // то будем считать, что это номер пресета, иначе попробуем разобрать его как buffer
      int n = parseInt (param, &error);
      if (!error && n < elements(std_Tornado_method))  p->m = std_Tornado_method[n];
      else                                             error=0, p->m.buffer = parseMem (param, &error);
    }

    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    return p;
  } else
    return NULL;   // Это не метод TORNADO
}

static int TORNADO_x = AddCompressionMethod (parse_TORNADO);   // Зарегистрируем парсер метода TORNADO
