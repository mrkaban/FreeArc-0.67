/*
    REP is an LZ77-family algorithm, i.e. it founds matches and outputs them as
    (len,offset) pairs. It is oriented toward very fast compression and small
    memory overhead (1/4 of buffer size), but limited to rather large values of
    mimimal match length (say, 32), and don't search for optimum match. It's
    intended to preprocess data before using full-fledged compressors. and in
    this area it beats RZIP and, to some degree, LZP preprocessors. Small
    memory overhead means that RZIP/LZP/REP are capable to find matches at very
    long distances and this algorithm does it much better than RZIP and LZP.
    The algorithm implemented in functions REPEncode() and REPDecode().

    Main differences comparing to RZIP:
    1) Sliding window which slides at 1/16 of buffer size each time
    2) Almost ideal hash function (see update_hash)
    3) Direct hashing without hash chains which 1.5x cuts memory requirements
    4) Tags are not saved in hashtable, which again halves memory requirements.
         Instead, a few lower bits of hash table entry are used to save a few
         bits of tag (see chksum)
    5) Hash size is proportional to buffer size (which is equal to the maximum
         search distance) and by default limited to 1/4 of buffer size
    6) In order to find strings of length >=MinLen, blocks of length L=MinLen/2
         are indexed via hash. Of all those possible blocks, only 1/sqrt(L) are
         indexed and only 1/sqrt(L) are searched. It is alternative to solution
         described in RZIP paper where 1/L of blocks are indexed and each block
         searched. This means that logb(sqrt(L)) lower bits of hash entry are
         zeroes which allows to use trick 4.


References for RZIP algorithm explanation and implementations:
http://samba.org/~tridge/phd_thesis.pdf
http://rzip.samba.org/ftp/rzip/rzip-2.1.tar.gz
http://ck.kolivas.org/apps/lrzip/lrzip-0.18.tar.bz2
http://www.edcassa-ict.nl/lrzip.zip
http://www.edcassa-ict.nl/rzip21.zip

TAYLOR, R., JANA, R., AND GRIGG, M. 1997. Checksum testing of remote
synchronisation tool. Technical Report 0627 (November), Defence Science and
Technology Organisation, Canberra, Australia. (p.72)


References for LZP algorithm implementations:
http://magicssoft.ru/content/download/GRZipII/GRZipIISRC.zip
http://www.compression.ru/ds/lzp.rar


** Detailed algorithm description in Russian **********************************************

    Этот алгоритм является разновидностью LZ77, т.е. он находит повторяющиеся
    строки во входных данных, и кодирует их как (len,offset). Его особенностью
    является ориентация на поиск совпадений достаточно большой длины на большом
    расстоянии. Поэтому он весьма эффективно использует память - как правило,
    для структур поиска требуется не более 25% от размера окна поиска. При этом
    он находит практически все совпадения если минимальная длина (MinLen)
    искомых строк - 512 байт, и порядка 98% - в одном моём эксперименте на
    поиск совпадений с длиной от 32 байт. На практике этот алгоритм нацелен на
    использование в качестве препроцессора, уменьшающего избыточность файла
    и/или находящего сопадения на таких дистанциях, которые недоступны
    основному алгоритму упаковки, и в этом качестве он конкурирует с такими
    алгоритмами, как LZP by Ilya Grebnev и RZIP. При этом, как показывают
    эксперименты, для препроцессора оптимальная величина минимальной искомой
    строки находится именно в этих пределах - 32-512 байт. Этот алгоритм
    находит куда больше совпадений, чем LZP/RZIP, и кроме того, его
    скорость работы увеличивается при увеличении MinLen.

    Алгоритм реализуется функциями REPEncode() и REPDecode(), и использует
    сочетание идей из LZP, RZIP и моих собственных. Поиск совпадений ведётся в
    скользящем окне - входные данные считываются блоками по 1/16 от размера
    буфера, и это означает что в любой момент времени как минимум 15/16 буфера
    содержат предыдущие данные, которые сканируются в поисках совпадений. Для
    упрощения алгоритма ни входные блоки, ни совпадения не могут пересекать
    границу буфера.

    Как обычно, для поиска строк с длиной от MinLen у каждого блока файла
    длиной MinLen вычисляется некая контрольная сумма (КС), которая заносится в
    хеш-таблицу. Поскольку алгоритм ориентирован на большие значения MinLen,
    быстрое вычисление КС от блоков такой длины является проблемой. Эта
    проблема решается использованием "скользящей КС", то есть такой, которую
    можно быстро пересчитать при добавлении нового байта в конец блока и
    удалении одного байта в начале (см. update_hash).

    Подбор наилучшей формулы для скользящего хеширования был отдельным
    приключением. В конце концов простая формула hash = p[-1] + PRIME*p[-2] +
    PRIME*PRIME*p[-3] + ..., где PRIME - простое число, оказалась самой быстрой
    и дающей весьма равномерное распределение. Разумеется, все вычисления идут
    по модулю 1<<32, любезно предоставленному нам процессором :)

    Далее, были использованы дополнительные меры для уменьшения требований к
    памяти и увеличения скорости. Рассмотрим к примеру работу алгоритма для
    MinLen=512. Поскольку любой 512-байтный блок включает в себя 256-байтный
    блок, начинающийся с позиции, кратной 256, то нам достаточно вставлять в
    хеш-таблицу ссылки только на эти блоки и искать совпадение только с ними.
    Разумеется, при проверке совпадения мы не ограничиваемся в точности 256
    байтами, а пытаемся продолжить его как можно дальше в обе стороны. Именно
    это и позволяет значительно уменьшить расход памяти при гарантированном
    нахождения почти всех совпадений - во всяком случае, когда MinLen
    достаточно велико.

    Однако можно пойти ещё дальше - вместо того, чтобы вставлять в хеш-таблицу
    каждый 256-й блок, но искать каждый-каждый, мы можем например вставлять
    каждый 32-й, а искать каждый 8-й, или вставлять каждый 2-й, а искать каждый
    128-й. Разумеется, оптимумом будет вставлять и искать каждый 16-й блок.
    Точнее говоря, нужно вставлять один блок через каждые 16 байт, а искать
    первые 16 блоков из каждых 256, то есть вставляем блоки, начинающиеся с
    позиций 0, 16, 32..., а ищем блоки, начинающиеся с позиций 0, 1, 2..., 15,
    256. 257... Таким образом, для MinLen=512 достигается 8-кратное ускорение
    работы (за счёт 8-кратного уменьшения количества обращений в память) по
    сравнению с прямолинейной реализацией - правда, за счёт увеличения
    требований к памяти (с 1/64 размера буфера до 1/4, что на мой взгляд вполне
    приемлемо).

    Наконец, последним трюком является использование младших битов записи в
    хеш-таблице для хранения нескольких бит из значения хеш-функции (chksum) -
    разумеется, тех, которые не являются частью индекса в хеш-таблице. Это
    позволяет отсеять большую часть ложных совпадений, не сравнивая содержимое
    блоков, и тем самым уменьшить количество обращений в память и ещё больше
    ускорить работу программы.

    В алгоритме используется хеширование с прямой адресацией, без вторичного
    хеширования, что делает реализацию очень простой. Значение хеш-функции
    от 256-байтного блока (в общем случае размер этого блока - L=MinLen/2)
    используется как индекс в хеш-таблице (hasharr[hash&HashMask]), при
    коллизиях новый блок просто заменяет более ранний. На практике это
    (практически) не ведёт к деградации компрессии. Ещё раз подчеркну, что
    этот алгоритм, в отличие от полноценных LZ77 реализаций, ищет не
    оптимальное (самое длинное) совпадение, а проверяет лишь одну ссылку - на
    последний блок, который занял этот хеш-слот, и чья КС, следовательно,
    предположительно совпадает с КС текущего блока.

    Размер хеша (HashSize): при разработке алгоритма я предполагал, что он
    должен быть в 2-4 раза больше количества элементов, которые в него придётся
    вставлять. Однако на практике оказалось, что вполне достаточно иметь то же
    самое кол-во слотов, а для MinLen=32 - даже вчетверо (!) меньшее. то есть,
    например, для 32 мб блока при MinLen=512 в хеш вставляется каждый 16-й
    256-байтный блок и общее количество вставляемых элементов - 32млн/16=2млн,
    т.е. 8 мб, и хеш создаётся именно такого размера. Для MinLen=32 общее
    количество элементов 32млн/4=8млн, но мы создаём хеш-таблицу вчетверо
    меньше, то есть получаются те же самые 8 мб. Таким образом, подбираемый
    алгоритмом автоматически размер хеш-таблицы никогда не превосходит 1/4
    размера входного буфера. Если вы хотите установить другое значение - то
    используйте параметр HashBits (опцию -h). Увеличение HashSize при небольших
    MinLen способно немного увеличить степень сжатия.

    Amplifier: как было описано выше, при поиске проверяется только часть
    блоков, которой бы с гарантией хватило для нахождения всех строк с длиной
    >=MinLen - будь у нас идеальное хеширование. Однако наше хеширование
    неидеально, и часть потенциальных хитов из-за этого теряется. Параметр
    Amplifier (опция -a) позволяет затребовать тестирование большего числа
    блоков (в эти самые Amplifier раз). Таким образом, для максимально
    тщательного поиска можно просто установить Amplifier в достаточно большое
    значение, скажем 99. Разумеется, это уменьшает скорость и лишь ненамного
    увеличивает сжатие.

    Barrier и SmallestLen: некоторые алгоритмы, в частности ppmd, выигрывают,
    если препроцессор использует меньшее значение MinLen для больших
    дистанций. Эти два параметра позволяют установить двухступенчатую границу
    отбора совпадений, например "в первом мегабайте - MinLen=128, далее
    MinLen=32" задаётся через MinLen=128, Barrier=1<<20, SmallestLen=32
    (опции -l128 -d1048576 -s32). При этом поиск строк настраивается, ес-но,
    на нахождение строк с длиной от SmallestLen вместо MinLen.


** Benchmarks using 1GHz processor ****************************************************************

Test results for 26mb:
        Compression time   Compressed size
-l8192  0.5 seconds
 -l512  1.1
 -l128  1.4
  -l32  2.5                12.7 mb
lrzip   2.6                14.1
lzp:h20 6.5                13.1
lzp:h13 3.0                20.6

Compression speed on incompressible data:
-l8192  52 mb/sec
 -l512  25 mb/sec
 -l128  17 mb/sec
  -l32   8 mb/sec
lrzip    8 mb/sec


** REP со словарём больше размера ОЗУ**************************************************************

кстати, раз уж разговор вертится вокруг rep с большими словарями. по умолчанию
в нём используется скажем 1гб для самих данных и вчетверо меньше - для индекса.
хранить историю данных приходится только потому, что простенький 4-байтный хеш,
который вычисляется от этих 512 байт - ненадёжен в смысле коллизий. теперь
представьте себе, что мы храним вместо него 16-байтный cryptographically strong
hash - типа md5. тогда историю можно вычеркнуть нафиг. более того, то что
хеш-таблица занимает четверть от объёма индексированных данных - это
необязательно. если хранить такой хеш для каждого 256-байтного блока данных, то
это гарантирует нам нахождение всех матчей длины от 511 (поскольку любой такой
матч включает как минимум один полный 256-байтный блок, начинающийся с
256-байтной границы). т.е. для поиска строк длины 511+ c N-гб историей
достаточно памяти в N/16 гб

проблемы возникнут только при распаковке :D  если при упаковке старые данные
нам и не нужны - достаточно иметь 100% уверенность в их совпадении, то при
распаковке нам как-никак надо их копировать со старого места :D  если считать,
что мы все эти данные будем хранить на диске вместо ОЗУ, то копирование каждой
строки потребует операции чтения с диска, накладные расходы на которую
практически равны disk seek time - т.е. 10 мс для винта и 1 мс для очень
хорошей флешки

представим себе, что мы хотим обеспечить скорость распаковки скажем 1 мб/с. это
означает, что каждые 10 мс мы должны распаковывать как минимум 10 кб, что в
свою очередь гарантировано только если у нас rep кодирует только совпадения
длины 10кб+

скажем, если остановиться на кодировании строк длины 4кб+, то для сжатия
потребуется N/128 гб памяти (т.е. твои 18 гиг можно прошерстить, используя
всего 160 мег озу) и скорость распаковки будет ограничена 400 кб/с. вот только
винт жалко :D

Ghost, попробуй для интереса - как меняется сжатие твоих данных при переходе от
rep:512 (по умолчанию) к rep:4096? с дальнейшей обработкой lzma и без оной.
понятно, это лишь прикидка, поскольку реально rep сейчас окучивать большие
дистанции не умеет :(  может, мне это дело добавить на быструю руку, без
возможности распаковки...

*/


// ХОЗЧАСТЬ ************************************************************************
#include "../Compression.h"


#ifdef REP_LIBRARY
#define stat1(nextmsg,size)
#else
void stat1 (char *nextmsg, int Size);
#endif


// ОПЦИИ КОМАНДНОЙ СТРОКИ **********************************************************************
#ifndef REP_LIBRARY
// Объем информации, выдаваемой на stdout
//   0   только ошибки
//   1   общая статистика
//   2   детальная информация о процессе
static int verbose = 0;
#endif


// ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ *********************************************************************

// Возведение в степень
template <class T>
T power (T base, unsigned n)
{
  T result = 1;
  while (n) {
    if (n % 2)  result*=base, n--;
    n /= 2;  base*=base;
  }
  return result;
}

// Наибольшая степень base, не превосходящая sqrt(n),
// например sqrtb(36,2) = 4
inline static unsigned sqrtb (unsigned n, unsigned base = 2)
{
    int result;
    for (result=1; (n/=base*base) != 0; result *= base);
    return result;
}

// Находит адрес начала совпадения, идя назад от *p и *q
static inline byte* find_match_start (byte* p, byte* q, byte* start)
{
    while (q>start)   if (*--p != *--q)  return q+1;
    return q;
}

// Находит адрес первого несовпадающего байта, идя вперёд от *p и *q
static inline byte* find_match_end (byte* p, byte* q, byte* end)
{
    while (q<end && *p==*q) p++,q++;
    return q;
}

// Копирует данные из буфера в буфер, идя в порядке возрастания адресов
// (это важно, поскольку буфера могут пересекаться и в этом случае нужно
// размножить существующие данные)
static inline void memcpy_lz_match (byte* p, byte* q, unsigned len)
{
    if (len)
    do *p++ = *q++;
    while (--len);
}


// ОСНОВНОЙ АЛГОРИТМ *********************************************************************

/*
    Для нахождения совпадений длиной от MinLen байт нужно заносить в хеш значения
    контрольной функции от блоков длиной L = MinLen/2 байт с частотой k = sqrt(L) байт.
    Искать в этой хеш-таблице совпадения для блоков, начинающихся в первых test=k байтах
    из каждого блока длиной L байт.
*/

#define update_hash(sub,add)                        \
{                                                   \
    hash = hash*PRIME + add - sub*PRIME_power_L;    \
}

const int PRIME = 153191;    // or any other large prime number
const int MAX_BLOCK = 8*mb;  // Макс. объём входных данных, читаемых за раз


// Вычислить количество элементов хеша
MemSize CalcHashSize (MemSize HashBits, MemSize BlockSize, int SmallestLen, int MinMatchLen, int ChunkSize, int Amplifier, int *L)
{
    // Мин. длина строк, совпадения в которых нужно находить
    int Len = mymin(SmallestLen,MinMatchLen);
    // Размер блоков, КС которых заносится в хеш
    *L = ChunkSize? ChunkSize : rounddown_to_power_of(Len-1,2)/2;
    // Размер хеша должен соответствовать 4*количество значений. которые мы хотим в него занести, но не превышать четверти от размера буфера / объёма входных данных (Size/16*sizeof(int)==Size/4)
    return HashBits>0? (1<<HashBits) : roundup_to_power_of(BlockSize/3*2,2) / mymax(*L/4,16);
}


#ifndef FREEARC_DECOMPRESS_ONLY

#define MULTI_THREADING_BASICS
#include "../MultiThreading.h"

struct Job
{
    int*   hashtable;         // Place for saving info about maximum hashes and their indeces
    byte*  buf;               // Buffer being hashed
    int    bytes;             // How much bytes to hash in buf[] (plus it needs a L-byte lookahead bytes after that)
    int    L;                 // Size of rolling hash AND number of positions among those we are looking for "local maxima"
                              //   (these may be different numbers in other implementations)

    // Index L-byte blocks starting at buf[0]..buf[bytes-1]
    void process()
    {
        int   PRIME_power_L = power(PRIME,L);
        int  *hashptr       = hashtable;
        byte *ptr           = buf;
        int   num_blocks    = bytes/L;
        if (num_blocks > 0)
        {
            int hash=0;  for (int i=0; i < L; i++)  update_hash (0, buf[i]);    // Initial hash value == hash of first L bytes of the buffer

            // Split buffer into L-byte blocks and store maximal hash for each block and its position to hashptr[]
            for (int block=0; block<num_blocks; block++) {
                int maxhash = hash, maxi = 0;
                for (int i=0; i<L; i++, ptr++) {
                    if unlikely(hash > maxhash)
                        maxhash = hash, maxi = i;
                    update_hash (ptr[0], ptr[L]);
                }
                *hashptr++ = maxhash;
                *hashptr++ = maxi;
            }
        }
    }
};


int rep_compress (unsigned BlockSize, int MinCompression, int ChunkSize, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, void *auxdata)
{
    // НАСТРОЙКА ПАРАМЕТРОВ АЛГОРИТМА
    if (SmallestLen>MinMatchLen)  SmallestLen=MinMatchLen;
    int L;   // Размер блоков, КС которых заносится в хеш
    int HashSize = CalcHashSize (HashBits, BlockSize, SmallestLen, MinMatchLen, ChunkSize, Amplifier, &L);
    int HashMask = HashSize-1;
    int *hasharr=NULL;  int errcode=FREEARC_OK;
    int DataEnd=0, last_i=0, last_match=0;    // last_match points to the end of last match written, we shouldn't start new match before it

    byte *buf = (byte*) BigAlloc(BlockSize);   // Буфер, куда будут помещаться входные данные
    if (buf==NULL)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;    // Error: not enough memory

    // Alloc hash array
    hasharr  = (int *) BigAlloc (HashSize * sizeof(int));
    if (HashSize && hasharr==NULL)  {BigFree(buf); return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;}   // Error: not enough memory
    memset (hasharr, 0, HashSize * sizeof(int));
    debug (verbose>0 && MinMatchLen==SmallestLen && printf(" Buf %d mb, MinLen %d, Hash %d mb, Amplifier %d\n", ((BlockSize-1)/mb)+1, MinMatchLen, (HashSize*sizeof(int))>>20, Amplifier));
    debug (verbose>0 && MinMatchLen!=SmallestLen && printf(" Buf %d mb, MinLen %d, Barrier %d, Smallest Len %d, Hash %d mb, Amplifier %d\n", ((BlockSize-1)/mb)+1, MinMatchLen, Barrier, SmallestLen, (HashSize*sizeof(int))>>20, Amplifier));

    FOPEN();

    // Буфера для отдельного хранения длин, смещений совпадений, длин несжатых блоков и самих этих блоков. Такое группирование позволяет увеличить конечную степень сжатия
    int bsize = (mymin(BlockSize,MAX_BLOCK)/SmallestLen+1) * sizeof(int32);    // Макс. объём данных, который может быть записан в любой из этих буферов
    Buffer lens(bsize), offsets(bsize), datalens(bsize), dataOffsets(bsize);

    // Приготовления к хешированию в фоновых потоках
    const int HTJOBS  = GetCompressionThreads(),     // количество одновременно выполняемых заданий по хешированию
              HTBUFS  = mymax(16,HTJOBS*2),          // количество буферов для сохранения результатов хеширования (минимум 16 для более равномерного распределения нагрузки)
              HTBLOCK = mymax(256*kb,L*4)/L*L,       // объём данных, обрабатываемых одним заданием хеширования
              HTCOUNT = HTBLOCK/L*2;                 // кол-во чисел, запоминаемых одним заданием хеширования (на каждые L байт запоминается наибольшее значение хеша и его индекс)
    int free_jobs     = HTBUFS;                      // количество свободных буферов для результатов хеширования
    int ht_index      = 0;                           // индекс очередного свободного буфера для результатов хеширования
    int next_job_i    = 0;
    int next_flush    = 0;
    int *ht = (int*) BigAlloc(HTBUFS*HTCOUNT*sizeof(*ht));
    MultipleProcessingThreads<Job> HashingThreads;  HashingThreads.MaxJobs = HTBUFS;  HashingThreads.NumThreads = HTJOBS;
    if (ht==NULL || HashingThreads.start()!=0)  {errcode=FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; goto finished;}   // Error: not enough memory


    // ГЛАВНЫЙ ЦИКЛ. КАЖДАЯ ИТЕРАЦИЯ КОДИРУЕТ ~8МБ ВХОДНЫХ ДАННЫХ.
   {bool DoItOnce       = true;   // Для однократной записи BlockSize в выходной буфер ПОСЛЕ чтения первых входных данных
    bool MoreInputData  = true;   // Продолжать чтение входных данных
    bool MoreHashedData = true;   // Продолжать получение хешированных блоков и их сжатие

    while (MoreHashedData) {
        int literals=0; lens.empty(), offsets.empty(), datalens.empty(), dataOffsets.empty();  // Очистим буфера
#ifdef DEBUG
        int match_cnt=0, matches=0;
#endif

        // Накапливаем сжатые данные, пока не обработаем очередные 8 мб входных данных
        next_flush += mymin (BlockSize-next_flush, MAX_BLOCK);
        while (last_i+L < next_flush) {   // Добавляем L байт потому, что первый хешируемый блок на L байт меньше

            // ЧТЕНИЕ ВХОДНЫХ ДАННЫХ И ХЕШИРОВАНИЕ ИХ В ФОНОВЫХ ПОТОКАХ
            while (free_jobs>0 && DataEnd<BlockSize && MoreInputData)
            {
                // Читаем очередные 256кб
                int Size;  READ_LEN(Size, buf+DataEnd, mymin (BlockSize-DataEnd, HTBLOCK));
                if (Size < 0)  {errcode=Size; goto finished;}         // Error: can't read input data
                if (DoItOnce)  {FWRITE4 (BlockSize); DoItOnce=false;} // Запишем размер словаря в выходной поток
                if (Size == 0)  MoreInputData = false;
                DataEnd += Size;                                      // Граница прочитанных данных

                // Отправляем прочитанные данные на хеширование (первый хешируемый блок на L байт меньше)
                int bytes =  DataEnd-(next_job_i+L);    if (bytes<=0)  {if (Size>0)  continue;  else bytes=0 /* no MoreHashedData signal */;}
                Job job   =  {ht+ht_index*HTCOUNT, buf+next_job_i, bytes, L};       // last byte accessed: buf[i+bytes+L-1] == buf[i+ DataEnd-(i+L) +L-1] == buf[DataEnd-1]
                HashingThreads.Put(job);
                debug (verbose>0 && printf(" Read (%x,%x),  Job[%d] (%x,%x),  free_jobs %d\n", DataEnd-Size, Size, ht_index, next_job_i, bytes, free_jobs));
                next_job_i += bytes;
                ht_index = (ht_index+1)%HTBUFS;
                free_jobs--;
            }
            // Получаем результаты хеширования для очередных 256 кб
            Job job = HashingThreads.Get();
            int *hashptr  = job.hashtable;
            int next_fill = job.buf+job.bytes-buf;
            free_jobs++;
            debug (verbose>0 && printf(" Hashed (%x,%x),  free_jobs %d\n", job.buf-buf, job.bytes, free_jobs));
            if (job.bytes==0)  {MoreHashedData=false; goto encode_data;}

            // ОСНОВНОЙ ЦИКЛ, НАХОДЯЩИЙ ПОВТОРЯЮЩИЕСЯ СТРОКИ ВО ВХОДНЫХ ДАННЫХ
            for ( ; last_i<next_fill; last_i+=L) {
                // Проверяем совпадение в лучшем на следующие L байт блоке тоже длины L
                int hash = *hashptr++;            // Считываем хеш и индекс этого блока, уже найденные одним из наших фоновых потоков
                int i    = *hashptr++ + last_i;
                if (i >= last_match) {            // Проверяем совпадение только если предыдущее найденное совпадение уже кончилось
                    int match = hasharr[hash&HashMask];
                    if (match) {
                        if (match>=i && match<DataEnd)  goto no_match;  // match попадает на ещё не обработанные данные, то есть он заведомо устарел
                        // Наименьшее/наибольшее значение, которое может принимать при поиске
                        // индекс базирующийся на i, чтобы индекс базирующийся на match,
                        // не вышел за пределы буфера и не заглянул в будущие данные
                        int LowBound  = match<i? i-match : match-DataEnd>i? 0 : i - (match-DataEnd);
                        int HighBound = BlockSize - match + i;
                        // Найдём реальные начало и конец совпадения, сравнивая вперёд и назад от buf[i] <=> buf[match]
                        // i ограничено снизу и сверху значениями last_match и DataEnd, соответственно
                        int start = find_match_start (buf+match, buf+i, buf+mymax(last_match,LowBound)) - buf;
                        int end   = find_match_end   (buf+match, buf+i, buf+mymin(DataEnd,HighBound)) - buf;
                        // start и end - границы совпадения вокруг i. Проверим, что найденное совпадение имеет длину >=MinMatchLen (или SmallestLen, если дистанция >Barrier)
                        if (end-start >= (i-match<Barrier? MinMatchLen : SmallestLen) ) {
                            int offset = i-match;  if (offset<0)  offset+=BlockSize;
                            // Совпадение найдено! Запишем информацию о нём в выходные буфера
                            dataOffsets.put32 (last_match);         // Адрес несжавшихся данных
                               datalens.put32 (start-last_match);   // Длина несжавшихся данных
                                offsets.put32 (offset);             // Смещение match'а
                                   lens.put32 (end-start);          // Длина match'а
                            // Запомнить позицию конца найденного совпадения и вывести отладочную статистику
                            debug ((match_cnt++, matches+=end-start));
                            debug (verbose>1 && printf ("Match %d %d %d  (lit %d)\n", -offset, start, end-start, start-last_match));
                            literals += start-last_match;  last_match=end;
                        }
                    }
                }
    no_match:   // Заносим в хеш-таблицу этот L-байтный блок
                hasharr[hash&HashMask] = i;
            }
        }

    encode_data:
        // ВЫВОД СЖАТЫХ ДАННЫХ В ВЫХОДНОЙ ПОТОК
        if (next_flush==BlockSize || !MoreHashedData)  last_i=DataEnd;   // Закодировать все оставшиеся / до конца буфера данные
        if (last_match > last_i) {                                       // Если последний матч кончается в ещё не проиндексированной области
          datalens.put32 (0);                                            //   Ничего кодировать не надо, но datalens должен всё равно быть ровно на одну запись длиннее lens/offsets
        } else {
          // Записать в выходные буфера остаток данных от последнего найденного совпадения до последней проиндексированной позиции
          dataOffsets.put32 (last_match);                                // Адрес остатка данных
             datalens.put32 (last_i-last_match);                         // Длина остатка данных
          literals  += last_i-last_match;
          last_match = last_i;
        }
        if (next_flush==BlockSize) {                                     // Если происходит переход через границу буфера
          DataEnd=last_match=last_i=next_job_i=next_flush=0;             //   Да! Начать заполнять буфер с начала!
        }
        // Записать размер сжатых данных и количество найденных совпадений в буфер
        int outsize = sizeof(int32)*2+lens.len()+offsets.len()+datalens.len()+literals;
        QUASIWRITE (outsize);
        FWRITE4 (outsize-sizeof(int32));
        FWRITE4 (lens.len()/sizeof(int32));
        // Вывести содержимое буферов и несжатые данные в выходной поток
        FWRITE (    lens.buf,     lens.len());
        FWRITE ( offsets.buf,  offsets.len());
        FWRITE (datalens.buf, datalens.len());
        dataOffsets.rewind(); datalens.rewind();
        while (!dataOffsets.eof()) {
            FWRITE (buf + dataOffsets.get32(), datalens.get32());
        }
        FFLUSH();
        // Отладочная статистика
        debug (verbose>0 && printf(" Compressed %d -> %d (%d + %d), %d bytes in %d matches, free_jobs %d\n", literals+matches, outsize, outsize-literals, literals, matches, match_cnt, free_jobs));
    }}


    // Записать финальный блок, содержащий несжавшийся остаток данных, и 0 - признак конца данных
   {int datalen = DataEnd-last_match;
    if (datalen) {
        FWRITE4 (sizeof(int32)*2 + datalen);  // Длина сжатого блока
        FWRITE4 (0);                          //   0 matches in this block
        FWRITE4 (datalen);                    //   Длина остатка данных
        FWRITE  (buf+last_match, datalen);    //   Сами эти данные
    }}
    FWRITE4 (0);                              //   EOF flag (see below)
finished:
    FCLOSE();
    BigFree(hasharr);
    if (errcode>=0) {                         // Only if we are sure that b/g threads are finished all their jobs
        HashingThreads.finish();
        BigFree(ht);
    }
    BigFree(buf);
    return errcode>=0? 0 : errcode;
}
#endif // FREEARC_DECOMPRESS_ONLY


// Classical LZ77 decoder with sliding window
int rep_decompress (unsigned BlockSize, int MinCompression, int ChunkSize, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode;
    byte *buf0 = NULL;
    MemSize bufsize, ComprSize;
    // Память для словаря может выделяться несколькими кусками чтобы не страдать от фрагментации адресного пространства
    const int MAX_BLOCKS = 100;                      // Макс. количество выделяемых буферов
    int       total_blocks = 0;                      // Реальное количество выделенных буферов
    byte     *datap[MAX_BLOCKS];                     // Адреса начал буферов, выделенных для словаря
    byte     *endp[MAX_BLOCKS];                      // Адреса концов буферов

    // Фактический размер словаря сохранён во входных данных
    READ4(BlockSize);

    // Цикл, выделяющий память для словаря отдельными кусками как можно большего размера
    {
    MemSize   cumulative_size[MAX_BLOCKS+1] = {0};   // Суммарный размер буферов, предшествующих i-му
    MemSize   remaining_size = BlockSize;            // Сколько ещё памяти нужно выделить для словаря
    while (remaining_size>0)
    {
        if (total_blocks >= MAX_BLOCKS)          ReturnErrorCode (FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);
        MemSize data_size = remaining_size;
        for(;;)
        {
            if (NULL  !=  (datap[total_blocks] = (byte*) BigAlloc (data_size)))  break;
            if (data_size <= 1*mb)   ReturnErrorCode (FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);
            data_size     -= 1*mb;
        }
        endp[total_blocks] = datap[total_blocks] + data_size;
        remaining_size -= data_size;
        total_blocks++;
        cumulative_size[total_blocks] = cumulative_size[total_blocks-1] + data_size;
    }

    // Буфер, куда будут помещаться входные данные
    bufsize = mymin(BlockSize,MAX_BLOCK)+1024;
    buf0 = (byte*) BigAlloc (bufsize);
    if (buf0==NULL)  ReturnErrorCode (FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);

    // Цикл, каждая итерация которого обрабатывает один блок сжатых данных
    int current_block = 0;            // Номер текущего буфера
    byte *start     = datap[0];       // Начало текущего буфера
    byte *last_data = datap[0];       // Начало незаписанной части данных
    byte *data      = datap[0];       // Текущий указатель данных
    byte *end       = endp[0];        // Конец текущего буфера     (start <= last_data <= data <= end)
    for(;;)
    {
        // Прочитаем один блок сжатых данных
        READ4(ComprSize);
        if (ComprSize == 0)  break;    // EOF flag (see above)

        if (ComprSize > bufsize)
        {
            BigFree(buf0); bufsize=ComprSize; buf0 = (byte*) BigAlloc(bufsize);
            if (buf0==NULL)  ReturnErrorCode (FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);
        }
        byte *buf = buf0;

        READ(buf, ComprSize);

        // Заголовок блока содержит размер таблиц lens/offsets/datalens; затем идут сами эти таблицы и наконец несжавшиеся данные
        int         num = *(int32*)buf;  buf += sizeof(int32);           // Количество совпадений (= количеству записей в таблицах lens/offsets/datalens)
        int32*     lens =  (int32*)buf;  buf += num*sizeof(int32);
        int32*  offsets =  (int32*)buf;  buf += num*sizeof(int32);
        int32* datalens =  (int32*)buf;  buf += (num+1)*sizeof(int32);   // Точнее, datalens содержит num+1 записей

        // Каждая итерация этого цикла копирует одну строку несжатых данных и один match, которые interleaved в нашей реализации процесса упаковки
        for (int i=0; ; i++) {
            int len = datalens[i];
            // Пока копируемая строка пересекает границу буфера
            while (end-data < len)
            {   //printf("  str: %d %d %d %d\n", BlockSize, data-datap[0], end-data, len);

                // Копируем влезающий кусочек и записываем заполненный буфер
                int bytes = end-data;
                memcpy (data, buf, bytes);  buf += bytes;  data += bytes;  len -= bytes;

                WRITE(last_data, data-last_data);

                // Место в этом буфере кончилось, переходим на следующий
                if (++current_block >= total_blocks)   current_block = 0;
                last_data = start = data = datap[current_block];
                end = endp[current_block];
            }
            // Копирование без пересечения границы буфера
            memcpy (data, buf, len);  buf += len;  data += len;


            if (i==num)  break;   // В самом конце у нас ещё один блок несжавшихся данных (возможно, нулевой длины) без парного ему lz-матча


            int offset = offsets[i];  len = lens[i];
            debug (verbose>1 && printf ("Match %d %d %d\n", -offset, data-start+cumulative_size[current_block], len));
            // Пока одна из копируемых строк пересекает границу буфера
            while ((offset > data-start && len)  ||  end-data < len)
            {
                MemSize dataPos = data-start+cumulative_size[current_block];   // Absolute position of LZ dest
                MemSize fromPos = dataPos-offset;  int k;                      // Absolute position of LZ src
                if (offset<=dataPos) {
                    // Копируемая строка имеет меньшее смещение, чем текущая позиция
                    for (k=current_block;  fromPos < cumulative_size[k]; k--);  // ищем блок, которому принадлежит копируемая строка
                } else {
                    // Копируемая строка имеет большее смещение, чем текущая позиция
                    fromPos += BlockSize;
                    for (k=current_block;  fromPos >= cumulative_size[k+1];  k++);  // ищем блок, которому принадлежит копируемая строка
                }
                byte *from    = fromPos - cumulative_size[k] + datap[k];       // Memory address of LZ src
                byte *fromEnd = endp[k];                                       // End of membuf containing LZ src

                int bytes = mymin(len, mymin(end-data, fromEnd-from));  // How much bytes we can copy without overrunning src or dest buffers
                //printf("? %d-%d=%d %d(%d %d)\n", dataPos, offset, fromPos, bytes, end-data, fromEnd-from);

                // Копируем влезающий кусочек
                memcpy_lz_match (data, from, bytes);  data += bytes;  len -= bytes;

                // Если dest буфер кончился - записываем заполненный буфер, и переключаемся на другой
                if (data==end)
                {
                    WRITE(last_data, data-last_data);

                    if (++current_block >= total_blocks)   current_block = 0;
                    last_data = start = data = datap[current_block];
                    end = endp[current_block];
                }
            }
            // Копирование без пересечения границы буфера
            memcpy_lz_match (data, data-offset, len);  data += len;
        }

        // Вывод распакованных данных, печать отладочной статистики и подготовка к следующей итерации цикла
        WRITE(last_data, data-last_data);
        debug (verbose>0 && printf( " Decompressed: %u => %u bytes\n", ComprSize+sizeof(int32), data-last_data) );
        last_data = data;
        // NB! check that buf==buf0+Size, data==data0+UncomprSize, and add buffer overflowing checks inside cycle
    }
    errcode = FREEARC_OK;}
finished:
    BigFree(buf0);
    for(int i=total_blocks-1; i>=0; i--)
        BigFree(datap[i]);
    return errcode;
}


/* to do:
+1. sliding window, In() function to read data
+2. освобождение памяти, втч. при ошибках
+3. save pointers to unmatched blocks instead of copying data
4. Проверить, что блок плохо упаковался, и заменить его одним литералом.
     Точнее, возвратить прежнее значение last_match и очистить все выходные буфера
5. last_small_match - если маленький match найден на небольшом расстоянии (<Barrier),
     то игнорировать маленькие матчи на больших расстояних (>Barrier) пока этот не кончится.
     Это позволит нам перестать отбирать хлеб у больших ребят :)
6. -l8192 -s512
7. buffer data for Out() in 256k blocks

http://forum.ru-board.com/topic.cgi?forum=5&topic=35164&start=1120#13
-хеш: обновлять на 4-16 байт за раз
+хеш: считать предварительно в заднем потоке
a99: уменьшать HTBUFS до 1-2, иначе мы выбиваемся из кеша!
производить все обращения к памяти заранее в заднем потоке
производить все чтения/записи данных в задних потоках
просчитывать chksum/i+chksum и hasharr+hash&HashMask в заднем потоке
  использовать все свободные биты для chksum (например, rep:1g:32:c16 - два бита в начале и два в конце)
  делать префетч по этим просчитанным адресам на 1-4 L-байтных блока вперёд: __builtin_prefetch (hashptr[K], 0/1, 0-3);
  упростить и развернуть эти циклы, сделать для них template по <test,k,L>
при поиске границ матча сравнивать по 4+ байта
помечать хеши 256-байтных строк в последних 512 мб в однобитной таблице, сегментируя её на 8-32 бита
в идеале - найти способ реализовать rep:32 таким же быстрым, как rep:512
опционально - интегрировать его в tor:3 (в основном это имеет смысл, если удастся сделать быстрый rep:32)
? if (i>=last_match) -> if (i+L>=last_match)  -- поскольку в следующие L байт шанса проверить матч у нас не будет...

  anchored hashing:
хешировать/проверять блоки длины меньше L, чтобы увеличить вероятность попадания на матч
запоминать N наибольших значений хеша внутри блока
хранить M значений в каждой строке хеш-таблицы, сдвигая их при обновлении строки (а-ля HT4)
  оптимизация:
? I/O в основном треде, сжатие в b/g треде (два набора выходных буферов lens/offsets/...)
__builtin_prefetch для адресов в hasharr[] - вычислять, делать prefetch и ненадолго сохранять их в 8-элементном массиве

Fixed bugs:
1. Проверка выхода за границу буфера: offset<data-data0 вместо <=
2. last_match не обнулялся при выходе из цикла при DataEnd=0 и Size=0
*/
