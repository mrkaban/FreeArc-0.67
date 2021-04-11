#include <stdio.h>
#include <string.h>
extern "C" {
#include "C_External.h"
}
#include "../LZMA2/MultiThreading/Thread.h"
#include "../LZMA2/MultiThreading/Synchronization.h"

// Is this command waits for preceding compression methods to be finsihed before starting succeeding ones
bool IsMemoryBarrier (char *cmd)
{
  return (strstr(cmd, " <stdin>") == NULL)
      || (strstr(cmd, " <stdout>")== NULL);
}

char *prepare_cmd (EXTERNAL_METHOD *p, char *cmd, bool *write_stdin, bool *read_stdout)
{
    // Replace "{options}" or "{-option }" in packcmd with string like "-m48 -r1 " (for "pmm:m48:r1" method string)
    char *OPTIONS_STR = "{options}",  *OPTION_STR = "option";
    char OPTIONS_START = '{',  OPTIONS_END = '}';

    // Params of option template in cmd line
    char before[MAX_METHOD_STRLEN] = "-";
    char after[MAX_METHOD_STRLEN]  =  " ";
    char *replaced = strstr (cmd, OPTIONS_STR);
    int  how_many  = strlen (OPTIONS_STR);

    // If there is no "{options}" in cmd - look for "{...option...}"
    if (!replaced)
    {
        // search for '{'
        for (char *p1 = cmd; *p1; p1++)
        {
            if (*p1 == OPTIONS_START)
            {
                // search for '}'
                char *p2 = p1, *p12 = NULL;
                for (; *p2; p2++)
                {
                    if (*p2 == OPTIONS_END)  break;
                    if (start_with(p2, OPTION_STR))  p12 = p2;
                }
                // if we have "option" inside of "{...}"
                if (*p2==OPTIONS_END && p12)
                {
                    // Save strings before and after "option" and how many chars in cmd to replace
                    strncopy (before, p1+1, p12-p1-1 + 1);
                    strncopy (after,  p12+strlen(OPTION_STR), p2-p12-strlen(OPTION_STR) + 1);
                    replaced = p1;
                    how_many = p2-p1+1;
                    break;
                }
            }
        }
    }

    // If we've found any option template in cmd
    if (replaced)
    {
        // Collect in param_str options in cmd format
        char param_str[MAX_METHOD_STRLEN] = "";
        for (char **opt = p->options; *opt; opt++)
        {
            strcat (param_str, before);
            strcat (param_str, *opt);
            strcat (param_str, after);
        }
        // Finally replace template with collected or default options
        cmd = str_replace_n (cmd, replaced, how_many, *p->options? param_str : p->defaultopt);
    } else {
        cmd = strdup_msg (cmd);
    }

    // Теперь уберём из команд ссылки на stdin/stdout и взведём соответствующие флаги
    char *cmd1 = str_replace (cmd,  " <stdin>", "");   *write_stdin = strstr(cmd, " <stdin>") != NULL;
    char *cmd2 = str_replace (cmd1, " <stdout>", "");  *read_stdout = strstr(cmd, " <stdout>")!= NULL;
    delete cmd;
    delete cmd1;
    return cmd2;
}


enum {READ=0, WRITE=1};

struct MYPIPE
{
    int mode;    // READ or WRITE
    int fdStd;   // stdin/stdout
    int fdDup;   // temporary saved copy of fdStd
    int fd;      // file descriptor for data exchange with child process

    MYPIPE (int _fdStd, int _mode)  {mode = _mode;  fdStd = _fdStd;  fd = -1;}

    int GetFd()
    {
        int fdPipe[2];
#ifdef FREEARC_WIN
        if (_pipe(fdPipe, LARGE_BUFFER_SIZE, O_BINARY | O_NOINHERIT) == -1)
#else
        if (pipe(fdPipe) != 0)
#endif
                                                    return 1;  // Create the pipe
        fdDup = dup(fdStd);                                    // Duplicate stdXXX file descriptor (next line will close original)
        if (dup2 (fdPipe[mode], fdStd) != 0)        return 2;  // Duplicate MODE end of pipe to stdXXX file descriptor
        close (fdPipe[mode]);                                  // Close original MODE end of pipe
        fd = fdPipe[READ+WRITE-mode];               return 0;  // Save file descriptor for data exchange with child process
    }

    void Cleanup()
    {
        if (fd >= 0) {                                          // If GetFd() was executed
            dup2(fdDup, fdStd);                                 //     Duplicate copy of original stdXXX back into stdXXX
            close(fdDup);                                       //     Close duplicate copy of original stdXXX
        }
    }

    void Close()
    {
        if (fd >= 0) {                                          // If GetFd() was executed
            close (fd);                                         //     Close file descriptor used for data exchange with child process
        }
    }
};

struct CLOSE_FD
{
    int fdStd;   // stdin/stdout
    int fdDup;   // temporary saved copy of fdStd

    CLOSE_FD (int _fdStd)  {fdStd = _fdStd;  fdDup = -1;}

    void Close()
    {
        fdDup = dup(fdStd);                                     // Duplicate stdXXX file descriptor (next line will close original)
        close (fdStd);                                          // Close original MODE end of pipe
    }

    void Restore()
    {
        if (fdDup >= 0) {                                       // If Close() was executed
            dup2(fdDup, fdStd);                                 //     Duplicate copy of original stdXXX back into stdXXX
            close(fdDup);                                       //     Close duplicate copy of original stdXXX
        }
    }
};


struct Waiter
{
    MYPIPE WriterPipe, ReaderPipe;  CLOSE_FD CloseStdErr;
    Thread WriterThread, ReaderThread;
    COMPRESSION direction; int useHeader; CALLBACK_FUNC *callback; void *auxdata;
    bool write_stdin, read_stdout;             // Записывать в stdin/читать stdout запущенной команды вместо передачи данных в файлах
    volatile int errcode;

    bool be_quiet()  {return !debug_mode && write_stdin && read_stdout;}

    Waiter (COMPRESSION _direction, int _useHeader, CALLBACK_FUNC *_callback, void *_auxdata)
        : WriterPipe(0/*stdin*/, READ)
        , ReaderPipe(1/*stdout*/, WRITE)
        , CloseStdErr(2/*stderr*/)
        { direction=_direction; useHeader=_useHeader; callback=_callback; auxdata=_auxdata; errcode=0; }
};

// Thread writing to external program's stdin
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE WriteToExternalProgram (void *param)
{
    Waiter *w = (Waiter*) param;  CALLBACK_FUNC *callback = w->callback;  void *auxdata = w->auxdata;
    int x;                                                // код, возвращённый последней операцией чтения/записи
    BYTE* Buf = (BYTE*) malloc_msg(LARGE_BUFFER_SIZE);
    while ( (x = callback ("read", Buf, LARGE_BUFFER_SIZE, auxdata)) > 0 )
    {
        if (write(w->WriterPipe.fd,Buf,x) != x)           {x=FREEARC_ERRCODE_WRITE; goto finished;}
    }
finished:
    FreeAndNil(Buf);
    if (x<0 && w->errcode==0)    // x>=0, если всё в порядке, и код ошибки иначе
      w->errcode = x;
    return 0;
}

// Thread reading from external program's stdout
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE ReadFromExternalProgram (void *param)
{
    Waiter *w = (Waiter*) param;  CALLBACK_FUNC *callback = w->callback;  void *auxdata = w->auxdata;
    int x;                                                // код, возвращённый последней операцией чтения/записи
    BYTE* Buf = (BYTE*) malloc_msg(LARGE_BUFFER_SIZE);
    BYTE compressed[1] = {1};
    if (w->direction==COMPRESS && w->useHeader)   checked_write(compressed,1);
    while ((x = read (w->ReaderPipe.fd, Buf, LARGE_BUFFER_SIZE)) > 0)
    {
        checked_write (Buf, x);
    }
finished:
    FreeAndNil(Buf);
    if (x<0 && w->errcode==0)    // x>=0, если всё в порядке, и код ошибки иначе
      w->errcode = x;
    return 0;
}

// Пока идёт процесс манипуляции с stdin/stdout, другим сюда лучше не соваться (включая индикатор прогресса в хаскельной части)
Mutex SynchronizeConio;
void SynchronizeConio_Enter(void) {SynchronizeConio.Enter();}
void SynchronizeConio_Leave(void) {SynchronizeConio.Leave();}

// Подготавливает stdin/stdout/stderr потоки к запуску внешней программы
int StartThreads (void *param)
{
    Waiter *w = (Waiter*) param;
    SynchronizeConio_Enter();
    if (w->write_stdin)  w->WriterPipe.GetFd();
    if (w->read_stdout)  w->ReaderPipe.GetFd();
    if (w->be_quiet())   w->CloseStdErr.Close();
    return 0;
}

// Восстанавливает stdin/stdout/stderr и выполняет треды, производящие обмен с запущенной программой
int WaitThreads (void *param)
{
    Waiter *w = (Waiter*) param;
    if (w->write_stdin)  w->WriterPipe.Cleanup();
    if (w->read_stdout)  w->ReaderPipe.Cleanup();
    if (w->be_quiet())   w->CloseStdErr.Restore();
    SynchronizeConio_Leave();
    if (w->write_stdin)  w->WriterThread.Create (WriteToExternalProgram,  w);
    if (w->read_stdout)  w->ReaderThread.Create (ReadFromExternalProgram, w);
    if (w->write_stdin)  w->WriterThread.Wait(),  w->WriterPipe.Close();     // Сначала закрываем поток данных из нашей программы во внешнюю
    if (w->read_stdout)  w->ReaderThread.Wait(),  w->ReaderPipe.Close();     // И только затем ждём завершения потока данных из внешней программы в нашу
    return 0;
}


/*-------------------------------------------------*/
/* Реализация класса EXTERNAL_METHOD               */
/*-------------------------------------------------*/

// Упаковка/распаковка
int EXTERNAL_METHOD::DeCompress (COMPRESSION direction, CALLBACK_FUNC *callback, void *auxdata)
{
    MYDIR t;  if (!t.create_tempdir())  return FREEARC_ERRCODE_WRITE;
    MYFILE infile (t, direction==COMPRESS? datafile : packedfile);   infile.mark_as_temporary();
    MYFILE outfile(t, direction==COMPRESS? packedfile : datafile);  outfile.mark_as_temporary();

    Waiter w(direction, useHeader, callback, auxdata);
    char *cmd = prepare_cmd (this, direction==COMPRESS? packcmd : unpackcmd, &w.write_stdin, &w.read_stdout);

    BYTE* Buf = NULL;                                     // буфер, используемый для чтения/записи данных
    int x;                                                // код, возвращённый последней операцией чтения/записи
    int ExitCode = 0;                                     // код возврата внешней программы

    // Перепишем входные данные во временный файл
    infile.remove();
    uint64 bytes = 0;
    BYTE runCmd = 1;
    if (direction==DECOMPRESS && useHeader)  checked_read (&runCmd, 1);
    if (!w.write_stdin)
    {
      Buf = (BYTE*) malloc_msg(LARGE_BUFFER_SIZE);
      while ( (x = callback ("read", Buf, LARGE_BUFFER_SIZE, auxdata)) > 0 )
      {
          if (!infile.isopen()) {// Не открываем файл пока не прочтём хоть сколько-нибудь данных (для решения проблем с перепаковкой солид-блоков)
              if (!infile.tryOpen(WRITE_MODE))           {x=FREEARC_ERRCODE_WRITE; goto finished;}
          }
          if (runCmd!=0 && runCmd!=1) {// Для совместимости со старыми версиями FreeArc, которые не добавляли 1 перед сжатыми данными (убрать из FreeArc 1.0!)
              outfile = "data7777";
              bytes += 1;
              if (write(infile.handle,&runCmd,1) != 1)   {x=FREEARC_ERRCODE_WRITE; goto finished;}
              runCmd = 1;
          }
          bytes += x;
          if (write(infile.handle,Buf,x) != x)           {x=FREEARC_ERRCODE_WRITE; goto finished;}
      }
      FreeAndNil(Buf);
      infile.close();
      if (x)  goto finished;   // Если при чтении/записи произошла ошибка - выходим
    }

    // Если cmd пусто - диск используется просто для буферизации данных перед дальнейшим сжатием.
    // Если runCmd==0 - данные были скопированы без сжатия
    outfile.remove();
    if (*cmd && runCmd) {
        MYFILE _tcmd(cmd); // utf8->utf16 conversion
        char temp[100];
        if (!w.be_quiet())   printf ("\n%s %s bytes with %s\n", direction==COMPRESS? "Compressing":"Unpacking", show3(bytes,temp), cmd);

        StartThreads(&w);  double time0 = GetGlobalTime();
        ExitCode = RunCommand (_tcmd.filename, t.filename, TRUE, WaitThreads, &w);
        addtime += GetGlobalTime() - time0;

        if (!w.be_quiet())                  printf ("\nErrorlevel=%d\n", ExitCode);
        if (w.errcode < 0)                  {x=w.errcode; goto finished;}
        if (w.read_stdout)                  {x=ExitCode?FREEARC_ERRCODE_GENERAL:0; goto finished;}
    } else {
        infile.rename (outfile);
    }

    // Откроем выходной файл, если команда завершилась успешно и его можно открыть
    if(ExitCode==0)    outfile.tryOpen (READ_MODE);
    if (outfile.isopen()) {
        infile.remove();
        BYTE compressed[1] = {1};
        if (direction==COMPRESS && useHeader)            checked_write(compressed,1);
    } else {
        if (direction==COMPRESS && !useHeader)           {x=FREEARC_ERRCODE_GENERAL; goto finished;}
        outfile.remove();
        if (direction==DECOMPRESS)                       {x=FREEARC_ERRCODE_INVALID_COMPRESSOR; goto finished;}
        infile.rename (outfile);
        if (!outfile.tryOpen (READ_MODE))                {x=FREEARC_ERRCODE_READ; goto finished;}
        BYTE uncompressed[1] = {0};
        if (direction==COMPRESS)                         checked_write(uncompressed,1);
    }

    // Прочитаем выходные данные из файла
    QUASIWRITE (outfile.size());
    Buf = (BYTE*) malloc_msg(LARGE_BUFFER_SIZE);
    while ((x = read (outfile.handle, Buf, LARGE_BUFFER_SIZE)) > 0)
    {
        checked_write (Buf, x);
    }
finished:
    FreeAndNil(Buf);
    delete cmd;
    return x;         // 0, если всё в порядке, и код ошибки иначе
}


// Универсальный метод: возвращаем различные простые характеристики метода сжатия
int EXTERNAL_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
    if      (strequ (what,"external?"))                     return 1;
    else if (strequ (what,"nosolid?"))                      return !solid;
    else if (strequ (what,"MemoryBarrierCompression?"))     return IsMemoryBarrier(packcmd);
    else if (strequ (what,"MemoryBarrierDecompression?"))   return IsMemoryBarrier(unpackcmd);
    else return COMPRESSION_METHOD::doit (what, param, data, callback);
}

#ifndef FREEARC_DECOMPRESS_ONLY

double myround (double x)  {return floor(x+0.5);}

// Изменить потребность в памяти, заодно оттюнинговав order
void EXTERNAL_METHOD::SetCompressionMem (MemSize _mem)
{
    if (can_set_mem && _mem>0) {
        order  +=  int (myround (log(double(_mem)/cmem) / log(2.0) * 4));
        cmem=dmem=_mem;
    }
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_EXTERNAL)
void EXTERNAL_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
    if (strequ (name, "pmm")) {
        char MemStr[100];
        showMem (cmem, MemStr);
        sprintf (buf, "pmm:%d:%s%s", order, MemStr, MRMethod==2? ":r2": (MRMethod==0? ":r0":""));
    } else {
        strcpy (buf, name);
        for (char** opt=options; *opt; opt++)
        {
            strcat(buf, ":");
            strcat(buf, *opt);
        }
    }
}

// Конструирует объект типа EXTERNAL_METHOD/PPMonstr с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_PPMONSTR (char** parameters)
{
  // Если название метода (нулевой параметр) - "pmm", то разберём остальные параметры
  if (strcmp (parameters[0], "pmm") == 0) {
    // Дефолтные значения параметров для метода сжатия PPMonstr
    EXTERNAL_METHOD *p = new EXTERNAL_METHOD;
    p->name           = "pmm";
    p->MinCompression = 100;
    p->can_set_mem    = true;
    p->order          = 16;
    p->cmem           = 192*mb;
    p->dmem           = 192*mb;
    p->MRMethod       = 1;
    p->datafile       = "$$arcdatafile$$.tmp";
    p->packedfile     = "$$arcdatafile$$.pmm";

    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    // Переберём все параметры метода (или выйдем раньше при возникновении ошибки при разборе очередного параметра)
    while (*++parameters && !error)
    {
      char *param = *parameters;
      if (start_with (param, "mem")) {
        param+=2;  // Обработать "mem..." как "m..."
      }
      if (strlen(param)==1) switch (*param) {    // Однобуквенные параметры
        case 'r':  p->MRMethod = 1; continue;
      }
      else switch (*param) {                    // Параметры, содержащие значения
        case 'm':  p->cmem = p->dmem = parseMem (param+1, &error); continue;
        case 'o':  p->order          = parseInt (param+1, &error); continue;
        case 'r':  p->MRMethod       = parseInt (param+1, &error); continue;
      }
      // Сюда мы попадаем, если в параметре не указано его название
      // Если этот параметр удастся разобрать как целое число (т.е. в нём - только цифры),
      // то присвоим его значение полю order, иначе попробуем разобрать его как mem
      int n = parseInt (param, &error);
      if (!error) p->order = n;
      else        error=0, p->cmem = p->dmem = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода

    // Создаёт packcmd/unpackcmd для PPMonstr
    char cmd[100];
    sprintf (cmd, "ppmonstr e -o%d -m%d -r%d %s", p->order, p->cmem>>20, p->MRMethod, p->datafile);
    p->packcmd = strdup_msg(cmd);
    sprintf (cmd, "ppmonstr d %s", p->packedfile);
    p->unpackcmd = strdup_msg(cmd);

    return p;
  } else {
    return NULL;   // Это не метод PPMONSTR
  }
}

static int PPMONSTR_x = AddCompressionMethod (parse_PPMONSTR);   // Зарегистрируем парсер метода PPMONSTR




// ПОДДЕРЖКА ПРОИЗВОЛЬНЫХ ВНЕШНИХ УПАКОВЩИКОВ **********************************************************************

// Конструирует объект типа EXTERNAL_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_EXTERNAL (char** parameters, void *method_template)
{
  if (strequ (parameters[0], ((EXTERNAL_METHOD*)method_template)->name)) {
    // Если название метода (нулевой параметр) соответствует названию проверяемого EXTERNAL метода, то разберём остальные параметры
    EXTERNAL_METHOD *p = new EXTERNAL_METHOD (*(EXTERNAL_METHOD*)method_template);

    // Копируем параметры метода внутрь нашего объекта
    char **param = parameters+1, **opt = p->options, *place = p->option_strings;
    while (*param)
    {
      strcpy (place, *param++);
      *opt++ = place;
      place += strlen(place)+1;
    }
    *opt++ = NULL;

    return p;
  } else {
    return NULL;   // Это не метод EXTERNAL
  }
}


// Добавить в таблицу методов сжатия описанный пользователем в arc.ini внешний упаковщик.
// params содержит описание упаковщика из arc.ini. Возвращает 1, если описание корректно.
// Пример описания:
//   [External compressor: ccm123, ccmx123, ccm125, ccmx125]
//   mem = 276
//   packcmd   = {compressor} c $$arcdatafile$$.tmp $$arcpackedfile$$.tmp
//   unpackcmd = {compressor} d $$arcpackedfile$$.tmp $$arcdatafile$$.tmp
//   datafile   = $$arcdatafile$$.tmp
//   packedfile = $$arcpackedfile$$.tmp
//
int AddExternalCompressor (char *params)
{
    // Разобьём описание метода сжатия на отдельные строки, хранящие его заголовок и параметры
    char  local_method [MAX_EXTERNAL_COMPRESSOR_SECTION_LENGTH];
    strncopy (local_method, params, MAX_METHOD_STRLEN);
    char* parameters [MAX_PARAMETERS];
    split (local_method, '\n', parameters, MAX_PARAMETERS);

    // Проверим, что первая строка - заголовок секции [External compressor]
    if (last_char(parameters[0])=='\r')  last_char(parameters[0]) = '\0';
    if (! (start_with (parameters[0], "[External compressor:")
           && end_with (parameters[0], "]")))
      return 0;

    // Извлечём из заголовка секции имена версий программы
    char *versions_list = strdup_msg (strchr(parameters[0],':')+1);
    last_char(versions_list) = '\0';
    char* version_name [MAX_COMPRESSION_METHODS];
    int versions_count = split (versions_list, ',', version_name, MAX_COMPRESSION_METHODS);

    // Для каждой версии создаём отдельный объект EXTERNAL_METHOD
    EXTERNAL_METHOD *version  =  new EXTERNAL_METHOD[versions_count];
    for (int i=0; i<versions_count; i++) {
        // Инициализируем шаблон EXTERNAL_METHOD именем очередной версии и параметрами по умолчанию
        version[i].name           = trim_spaces(version_name[i]);
        version[i].MinCompression = 100;
        version[i].can_set_mem    = false;
        version[i].cmem           = 0;
        version[i].dmem           = 0;
        version[i].datafile       = "$$arcdatafile$$.tmp";
        version[i].packedfile     = "$$arcpackedfile$$.tmp";
        version[i].packcmd        = "";
        version[i].unpackcmd      = "";
        version[i].defaultopt     = "";
        version[i].solid          = 1;
        version[i].useHeader      = 1;
    }


    // Теперь заполним эти шаблоны по описанию упаковщика, предоставленному пользователем
    // (команды упаковки/распаковки, требования к памяти и так далее).
    for (char **param=parameters;  *++param; ) {
        // Обработаем строку описания, разбив её на левую часть до '='
        // c названием параметра и правую часть с его значением
        char *s = *param;
        if (last_char(s)=='\r')  last_char(s) = '\0';  // На случай обработки файла с '\r\n' разделителями
        if (*s=='\0' || *s==';')  continue;  // Пропустим целиком пустую строку / строку комментариев
        while (*s && isspace(*s))  s++;   // Пропустим начальные пробелы в строке
        char *left = s;                   // Заякорим начало левой части (имени) параметра
        while (*s && !isspace(*s) && *s!='=')  s++;   // Найдём конец имени
        if (*s=='\0')  return 0;
        if (*s!='=') {                         // Пропустим пробелы после имени, если нужно
            *s++ = '\0';
            while (*s && isspace(*s))  s++;
            if (*s!='=')  return 0;
        }
        *s++ = '\0';                           // Поставим '\0' после имени
        while (*s && isspace(*s))  s++;        // Пропустим пробелы в начале правой части (значении)
        if (*s=='\0')  return 0;
        char *right = s;                       // Заякорим начало значения

        // Теперь left содержит левую часть строки (до '=') без пробелов,
        // а right - правую часть без начальных пробелов.
        // Переберём все версии компрессора и обновим в них соответствующее поле
        for (int i=0; i<versions_count; i++) {
            int error = 0;  // Признак того, что при разборе параметров произошла ошибка
                 if (strequ (left, "mem"))         version[i].cmem = version[i].dmem = parseInt (right,&error)*mb;
            else if (strequ (left, "cmem"))        version[i].cmem        = parseInt (right,&error)*mb;
            else if (strequ (left, "dmem"))        version[i].dmem        = parseInt (right,&error)*mb;
            else if (strequ (left, "packcmd"))     version[i].packcmd     = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "unpackcmd"))   version[i].unpackcmd   = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "datafile"))    version[i].datafile    = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "packedfile"))  version[i].packedfile  = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "default"))     version[i].defaultopt  = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "solid"))       version[i].solid       = parseInt (right, &error);
            else if (strequ (left, "header"))      version[i].useHeader   = parseInt (right, &error);
            else                                   error=1;

            if (error)  return 0;
        }
    }


    // Наконец, зарегистрируем парсер EXTERNAL метода сжатия, использующий эти шаблоны
    // для распознавания новых методов сжатия и получения всех необходимых сведений
    // о том, какие команды нужно вызывать для его реализации, через какие файлы
    // передавать данные и т.д.
    for (int i=0; i<versions_count; i++) {
        AddExternalCompressionMethod (parse_EXTERNAL, &version[i]);
    }
    return 1;
}

// Псевдо-метод сжатия, записывающий все получаемые им данные в файл, и затем считывающий его.
// Автоматически вставляется между жрущими много памяти алгоритмами, например REP и LZMA
static int TEMPFILE_x = AddExternalCompressor ("[External compressor:tempfile]\n header=0");   // Зарегистрируем парсер метода TEMPFILE
