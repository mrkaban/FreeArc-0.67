/******************************************************************************
** Процесс выполнения команды *************************************************
******************************************************************************/
class PROCESS
{
public:
  COMMAND *cmd;            // Выполняемая команда
  BASEUI  *UI;
  ARCHIVE arcinfo;

  // Переменные, отражающие состояние процесса чтения входных данных
  MYFILE *infile;          // Файл архива, из которого идёт чтение
  FILESIZE bytes_left;     // Кол-во байт, которое осталось прочитать до исчерпания упакованных данных этого солид-блока

  // Переменные, отражающие состояние процесса записи распакованных данных
  DIRECTORY_BLOCK *dir;     // Каталог, которому принадлежат распаковываемые файлы
  int curfile;              //   Номер в каталоге текущего распаковываемого файла
  BOOL included;            //   Текущий файл включён в обработку или мы просто пропускаем его?
  int extractUntil;         //   Номер последнего файла, который нужно извлечь из этого солид-блока
  MYFILE outfile;           // Файл, извлекаемый из архива
  MYFILE auxfile;           // Вспомогательная переменная для хранения имени файла в том виде как оно выводится на экран
  char fullname[MY_FILENAME_MAX*4]; // Полное имя распаковываемого сейчас файла
  FILESIZE bytes_to_write;  // Сколько байт в текущем файле осталось записать
  FILESIZE writtenBytes;    // Сколько байт всего было распаковано в текущем архиве
  FILESIZE archive_pos;     // Текущая позиция в архиве
  CRC crc;                  // CRC данных, записанных в файл
  enum PASS {FIRST_PASS, SECOND_PASS};  // Первый/второй проход по солид-блоку (первый - распаковка каталогов и пустых файлов, второй - всех остальных)

  // Методы
  bool outfile_open (PASS pass);                         // Открыть очередной выходной файл и напечатать сообщение о его распаковке
  bool outfile_write (void *buf, int size);              // Записать данные в выходной файл
  bool outfile_close();                                  // Закрыть выходной файл
  int  DecompressCallback (const char *what, void *buf, int size);  // Callback-функция чтения/записи для распаковщика

  // Если алгоритм сжатия включает алгоритм(ы) шифрования, то добавляет к ним информацию о пароле
  char *GenerateDecryption (char *compressor, char *new_compressor);

  // Распаковать или протестировать файлы из солид-блока с номером block_num каталога dirblock
  void ExtractFiles (DIRECTORY_BLOCK *dirblock, int block_num);

  // Получить информацию об архиве
  PROCESS (COMMAND* _cmd, BASEUI* _UI, uint64 &total_files, uint64 &origsize, uint64 &compsize);

  // Читает структуру архива и вызывает в зависимости от выполняемой команды
  // ListFiles для каждого блока каталога или ExtractFiles для каждого солид-блока
  PROCESS(COMMAND* _cmd, BASEUI* _UI);
  void OpenArchive();

  // Процедура экстренного выхода
  bool quit (int errcode, char* errmsg);
} *CurrentProcess;


/*************************************************************************************************
** Нижеследующие процедуры предоставляют абстрактные средства работы с текущим выходным файлом, **
** скрывающие такие детали, как различия команд e/x/t, различие между каталогами и файлами,     **
** и то, что часть файлов может быть исключена из обработки                                     **
*************************************************************************************************/

// Открыть очередной выходной файл и напечатать сообщение о его распаковке
bool PROCESS::outfile_open (PASS pass)
{
  crc = INIT_CRC;
  bytes_to_write = dir->size[curfile];
  if (pass==SECOND_PASS && bytes_to_write==0  ||  !cmd->ok)
    return cmd->ok;  // Directories and empty files were extracted in first pass; don't create outfile if we are in the process of aborting extraction
  included = cmd->accept_file (dir, curfile);
  // Имя выходного файла (помимо каталога, указанного в -dp)
  char *xname = cmd->cmd=='e'? dir->name[curfile]
                             : dir->fullname (curfile, fullname)  +  (strequ(cmd->arc_base_dir,"")?  0  :  strlen (cmd->arc_base_dir)+1);
  outfile.setname (xname);

  if (included && cmd->cmd!='t')
    if (dir->isdir[curfile])
      {if (cmd->cmd!='e')  BuildPathTo (outfile.filename), create_dir (outfile.filename);}
    else
      {bool outfile_exists = outfile.exists();
       if (outfile_exists)
       {
         if (cmd->no)  included = FALSE;
         else if (!cmd->yes)
         {
           char answer = UI->AskOverwrite (outfile.displayname(), dir->size[curfile], dir->time[curfile]);
           switch (answer)
           {
             case 'y': break;
             case 'n': included = FALSE;  break;
             case 'a': cmd->yes = TRUE;   break;
             case 's': cmd->no  = TRUE;   included = FALSE;  break;
             case 'q': return quit(FREEARC_ERRCODE_OPERATION_TERMINATED, "");
           }
         }
       }
       if (included)  {if(outfile_exists)  outfile.remove_readonly_attrib();  outfile.open (WRITE_MODE);}
      }

  if (pass==FIRST_PASS || dir->size[curfile]>0)   // Не писать повторно о распаковке каталогов/пустых файлов
    if (!(dir->isdir[curfile] && cmd->cmd!='x'))  // Не сообщать о тестировании каталогов ;)
    {
      auxfile.setname (dir->fullname (curfile, fullname));
      if (!UI->ProgressFile (dir->isdir[curfile], included? (cmd->cmd=='t'? "Testing":"Extracting"):"Skipping", auxfile.displayname(), bytes_to_write))
        return quit(FREEARC_ERRCODE_OPERATION_TERMINATED, "");
    }

  return cmd->ok;
}

// Записать данные в выходной файл
bool PROCESS::outfile_write (void *buf, int size)
{
  if (!cmd->ok)
    return cmd->ok;  // quickly return if we are in the process of aborting extraction
  crc = UpdateCRC (buf, size, crc);
  if (included && cmd->cmd!='t' && size)
    outfile.write(buf,size);
  if (!UI->ProgressWrite (writtenBytes += size))
    return quit(FREEARC_ERRCODE_OPERATION_TERMINATED, "");
  return cmd->ok;
}

// Закрыть выходной файл
bool PROCESS::outfile_close()
{
  if (!cmd->ok)
    return cmd->ok;  // quickly return if we are in the process of aborting extraction
  if (included)
  {
    CHECK (FREEARC_ERRCODE_BAD_CRC,  (crc^INIT_CRC) == dir->crc[curfile],  (s,"ERROR: file %s failed CRC check", outfile.utf8name));
    if (cmd->cmd!='t' && !dir->isdir[curfile])
      outfile.close();
      outfile.SetFileDateTime (dir->time[curfile]);
  }
  included = FALSE;
  return cmd->ok;
}


/******************************************************************************
** Реализация команд распаковки и тестирования архивов ************************
******************************************************************************/

// Callback-функция чтения/записи для распаковщика
int PROCESS::DecompressCallback (const char *what, void *buf, int size)
{
  if (strequ (what, "read")) {
    int read_bytes = mymin (bytes_left, size);
    if (read_bytes==0)  return 0;
    if (!UI->ProgressRead (archive_pos))  {quit(FREEARC_ERRCODE_OPERATION_TERMINATED, ""); return FREEARC_ERRCODE_OPERATION_TERMINATED;}
    int len = infile->tryRead (buf, read_bytes);
    if (len>0)  bytes_left -= len,  archive_pos += len;
    return len;

  } else if (strequ (what, "write")) {
    int origsize = size;
    if (curfile > extractUntil)  return FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED;   // Нам попался тупой распаковщик, не способный завершить распаковку по требованию :(
    while (size>0) {
      int n = mymin (bytes_to_write, size);   // Записываем сколько осталось до конца файла или сколько осталось данных в буфере - смотря что меньше
      if (!outfile_write (buf,n))        return FREEARC_ERRCODE_OPERATION_TERMINATED;
      bytes_to_write -= n;
      if (bytes_to_write==0) {                // Если файл записан до конца - перейдём к следующему
        if (!outfile_close())            return FREEARC_ERRCODE_OPERATION_TERMINATED;
        if (++curfile > extractUntil)    return FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED;   // Если все файлы, которые мы должны распаковать из этого блока, уже извлечены, то попросить распаковщик завершить распаковку
        if (!outfile_open(SECOND_PASS))  return FREEARC_ERRCODE_OPERATION_TERMINATED;
      }
      buf=(uint8*)buf+n; size-=n;
    }
    return origsize;     // Сигнализировать успешную запись и попросить продолжить распаковку

  } else return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}



// Insert "tempfile" into compressors chain where required
// При этом производятся достаточно хитроумные манипуляции чтобы REP и другие алгоритмы с SparseDecompression
//   (способные разбивать память для распаковки на небольшие блоки) ограничивались лишь общим объёмом памяти, а остальные алгоритмы -
//   размером наибольшего НЕПРЕРЫВНОГО свободного блока. В частности, GetTotalMemoryToAlloc() игнорирует блоки размером меньше 10 мб.
//   Но гарантий надёжной работы этого алгоритма всё равно нет. В большинстве случаев он будет работать оптимально.
char *InsertTempfile (char *compressor, COMMAND* cmd)
{
  // noLimitMem отключает весь этот механизм, оставляя ответственность на пользователе
  if (cmd->noLimitMem)
    return NULL;

  char *c = (compressor = strdup_msg(compressor));
  int  compressor_len = strlen(compressor);
  char *BUFFERING = "tempfile";
  char PLUS[] = {COMPRESSION_METHODS_DELIMITER, '\0'};

  // Разобьём компрессор на отдельные алгоритмы и посчитаем расход памяти
  CMETHOD        cm[MAX_METHODS_IN_COMPRESSOR];
  LongMemSize  memi[MAX_METHODS_IN_COMPRESSOR];
  bool        solid[MAX_METHODS_IN_COMPRESSOR];
  bool      barrier[MAX_METHODS_IN_COMPRESSOR];
  int N = split (compressor, COMPRESSION_METHODS_DELIMITER, cm, MAX_METHODS_IN_COMPRESSOR);
  LongMemSize mem = 0, block = 0;
  for (int i=0; i<N; i++)
  {
    COMPRESSION_METHOD *method = ParseCompressionMethod (cm[i]);
    if (!method)  goto abort;
    mem += memi[i] = method->GetDecompressionMem();
    solid[i] = !method->is("SparseDecompression?");
    if (solid[i])  block += memi[i];
    barrier[i] = method->is("MemoryBarrierDecompression?");
    if (barrier[i])  mem = block = memi[i] = 0;
    free(method);
  }

  {
    // Maximum memory allowed to use and largest contiguous memory block
    LongMemSize maxmem    =  cmd->limitMem?  cmd->limitMem  :  mymin (GetPhysicalMemory()/4*3, GetTotalMemoryToAlloc()-30*mb);
    LongMemSize maxblock  =  cmd->limitMem?  cmd->limitMem  :  GetMaxBlockToAlloc();

    // If memreqs are too large - add "tempfile" between methods
    if (mem > maxmem  ||  block > maxblock)
    {
      compressor = (char*) malloc_msg (compressor_len + (strlen(BUFFERING)+strlen(PLUS))*(N-1) + 1);
      strcpy(compressor, "");
      mem = block = 0;

      for (int i=0; i<N; i++)
      {
        // Limit i'th compression method if it needs too much memory (by reducing number of threads so we don't lose compatibility)
        char new_method[MAX_METHOD_STRLEN];
        LongMemSize maxi  =  solid[i]? maxblock:maxmem;

        if (!barrier[i]  &&  memi[i] > maxi)
          LimitDecompressionMem (cm[i], maxi, new_method),
          memi[i] = GetDecompressionMem(new_method);
        else
          strcpy (new_method, cm[i]);

        // If total memreqs of methods after last barrier >maxmem (or total reqs of methods requiring solid memory blocks >maxblock),
        //   then add one more tempfile occurence
        if (mem>0 && memi[i]>0 && (mem+memi[i]>maxmem || (solid[i] && block+memi[i]>maxblock)))
        {
          strcat (compressor, BUFFERING);
          strcat (compressor, PLUS);
          mem = block = 0;
        }
        strcat (compressor, new_method);
        strcat (compressor, PLUS);

        mem += memi[i];
        if (solid[i])  block += memi[i];
        if (barrier[i])  mem = block = 0;
      }
      free(c);  // we can't free c earlier since its space used by cm[i]
      last_char(compressor) = '\0';   // remove superfluous '+'
      return compressor;
    }
  }

abort:
  free(c);
  return NULL;
}



// Если алгоритм сжатия включает алгоритм(ы) шифрования, то добавляет к ним информацию о пароле
char *PROCESS::GenerateDecryption (char *compressor, char *new_compressor)
{
#ifndef UNARC_DECRYPTION
  return compressor;
#else
  if (!compressorIsEncrypted(compressor))                                     // Быстро выйдем если в алгоритм не входят методы шифрования
    return compressor;

  char new_method_buf[MAX_COMPRESSOR_STRLEN], *new_method = new_method_buf;   // Буфер для записи вновь сконструированных методов шифрования (с ключом)

  // Разобьём компрессор на отдельные алгоритмы
  CMETHOD cm[MAX_METHODS_IN_COMPRESSOR];
  int N = split (compressor, COMPRESSION_METHODS_DELIMITER, cm, MAX_METHODS_IN_COMPRESSOR);
  for (int i=0; i<N; i++)
  {
    CMETHOD method = cm[i];
    // Eсли ответ положительный - это метод шифрования
    if (CompressionService(method, "encryption?", 0, NULL, NULL) > 0)
    {
      // Вычислим ключ шифрования согласно PKCS5#2, используя пароль+соль
      int keySize = CompressionService(method, "keySize", 0, NULL, NULL),     // Длина ключа (определяется методом сжатия)
          error = 0;

      CPARAM param[MAX_PARAMETERS];
      int paramcnt = split (method, COMPRESSION_METHOD_PARAMETERS_DELIMITER, param, MAX_PARAMETERS-1);

      char *saltStr  = search_param(param+1, "s");                            // Соль
      int   saltSize = strlen(saltStr) / 2;
      BYTE  salt[MAXKEYSIZE];  decode16 (saltStr, salt);

      char *checkCodeStr  = search_param(param+1, "c");                       // Код, используемый доля проверки правильности пароля
      int   checkCodeSize = strlen(checkCodeStr) / 2;
      BYTE  checkCode[MAXKEYSIZE];  decode16 (checkCodeStr, checkCode);

      char *numIterationsStr = search_param(param+1, "n");                    // Количество итераций PKCS5#2
      int   numIterations    = parseInt(numIterationsStr, &error);

      BYTE  key_and_checkCode[MAXKEYSIZE*2];

retry:Pbkdf2Hmac ((BYTE*) cmd->pwd, strlen(cmd->pwd), salt, saltSize, numIterations, key_and_checkCode, keySize+checkCodeSize);

      if (memcmp (key_and_checkCode+keySize, checkCode, checkCodeSize))       // Проверочный код не совпал - ошибка в пароле!
      {
        char answer = UI->AskPassword (cmd->pwd, PASSWORDBUF_SIZE);
        switch (answer)
        {
          case 'y': goto retry;
          case 'n': break;
          case 'q': quit(FREEARC_ERRCODE_OPERATION_TERMINATED, "");
        }
        CHECK (FREEARC_ERRCODE_BAD_PASSWORD,  FALSE,  (s,"ERROR: wrong password"));
      }

      // Добавим ключ в запись алгоритма шифрования
      char keyParam[MAXKEYSIZE*2+2];        // Строка для шестнадцатеричной записи ключа плюс "k" в начале и '\0' в конце
      keyParam[0] = 'k';
      encode16(key_and_checkCode, keySize, keyParam+1);
      param[paramcnt++] = keyParam;
      param[paramcnt++] = NULL;

      // Пересоберём метод шифрования с добавлением ключа
      join (param, COMPRESSION_METHOD_PARAMETERS_DELIMITER, new_method, endof(new_method_buf)-new_method);
      cm[i] = new_method;
      new_method += strlen(new_method)+1;   // Переместим указатель на первый свободный байт в буфере new_method_buf
    }
  }

  // Пересоберём алгоритм с обновлёнными описаниями методов шифрования
  join (cm, COMPRESSION_METHODS_DELIMITER, new_compressor, MAX_COMPRESSOR_STRLEN);
  return new_compressor;
#endif //UNARC_DECRYPTION
}

char* global_GenerateDecryption (char* compressor, char* new_compressor, void* auxdata)
{
  return ((PROCESS*)auxdata) -> GenerateDecryption (compressor, new_compressor);
}

int global_callback (const char *what, void *buf, int size, void *auxdata)
{
  return ((PROCESS*)auxdata) -> DecompressCallback (what, buf, size);
}

// Распаковать или протестировать файлы из солид-блока с номером block_num каталога dirblock
void PROCESS::ExtractFiles (DIRECTORY_BLOCK *dirblock, int block_num)
{
  dir = dirblock;
  BLOCK& data_block (dirblock->data_block [block_num]);
  extractUntil = -1;                     // В эту переменную будет записан номер последнего файла в солид-блоке, который нужно обработать
  // Переберём все файлы в этом блоке
  for (curfile = dirblock->block_start(block_num); curfile < dirblock->block_end(block_num); curfile++) {
    if (cmd->accept_file (dirblock, curfile))    // Если этот файл требуется обработать
    {
      if (dir->size[curfile]==0) {               //   то если это каталог или пустой файл - сделаем это сразу
        if (!outfile_open (FIRST_PASS))  return;
        if (!outfile_close())            return;}
      else
        extractUntil = curfile;                  //   а иначе - запомним, что нужно распаковать блок как минимум до этого файла
    }
  }
  if (extractUntil >= 0) {                       // Если в этом блоке нашлось что распаковывать - значит, распакуем! :)
    infile = &dirblock->arcfile;                 //   Архивный файл
    infile->seek (archive_pos = data_block.pos); //   Начало данных солид-блока в архиве
    bytes_left = data_block.compsize;            //   Размер упакованных данных в солид-блоке
    curfile = dirblock->block_start (block_num); // Номер первого файла в этом солид-блоке
    if (!outfile_open (SECOND_PASS))  return;    // Откроем первый выходной файл
    char *compressor1 = InsertTempfile (data_block.compressor, cmd);  // Добавим "tempfile" между компрессорами если не хватает памяти для распаковки
    char compressor2_buf[MAX_COMPRESSOR_STRLEN];
    char *compressor2 = GenerateDecryption(compressor1? compressor1 : data_block.compressor, compressor2_buf);
    int result = Decompress (compressor2, global_callback, this);
    CHECK (result,  result!=FREEARC_ERRCODE_INVALID_COMPRESSOR,  (s,"ERROR: unsupported compression method %s", data_block.compressor));
    CHECK (result,  result>=0 || result==FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED,  (s,"ERROR: archive data corrupted (decompression fails)"));
    if (compressor1!=data_block.compressor)  free (compressor1);
    if (!outfile_close())  return;               // Закроем последний выходной файл
  }
}


/******************************************************************************
** Получить информацию об архиве **********************************************
******************************************************************************/

PROCESS::PROCESS (COMMAND* _cmd, BASEUI* _UI, uint64 &total_files, uint64 &origsize, uint64 &compsize) : cmd(_cmd), UI(_UI)
{
  CurrentProcess = this;
  SetCompressionThreads (GetProcessorsCount());
  OpenArchive();                                                      // Откроем файл архива и прочитаем структуру архива
  total_files = origsize = compsize = 0;

  iterate_array (i, arcinfo.control_blocks_descriptors) {             // Переберём все служебные блоки в архиве...
    BLOCK& block_descriptor = arcinfo.control_blocks_descriptors[i];
    if (block_descriptor.type == DIR_BLOCK) {                         // ... и отберём из них блоки каталога
      DIRECTORY_BLOCK dirblock (arcinfo, block_descriptor,           // Прочитаем блок каталога
                                global_GenerateDecryption, this);
      iterate_var (i, dirblock.total_files)                           // Переберём все файлы в каталоге
      	origsize += dirblock.size[i];
      iterate_array (i, dirblock.data_block)                          // Переберём все солид-блоки в каталоге
      	compsize += dirblock.data_block[i].compsize;
      total_files += dirblock.total_files;
    }
  }
  arcinfo.arcfile.tryClose();
}


/******************************************************************************
** Головная процедура выполнения команды над архивом **************************
******************************************************************************/

// Откроем файл архива и прочитаем структуру архива
// Для SFX пробует соответствующий .arc файл если SFX-модуль не содержит архива
void PROCESS::OpenArchive()
{
#ifdef FREEARC_SFX
  SET_JMP_POINT_GOTO(try_arc);
try_exe:
#endif
  arcinfo.arcfile.open (cmd->arcname, READ_MODE);            // Откроем файл архива
  arcinfo.read_structure (global_GenerateDecryption, this);  // Прочитаем структуру архива
#ifdef FREEARC_SFX
  RESET_JMP_POINT();
  return;

try_arc:                                                     // Откроем файл с тем же именем что SFX, но расширением .arc
  SET_JMP_POINT_GOTO(try_exe);                               // При ошибке с .arc снова попытаемся открыть .exe чтобы вывести сообщение об ошибке с .exe
  arcinfo.arcfile.setname (cmd->arcname);
  arcinfo.arcfile.change_executable_ext (FREEARC_FILE_EXTENSION);
  arcinfo.arcfile.open (READ_MODE);
  arcinfo.read_structure (global_GenerateDecryption, this);
  RESET_JMP_POINT();
#endif
}


// Читает структуру архива и вызывает в зависимости от выполняемой команды
// ListFiles для каждого блока каталога или ExtractFiles для каждого солид-блока
PROCESS::PROCESS (COMMAND* _cmd, BASEUI* _UI) : cmd(_cmd), UI(_UI)
{
  CurrentProcess = this;
  cmd->Prepare();
  OpenArchive();                                                      // Откроем файл архива и прочитаем структуру архива

  // Выведем заголовок операции на экран и запросим у пользователя разрешение на распаковку SFX
  if (!UI->AllowProcessing (cmd->cmd, cmd->silent, MYFILE(cmd->arcname).displayname(), &arcinfo.arcComment[0], arcinfo.arcComment.size, cmd->outpath.utf8name)) {
    cmd->ok = FALSE;  return;
  }
  if (cmd->cmd!='t')       outfile.SetBaseDir (UI->GetOutDir());

  writtenBytes = 0;
  if (cmd->list_cmd())     UI->ListHeader (*cmd);
  else                     {if (!UI->BeginProgress (arcinfo.arcfile.size()))   return;}
  iterate_array (i, arcinfo.control_blocks_descriptors) {             // Переберём все служебные блоки в архиве...
    BLOCK& block_descriptor = arcinfo.control_blocks_descriptors[i];
    if (block_descriptor.type == DIR_BLOCK) {                         // ... и отберём из них блоки каталога
      DIRECTORY_BLOCK dirblock (arcinfo, block_descriptor,            // Прочитаем блок каталога
                                global_GenerateDecryption, this);
      if (cmd->list_cmd())                                            // Если это команда получения листинга
        UI->ListFiles (&dirblock, *cmd);                              //   то выполним её
      else
        iterate_array (i, dirblock.data_block)                        //   иначе - переберём все солид-блоки в каталоге
          ExtractFiles (&dirblock, i);                                //     и для каждого из них выполним процедуру тестирования/распаковки
    }
  }
  if (cmd->list_cmd())  UI->ListFooter (*cmd);
  else                  UI->EndProgress (cmd);
  arcinfo.arcfile.tryClose();
}


// Процедура экстренного прекращения работы (для exe) или экстренного выхода (для dll)
bool PROCESS::quit (int errcode, char* errmsg)
{
  cmd->ok = FALSE;
  if (outfile.isopen())  outfile.close(), outfile.remove(), included = FALSE;
#ifndef FREEARC_LIBRARY
  arcinfo.arcfile.tryClose();
  compressionLib_cleanup();
#endif
  UI->Abort (cmd, errcode, errmsg);
  return cmd->ok;
}


