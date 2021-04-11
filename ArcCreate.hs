{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Создание и изменение архивов.                                                              ----
---- Здесь отрабатываются все команды создания и модификации архивов:                           ----
----   create/a/f/m/u/ch/c/d/k/s/rr/j                                                           ----
---- Процедура runArchiveCreate создаёт список файлов, которые должны попасть в выходной архив, ----
----   затем запускает процессы создания структуры выходного архива, чтения входных файлов,     ----
----   упаковки и записи данных в выходной архив.                                               ----
---- Эти процессы описаны в ArcvProcessRead.hs и ArcvProcessCompress.hs                         ----
----------------------------------------------------------------------------------------------------
module ArcCreate where

import Prelude hiding (catch)
import Control.Concurrent
import Control.OldException
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import System.Mem
import System.IO
#if defined(FREEARC_UNIX)
import System.Posix.Files hiding (fileExist)
#endif

import Utils
import Files
import Charsets            (i18n)
import Process
import Errors
import ByteStream
import FileInfo
import Options
import UI
import ArhiveStructure
import ArhiveFileList
import Arhive7zLib
import ArhiveDirectory
import ArcExtract
import ArcvProcessRead
import ArcvProcessExtract
import ArcvProcessCompress


-- |Обобщённая команда создания/изменения архива
runArchiveCreate pretestArchive
                 writeRecoveryBlocks
                 command @ Command {             -- данные о выполняемой команде:
      cmd_name            = cmd                  --   название команды
    , cmd_arcname         = arcname              --   основной архив, который подвергается обновлению
    , cmd_archive_filter  = archive_filter       --   предикат выбора обрабатываемых файлов из архивов
    , cmd_added_arcnames  = find_added_arcnames  --   дополнительные входные архивы
    , cmd_diskfiles       = find_diskfiles       --   файлы, которые нужно добавить с диска
    , opt_arccmt_str      = arccmt_str           --   новый комментарий к архиву, или
    , opt_arccmt_file     = arccmt_file          --   файл, из которого читается новый комментарий к архиву
    , opt_data_compressor = compressor           --   алгоритм сжатия
    } = do

  -- Напечатаем карту памяти, если включена отладка
  opt_testMalloc command &&& testMalloc

  -- Создаём sfx-архив сразу с расширением EXE, если только мы не должны обновить уже существующий архив
  arcname <- do archiveExists <- fileExist arcname
                if is_CMD_CREATE cmd || not archiveExists
                  then return$ cmdChangeSfxExt command arcname
                  else return arcname
  command <- return command {cmd_arcname = arcname}

  -- Команда "create" всегда создаёт архив с нуля
  when (is_CMD_CREATE cmd)$  do ignoreErrors$ fileRemove arcname
  -- Сообщить пользователю о начале обработки архива и запросить пароль архивации, если необходимо
  uiStartArchive command =<< limit_compression command compressor   -- ограничить компрессор объёмом доступной памяти и значением -lc
  command <- (command.$ opt_cook_passwords) command ask_passwords   -- подготовить пароли в команде к использованию
  debugLog "Started"

  -- Прочитать служебную информацию основного (обновляемого) архива, включая каталоги.
  -- Выйти, если архив залочен или содержит recovery info и повреждён.
  -- Если мы создаём новый архив, то подставить вместо старого "фантом".
  let abort_on_locked_archive archive footer = do
          -- to do: check that archive type is updatable (f.e. non RAR)
          when (ftLocked footer) $
              registerError$ GENERAL_ERROR ["0310 can't modify archive locked with -k"]
          pretestArchive command archive footer
  --
  uiStage "0249 Reading archive directory"
  updatingArchive <- fileExist arcname
  main_archive    <- if updatingArchive
                       then archiveReadInfo command "" "" archive_filter abort_on_locked_archive arcname
                       else return phantomArc
  debugLogList "There are %1 files in archive being updated" (arcDirectory main_archive)

  -- Установить тип создаваемого архива
  let arctype | isArcPhantom main_archive = opt_archive_type command
              | isArcArchive main_archive = aFreeArcInternalExt
              | otherwise                 = arcArchiveType main_archive
  command <- return command {opt_archive_type = arctype}

  -- Найти на диске добавляемые архивы (для команды "j") и прочитать их служебную информацию.
  -- Выйти, если любой из этих архивов содержит recovery info и повреждён.
  uiStartScanning
  added_arcnames <- find_added_arcnames
  debugLogList "Found %1 archives to add" added_arcnames
  added_archives  <- foreach added_arcnames (archiveReadInfo command "" "" archive_filter (pretestArchive command))
  debugLogList "There are %1 files in archives to add" (concatMap arcDirectory added_archives)
  let input_archives = main_archive:added_archives      -- список всех входных архивов
      closeInputArchives = for input_archives arcClose  -- операция закрытия всех входных архивов

  -- Получить комментарий к создаваемому архиву путём комбинации старых или вводом от пользователя
  arcComment <- getArcComment arccmt_str arccmt_file input_archives (opt_parseFile command)

  -- Найти добавляемые файлы на диске и отсортировать их список
  uiStartScanning
  diskfiles <- find_diskfiles
  debugLogList "Found %1 files" diskfiles
  uiStage "0250 Sorting filelist"
  sorted_diskfiles <- (opt_reorder command &&& reorder) (sort_files command diskfiles)
  debugLogList "Sorted %1 files" sorted_diskfiles
  uiStartScanning  -- очистим счётчик для стадии анализа содержимого файлов

  -- Получить список файлов, которые должны попасть в выходной архив, путём объединения.
  -- списка файлов из обновляемого архива, списка файлов из добавляемых (командой "j")
  -- к нему архивов, и файлов с диска. Предварительно эти списки зачищаются от дубликатов.
  files_to_archive <- join_lists main_archive added_archives sorted_diskfiles command
  debugLogList "Joined filelists, %1 files" files_to_archive

  if null files_to_archive &&  not (is_CMD_MODIFY cmd)    -- Если выходной архив не содержит ни одного файла
    then do registerWarning NOFILES                       -- то сообщить об этом пользователю
            closeInputArchives                            --    закрыть входные архивы
            ignoreErrors$ fileRemove arcname              --    удалить архив, если он существовал перед операцией (например, в случае команды "arc d archive *")
            return (1,0,0,0)
    else do

  -- Враппер, выполняющий постпроцессинг (-d[f], -ac) только если при тестировании созданного архива не было ни одного warning'а
  postProcess_wrapper command $ \postProcess_processDir deleteFiles -> do

  -- Ссылка для возврата результатов работы команды в вызывающую процедуру
  results <- ref (error "runArchiveCreate:results undefined")

  -- Сохранить mtime архива для опции -tk
  old_arc_exist <- fileExist arcname
  arc_time <- if old_arc_exist  then getFileDateTime arcname  else return (error "runArchiveCreate:arc_time undefined")

  -- Для реализации опции -tl мы должны получать списки всех записываемых в архив файлов и найти самый свежий из них.
  --   Для этого в create_archive_structure_PROCESS передаётся процедура `find_last_time`.
  --   Ей передают по частям список файлов, записываемых в архив, и она отслеживает самый свежий из них.
  --   Этой датой будет проштампован архив после окончания архивации.
  last_time <- ref aMINIMUM_POSSIBLE_FILETIME
  let find_last_time dir  =  last_time .= (\time -> maximum$ time : map (fiTime.fwFileInfo) dir)
  let processDir dir      =  do when (opt_time_to_last command) $ do
                                  find_last_time dir
                                postProcess_processDir dir  -- враппер постпроцессинга тоже должен получить список успешно сархивированных файлов

  -- Сообщить пользователю о начале упаковки данных
  unless (is_CMD_MODIFY$ cmd_name command)$ do
    uiStartProcessing (length files_to_archive) (sum$ map (fiSize.cfFileInfo) files_to_archive) 0 0
  performGC   -- Почистить мусор чтобы освободить как можно больше памяти для алгоритмов сжатия данных

  -- Сначала мы записываем содержимое создаваемого архива во временный файл и лишь затем, при успехе архивации - переименовываем его
  tempfile_wrapper arcname command deleteFiles pretestArchive $ \temp_arcname temp_arcnames' -> do
    ensureCtrlBreak "closeInputArchives" closeInputArchives $ do   -- Закроем входные архивы по завершении архивации
      if (arctype /= aFreeArcInternalExt)   then szCompress command main_archive arcname temp_arcname temp_arcnames' diskfiles results   else do   -- Обратимся к 7z.dll для архивации в недефолтный формат
      bracketCtrlBreak "archiveClose:ArcCreate" (archiveCreateRW temp_arcname) (archiveClose) $ \archive -> do
        writeSFX (opt_sfx command) archive main_archive    -- Начнём создание архива с записи SFX-модуля
        -- Создание архива - последовательность отдельных процессов, передающих данные друг другу:
        --   процесса разработки структуры архива и чтения упаковываемых данных
        --   процесса упаковки и записи сжатых данных в архивный файл
        -- Между ними создаётся очередь неограниченной длины (|>>>), что позволяет осуществлять read-ahead сжимаемых данных
        let read_files          =  create_archive_structure_AND_read_files_PROCESS command archive main_archive files_to_archive processDir arcComment writeRecoveryBlocks results
            compress_AND_write  =  compress_AND_write_to_archive_PROCESS archive command
        backdoor <- newChan   -- Этот канал используется для возвращения информации о созданных блоках архива
        runP (read_files backdoor |>>> compress_AND_write backdoor)
      --debugLog "Archive written"

  when (opt_keep_time command && old_arc_exist) $ do   -- Если использована опция -tk и это было обновление существующего архива
    setFileDateTime arcname arc_time                   --   то восстановить mtime архива
  when (opt_time_to_last command) $ do                 -- Если использована опция -tl
    setFileDateTime arcname =<< val last_time          --   то установить время&дату модификации архива на время&дату модификации самого свежего файла в нём
  renameArchiveAsSFX command arcname                   -- Переименуем архив, если в него был добавлен или из него убран SFX-модуль
  val results                                          -- Возвратим статистику выполнения команды


----------------------------------------------------------------------------------------------------
---- Использование временного файла при создании архива --------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Префикс и суффикс имён создаваемых временных файлов
temparc_prefix = "freearc"
temparc_suffix = ".tmp"

-- |Выполнить `action` с именем временного файла и затем переименовать его (если нет опций -v)
tempfile_wrapper filename command deleteFiles pretestArchive action   =   find 1 >>= doit
  where -- Найти свободное имя для временного файла
        find n = do tempdir <- if opt_create_in_workdir command  then getTempDir  else return (takeDirectory filename)
                    createDirectoryHierarchy tempdir
                    let tempname = tempdir </> (temparc_prefix++show n++temparc_suffix)
                    found <- liftM2 (||) (fileOrDirExist tempname) (fileOrDirExist$ tempname++".001")   -- check that both archive and multi-volume archive may be created
                    case found of
                        True  | n==999    -> registerError$ GENERAL_ERROR ["0311 can't create temporary file"]
                              | otherwise -> find (n+1)
                        False             -> return tempname

        -- Выполнить действие, используя временное имя файла, протестировать и затем переименовать окончательный архив
        doit tempname = do old_file <- fileExist filename      -- Мы выполняем обновление существующего архива?
                           tempnames' <- mvar [tempname]
                           handleCtrlBreak "fileRemove tempname" (val tempnames' >>= mapM_ (ignoreErrors.fileRemove)) $ do
                               -- Выполнить архивацию
                               action tempname tempnames';  tempnames <- val tempnames'
                               -- Если указана опция "-t", то протестируем только что созданный архив
                               when (opt_test command) $ do
                                   test_archive tempnames (opt_keep_broken command)
                           -- Получим имена временных архивов и вычислим сответствующие им имена окончательных файлов
                           tempnames <- val tempnames'
                           let filenames = tempnames.$map ((filename++) . drop (length$ takeFileName tempname) . takeFileName)
                           handleCtrlBreak "Keeping temporary archive" (condPrintLineLn "n"$ "Keeping temporary archive "++head tempnames) $ do
                               -- Удалить сархивированные файлы, если использована опция -d
                               deleteFiles
                               -- Заменить старый архив новым
                               if old_file && filename==head filenames
                                   then fileRemove filename   -- Хорошо бы проверять, что это всё ещё тот самый файл
                                   else whenM (fileExist filename) $ do  -- Если файл с именем выходного архива создали за время архивации, то сообщить об ошибке
                                            registerError$ GENERAL_ERROR ["0312 output archive already exists, keeping temporary file %1", head tempnames]
                               for (zip tempnames filenames) $ \(tempname,filename) -> do
                                   fileRename tempname filename
                                       `catch` (\_-> do condPrintLineLn "n"$ "Copying temporary archive "++tempname++" to "++filename
                                                        fileCopy tempname filename; fileRemove tempname)
                           -- Если указана опция "-t" и архивы были скопированы в другой каталог, то ещё раз протестируем окончательный архив
                           when (opt_test command && takeDirectory tempname/=takeDirectory filename) $ do
                               test_archive filenames (opt_keep_broken command || opt_delete_files command /= NO_DELETE)

        -- Протестировать архив и выйти, удалив его, если при этом возникли проблемы
        test_archive arcnames keep_broken_archive = do
            w <- count_warnings $ do
                     testArchive command (head arcnames) pretestArchive
            -- Продолжать работу только при отсутствии warning'ов
            when (w/=0) $ do
                unless keep_broken_archive $ do
                    for arcnames (ignoreErrors.fileRemove)
                registerError$ GENERAL_ERROR$ if keep_broken_archive
                                                 then ["0313 archive broken, keeping temporary file %1", head arcnames]
                                                 else ["0314 archive broken, deleting"]


----------------------------------------------------------------------------------------------------
---- Постпроцессинг, выполняемый только если архивация прошла успешно ------------------------------
----------------------------------------------------------------------------------------------------

-- |Постпроцессинг, выполняемый только если архивация прошла успешно:
--    удалить успешно сархивированные файлы, если задана опция -d[f]
--    сбросить у них атрибуты Archive, если задана опция -ac
postProcess_wrapper command archiving = do
  doFinally uiDoneArchive2 $ do
  case (opt_delete_files command/=NO_DELETE || opt_clear_archive_bit command) of
      False -> archiving (\dir->return()) (return())  -- Если файлы удалять не нужно, то просто выполним archiving

      _ -> do files2delete <- ref []   -- Список файлов, которые мы должны удалить
              dirs2delete  <- ref []   -- Список каталогов, которые мы должны удалить
              let -- Этой процедуре по частям передаётся список успешно сархивированных файлов и каталогов,
                  -- и она запоминает их все с тем, чтобы после успешного окончания архивации удалить их
                  processDir filelist0  =  do
                      let filelist = map fwFileInfo$ filter isFILE_ON_DISK filelist0
                          (dirs,files)  =  partition fiIsDir filelist
                      evalList files  `seq`  (files2delete ++= files)
                      evalList dirs   `seq`  (dirs2delete  ++= dirs )
                  -- Удалить сархивированные файлы и каталоги
                  deleteFiles = when (opt_delete_files command /= NO_DELETE) $ do
                                    condPrintLineLn "n"$ "Deleting successfully archived files"
                                    -- Удаление файлов
                                    files <- val files2delete
                                    for files $ \fi -> do
                                        whenM (check_that_file_was_not_changed fi) $ do
                                            forcedFileRemove (diskName fi)
                                    -- Удаление каталогов
                                    when (opt_delete_files command == DEL_FILES_AND_DIRS) $ do
                                        dirs <- val dirs2delete
                                        for (reverse dirs) (dirRemove.diskName)   -- Каталоги обычно сохраняются в порядке обхода, то есть родительский каталог в списке раньше дочерних. Так что reverse позволяет удалить сначала дочерние каталоги

              -- Выполнить архивацию, занося успешно сархивированные файлы в списки files2delete и dirs2delete.
              -- Удалить файлы после архивации, если задана опция -d[f]
              results <- archiving processDir deleteFiles
              -- Сбросить атрибут "архивировано" у успешно упакованных файлов, если задана опция -ac
              when (opt_clear_archive_bit command) $ do
                  condPrintLineLn "n"$ "Clearing Archive attribute of successfully archived files"
                  files <- val files2delete
                  for files $ \fi -> do
                      whenM (check_that_file_was_not_changed fi) $ do
                          clearArchiveBit.fpFullname.fiDiskName$ fi
              return results

-- |Проверить, что файл не изменился с момента архивации
check_that_file_was_not_changed fi = do
    fileWithStatus "check_that_file_was_not_changed" (fpFullname.fiDiskName$ fi) $ \p_stat -> do
        size <- stat_size  p_stat
        time <- stat_mtime p_stat
        return (size==fiSize fi  &&  time==fiTime fi)


----------------------------------------------------------------------------------------------------
---- Вспомогательные операции ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Получить комментарий выходного архива из файла, указанного опцией -z,
-- или конкатенацией комментариев входных архивов, и вывести его на экран
getArcComment arccmt_str arccmt_file input_archives parseFile = do
  -- Используем комментарий, заданный в командной строке, если есть
  if arccmt_str>""  then do uiPrintArcComment arccmt_str
                            return arccmt_str
    else do
  let old_comment = joinWith "\n\n" $ deleteIf null $ map arcComment input_archives
  -- В зависимости от значения опции "-z":
  case arccmt_file of
  -- Ввести комментарий с stdin
    ""   -> uiInputArcComment old_comment
  -- Удалить старый комментарий
    "-"  -> return ""
  -- Скопировать существующий комментарий (по умолчанию):
    "--" -> do uiPrintArcComment old_comment
               return old_comment
  -- Прочитать новый комментарий из указанного файла:
    _    -> do newcmt <- parseFile 'c' arccmt_file >>== joinWith "\n"
               uiPrintArcComment newcmt
               return newcmt

-- |Записать SFX-модуль в начало создаваемого архива
writeSFX sfxname archive old_archive = do
  let Just oldArchive = arcArchive old_archive
      oldSFXSize      = ftSFXSize (arcFooter old_archive)
  case sfxname of                                      -- В зависимости от значения опции "-sfx":
    "-"      -> return ()                              --   удалить старый sfx-модуль
    "--"     -> unless (isArcPhantom old_archive) $ do   --   скопировать sfx из исходного архива (по умолчанию)
                  archiveCopyData oldArchive 0 oldSFXSize archive
    filename -> bracket (archiveOpen sfxname              --   прочитать модуль sfx из указанного файла
                          `catch` (\e -> registerError$ GENERAL_ERROR ["0315 can't open SFX module %1", sfxname]))
                        (archiveClose)
                        (\sfxfile -> do size <- archiveGetSize sfxfile
                                        archiveCopyData sfxfile 0 size archive)

-- |Новое имя архива в соответствии с тем, что мы добавили или наоборот убрали из него SFX-модуль
cmdChangeSfxExt command  =  changeSfxExt (opt_noarcext command) (opt_sfx command) (opt_archive_type command)

changeSfxExt opt_noarcext opt_sfx arctype arcname =
  case (opt_noarcext, opt_sfx) of
--  Отключено, поскольку мешало конвертировать в SFX архивы изнутри GUI
--  (True, _)     -> arcname                -- Не менять расширение, если указана опция --noarcext
    (_   , "--")  -> arcname                --   или не указана опция "-sfx"
                                            -- При "-sfx-" расширение меняется на ".arc/.7z"
    (_   , "-")   -> if takeExtension arcname == aDEFAULT_SFX_EXTENSION
                       then replaceExtension arcname ("."++arctype)
                       else arcname
                                            -- При "-sfx..." расширение меняется на ".exe"
    _             -> if takeExtension arcname == "."++arctype
                       then replaceExtension arcname aDEFAULT_SFX_EXTENSION
                       else arcname

-- |Переименовать архив в соответствии с его SFX-именем
renameArchiveAsSFX command arcname = do
  let newname = cmdChangeSfxExt command arcname
  when (newname/=arcname) $ do
    condPrintLineLn "n"$ "Renaming "++arcname++" to "++newname
    fileRename arcname newname
#if defined(FREEARC_UNIX)
  -- Добавить или убрать "+x" из атрибутов файла, если его sfx-префикс изменился
  when (opt_sfx command /= "--") $ do
    let isSFX   = opt_sfx command /= "-"
    oldmode    <- fmap fileMode (fileGetStatus newname)
    let newmode = foldl (iif isSFX unionFileModes removeFileModes) oldmode executeModes
    fileSetMode newname newmode
#endif

-- |Протестировать только что созданный архив, находящийся в файле по имени `temp_arcname`
testArchive command temp_arcname pretestArchive = do
  let test_command = command{ cmd_name           = "t"           -- Тестируем
                            , cmd_arcname        = temp_arcname  -- в созданном архиве
                            , opt_arc_basedir    = ""            -- все файлы
                            , opt_disk_basedir   = ""            -- ...
                            , cmd_archive_filter = const True    -- ...
                            , cmd_subcommand     = True          -- Это подкоманда (тестирование внутри упаковки)
                            , opt_pretest        = 1             -- не стоит проводить тестирование перед тестированием, но recovery info проверить надо :)
                            }
  uiStartSubCommand command test_command
  results <- runArchiveExtract pretestArchive test_command
  uiDoneSubCommand command test_command [results]

