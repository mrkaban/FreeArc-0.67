{-# OPTIONS_GHC -cpp #-}
---------------------------------------------------------------------------------------------------
---- Описание команд и опций, поддерживаемых FreeArc.                                          ----
---- Универсальный парсер командной строки.                                                    ----
---- Интерпретация Lua-скриптов.                                                               ----
---------------------------------------------------------------------------------------------------
module Options where

import Prelude hiding (catch)
import Control.OldException
import Control.Monad
import Control.Concurrent
import Data.Array
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.C.Types
import System.Environment
import System.IO.Unsafe
import System.Time
#if !defined(FREEARC_NO_LUA)
import qualified Scripting.Lua as Lua
#endif

import qualified CompressionLib
import Utils
import Files
import Charsets
import Errors
import FileInfo
import Compression


-- |Описание выполняемой команды
data Command = Command {
    cmd_args                 :: ![String]           -- Полный текст команды, разбитый на слова
  , cmd_additional_args      :: ![String]           -- Дополнительные опции, прочитанные из переменной среды и конфиг-файла
  , cmd_name                 :: !String             -- Название команды
  , cmd_arcspec              ::  String             -- Маска архивов
  , cmd_arclist              ::  [FilePath]         --   Имена всех найденных по этой маске (и возможно, рекурсивно) архивов
  , cmd_arcname              ::  FilePath           --   Имя обрабатываемого архива (из одной команды с wildcards в cmd_arcspec формируется несколько команд с конкретным именем обрабатываемого архива)
  , cmd_archive_filter       :: (FileInfo -> Bool)  -- Предикат отбора файлов из существующих архивов
  , cmd_filespecs            :: ![String]           -- Спецификации добавляемых архивов или файлов
  , cmd_added_arcnames       :: !(IO [FilePath])    --   Вычисление, возвращающее имена добавляемых архивов (команда "j")
  , cmd_diskfiles            :: !(IO [FileInfo])    --   Вычисление, возвращающее имена добавляемых файлов  (остальные команды создания/обновления архивов)
  , cmd_subcommand           :: !Bool               -- Подкоманда? (например, тестирование после архивации)
  , cmd_setup_command        :: !(IO ())            -- Действия, которые надо выполнить непосредственно перед началом отработки этой команды (один раз на все архивы)
                                                    -- Опции:
  , opt_scan_subdirs         :: !Bool               --   рекурсивный поиск файлов?
  , opt_add_dir              :: !Bool               --   добавить имя архива к имени каталога, куда происходит распаковка?
  , opt_add_exclude_path     :: !Int                --   исключить имя базового каталога / сохранить абсолютный путь (при ПОИСКЕ архивируемых файлов на диске)
  , opt_dir_exclude_path     :: !Int                --   исключить имя базового каталога / сохранить абсолютный путь (при чтении КАТАЛОГА АРХИВА)
  , opt_arc_basedir          :: !String             --   базовый каталог внутри архива
  , opt_disk_basedir         :: !String             --   базовый каталог на диске
  , opt_group_dir            :: ![Grouping]         --   группировка файлов для каталога архива
  , opt_group_data           :: ![Grouping]         --   группировка файлов для солид-блока
  , opt_data_compressor      :: !UserCompressor     --   методы сжатия для данных
  , opt_dir_compressor       :: !Compressor         --   метод сжатия для блоков каталога
  , opt_autodetect           :: !Int                --   уровень автодетекта типов файлов (0..9)
  , opt_arccmt_file          :: !String             --   файл, из которого читается (в который пишется) комментарий к архиву
  , opt_arccmt_str           :: !String             --   .. или сам комментарий в чистом виде
  , opt_include_dirs         :: !(Maybe Bool)       --   включить каталоги в обработку? (Да/Нет/По обстоятельствам)
  , opt_indicator            :: !String             --   тип индикатора прогресса ("0" - отсутствует, "1" - индикатор по умолчанию, "2" - вывод в отдельной строке имени каждого обрабатываемого файла)
  , opt_display              :: !String             --   внутренняя опция, описывающая какие строки выводить на экран
  , opt_overwrite            :: !(IORef String)     --   состояние запроса к пользователю о перезаписи файлов ("a" - перезаписывать все, "s" - пропускать все, любые другие - задавать вопросы)
  , opt_sfx                  :: !String             --   имя SFX-модуля, который надо присоединить к архиву ("-" - отсоединить, если уже есть, "--" - скопировать существующий)
  , opt_keep_time            :: !Bool               --   сохранить mtime архива после обновления содержимого?
  , opt_time_to_last         :: !Bool               --   установить mtime архива на mtime самого свежего файла в нём?
  , opt_keep_broken          :: !Bool               --   не удалять файлы, распакованные с ошибками?
  , opt_test                 :: !Bool               --   протестировать архив после упаковки?
  , opt_pretest              :: !Int                --   режим тестирования архивов _перед_ выполнением операции (0 - нет, 1 - только recovery info, 2 - recovery или full, 3 - full testing)
  , opt_lock_archive         :: !Bool               --   закрыть создаваемый архив от дальнейших изменений?
  , opt_match_with           :: !(PackedFilePath -> FilePath)  -- сопоставлять при фильтрации маски с fpBasename или fpFullname
  , opt_append               :: !Bool               --   добавлять новые файлы только в конец архива?
  , opt_recompress           :: !Bool               --   принудительно перепаковать все файлы?
  , opt_keep_original        :: !Bool               --   не перепаковывать ни одного файла?
  , opt_noarcext             :: !Bool               --   не добавлять стандартное расширение к имени архива?
  , opt_nodir                :: !Bool               --   не записывать в архив его оглавление (для бенчмарков)?
  , opt_nodates              :: !Bool               --   не записывать в архив даты модификации файлов?
  , opt_update_type          :: !Char               --   алгоритм обновления файлов (a/f/u/s)
  , opt_x_include_dirs       :: !Bool               --   включить каталоги в обработку (для команд листинга/распаковки)?
  , opt_no_nst_filters       :: !Bool               --   TRUE, если в команде отсутствуют опции отбора файлов по имени/размеру/времени (-n/-s../-t..)
  , opt_file_filter          :: !(FileInfo -> Bool) --   сформированный опциями предикат отбора файлов по атрибутам/размеру/времени/имени (всё, кроме filespecs)
  , opt_sort_order           :: !String             --   порядок сортировки файлов в архиве
  , opt_reorder              :: !Bool               --   переупорядочить файлы после сортировки (поместив рядом одинаковые/близкие файлы)?
  , opt_find_group           :: !(FileInfo -> Int)  --   функция, определяющая по FileInfo к какой группе (из arc.groups) относится данный файл
  , opt_groups_count         :: !Int                --   количество групп (`opt_find_group` возвращает результаты в диапазоне 0..opt_groups_count-1)
  , opt_find_type            :: !(FileInfo -> Int)  --   функция, определяющая по FileInfo к какому типу данных (из перечисленных в `opt_data_compressor`) относится данный файл
  , opt_types_count          :: !Int                --   количество типов файлов (`opt_find_type` возвращает результаты в диапазоне 0..opt_types_count-1)
  , opt_group2type           :: !(Int -> Int)       --   преобразует номер группы из arc.groups а номер типа файла из opt_data_compressor
  , opt_logfile              :: !String             --   имя лог-файла или ""
  , opt_delete_files         :: !DelOptions         --   удалить файлы/каталоги после успешной архивации?
  , opt_create_in_workdir    :: !Bool               --   создать архив сначала во временном каталоге?
  , opt_clear_archive_bit    :: !Bool               --   сбросить атрибут Archive у успешно упакованных файлов (и файлов, которые уже есть в архиве)
  , opt_language             :: !String             --   язык/файл локализации
  , opt_recovery             :: !String             --   величина Recovery блока (в процентах, байтах или секторах)
  , opt_broken_archive       :: !String             --   обрабатывать неисправный архив, полностью сканируя его в поисках оставшихся исправными блоков
  , opt_original             :: !String             --   перезагружать с указанного URL сбойные части архива
  , opt_save_bad_ranges      :: !String             --   записать в заданный файл список неисправных частей архива для их перевыкачки
  , opt_pause_before_exit    :: !String             --   сделать паузу перед выходом из программы
  , opt_global_queueing      :: !Bool               --   ждать в глобальной очереди операций?
  , opt_shutdown             :: !Bool               --   выключить компьютер по окончанию работы?
  , opt_compression_cache    :: !Int                --   размер буфера упреждающего чтения при сжатии
  , opt_decompression_cache  :: !Int                --   размер буфера упреждающего чтения при распаковке
  , opt_limit_compression_memory   :: !MemSize      --   ограничение памяти при упаковке, байт
  , opt_limit_decompression_memory :: !MemSize      --   ограничение памяти при распаковке, байт
  , opt_volumes              :: ![FileSize]         --   размеры томов создаваемого архива
  , opt_archive_type         :: !String             --   тип обрабатываемого архива (arc/zip/rar/...)
  , opt_archive_extension    :: !String             --   расширение архивов по умолчанию (.arc/.zip/.rar/...)
  , opt_7z_compression       :: ![String]           --   настройки сжатия для 7z.dll

                                                    -- Настройки шифрования:
  , opt_encryption_algorithm :: !String             --   алгоритм шифрования.
  , opt_cook_passwords                              --   подготавливает команду к использованию шифрования, заправшивая у пользователя пароль и считывая keyfile (не должно выполняться прежде, чем начнётся выполнение самой команды, поэтому не может быть выполнено в parseCmdline)
                             :: !(Command -> (ParseDataFunc -> IO String, ParseDataFunc -> IO String, IO ()) -> IO Command)
  , opt_data_password        :: String              --   пароль, используемый для шифрования данных (включает в себя ввод с клавиатуры и содержимое keyfiles). "" - паролирование не нужно
  , opt_headers_password     :: String              --   пароль, используемый для шифрования заголовков (ditto)
  , opt_decryption_info                             --   информация, используемая процедурой подбора ключа дешифрации:
                             :: ( Bool              --     не запрашивать у пользователя новый пароль, даже если все известные для распаковки данных не подходят?
                                , MVar [String]     --     список "старых паролей", которыми мы пытаемся расшифровать распаковываемые данные
                                , [String]          --     содержимое keyfiles, добавляемых к паролям
                                , IO String         --     ask_decryption_password
                                , IO ()             --     bad_decryption_password
                                )
  -- Операции чтения/записи файлов в кодировке, настраиваемый опцией -sc
  , opt_parseFile   :: !(Domain -> FilePath -> IO [String])      -- процедура парсинга файла с настраиваемой в -sc кодировкой и ОС-независимым разбиением на строки
  , opt_unParseFile :: !(Domain -> FilePath -> String -> IO ())  -- процедура записи файла с настраиваемой в -sc кодировкой
  , opt_parseData   :: !(Domain -> String -> String)             -- процедура парсинга введённых данных с настраиваемой в -sc кодировкой
  , opt_unParseData :: !(Domain -> String -> String)             -- процедура депарсинга данных для вывода с настраиваемой в -sc кодировкой
  }


-- |Прочитать список паролей, введённых пользователем для расшифровки архива
get_opt_decryption_passwords command  =  let (_, mvar_passwords, _, _, _) = opt_decryption_info command
                                         in val mvar_passwords

-- |Виртуальная опция --debug
opt_debug cmd = cmd.$opt_display.$(`contains_one_of` "$#")

-- |Включить тестирование памяти?
opt_testMalloc cmd = cmd.$opt_display.$(`contains_one_of` "%")


-- |Ограничивает метод сжатия до реально доступного объёма памяти (вызывается непосредственно перед стартом алгоритма)
-- и добавляет вызовы "tempfile" между слишком прожорливыми алгоритмами
-- (память ограничивается до значения -lc и размера наибольшего блока свободной памяти, если не задано -lc-)
limit_compression   = limit_de_compression opt_limit_compression_memory   limitCompressionMem

-- |Добавляет вызовы "tempfile" между слишком прожорливыми алгоритмами при распаковке
limit_decompression = limit_de_compression opt_limit_decompression_memory limitDecompressionMem

-- |Ограничивает размер добавляемой к архиву Recovery Record до реально доступного объёма памяти (поскольку RR при создании хранится целиком в ОЗУ)
limit_protection    = limit_de_compression opt_limit_compression_memory   minI

-- Generic algorithm
limit_de_compression option limit_f command method = do
  let memory_limit = command.$option
  if memory_limit==CompressionLib.aUNLIMITED_MEMORY
    then return method
    else do maxMem <- getMaxBlockToAlloc
            return$ limit_f (memory_limit `min` maxMem) method



-- |Список опций, поддерживаемых программой
optionsList = sortOn (\(OPTION a b _) -> (a|||"zzz",b))
   [OPTION "--"    ""                   "stop processing options"
   ,OPTION "cfg"   "config"            ("use configuration FILES (default: " ++ aCONFIG_FILES ++ ")")
   ,OPTION "env"   ""                  ("read default options from environment VAR (default: " ++ aCONFIG_ENV_VAR ++ ")")
   ,OPTION "r"     "recursive"          "recursively collect files"
   ,OPTION "f"     "freshen"            "freshen files"
   ,OPTION "u"     "update"             "update files"
   ,OPTION ""      "sync"               "synchronize archive and disk contents"
   ,OPTION "o"     "overwrite"          "existing files overwrite MODE (+/-/p)"
   ,OPTION "y"     "yes"                "answer Yes to all queries"
   ,OPTION "x"     "exclude"            "exclude FILESPECS from operation"
   ,OPTION "n"     "include"            "include only files matching FILESPECS"
   ,OPTION "ep"    "ExcludePath"        "Exclude/expand path MODE"
   ,OPTION "ap"    "arcpath"            "base DIR in archive"
   ,OPTION "dp"    "diskpath"           "base DIR on disk"
   ,OPTION "m"     "method"             "compression METHOD (-m0..-m9, -m1x..-m9x)"
   ,OPTION "dm"    "dirmethod"          "compression METHOD for archive directory"
   ,OPTION "ma"    ""                   "set filetype detection LEVEL (+/-/1..9)"
   ,OPTION "md"    "dictionary"         "set compression dictionary to N mbytes"
   ,OPTION "mm"    "multimedia"         "set multimedia compression to MODE"
   ,OPTION "ms"    "StoreCompressed"    "store already compressed files"
   ,OPTION "mt"    "MultiThreaded"      "number of compression THREADS"
   ,OPTION "mc"    ""                   "disable compression algorithms (-mcd-, -mc-rep...)"
   ,OPTION "mx"    ""                   "maximum internal compression mode"
   ,OPTION "max"   ""                   "maximum compression using external precomp, ecm, ppmonstr"
   ,OPTION "ds"    "sort"               "sort files in ORDER"                      -- to do: сделать эту опцию OptArg
   ,OPTION ""      "groups"             "name of groups FILE"                      -- to do: сделать эту опцию OptArg
   ,OPTION "s"     "solid"              "GROUPING for solid compression"           -- to do: сделать эту опцию OptArg
   ,OPTION "p"     "password"           "encrypt/decrypt compressed data using PASSWORD"
   ,OPTION "hp"    "HeadersPassword"    "encrypt/decrypt archive headers and data using PASSWORD"
   ,OPTION "ae"    "encryption"         "encryption ALGORITHM (aes, blowfish, serpent, twofish)"
   ,OPTION "kf"    "keyfile"            "encrypt/decrypt using KEYFILE"
   ,OPTION "op"    "OldPassword"        "old PASSWORD used only for decryption"
   ,OPTION "okf"   "OldKeyfile"         "old KEYFILE used only for decryption"
   ,OPTION "w"     "workdir"            "DIRECTORY for temporary files"
   ,OPTION ""      "create-in-workdir"  "create archive in workdir and then move to final location"
   ,OPTION "sc"    "charset"            "CHARSETS used for listfiles and comment files"
   ,OPTION ""      "language"           "load localisation from FILE"
   ,OPTION "tp"    "pretest"            "test archive before operation using MODE"
   ,OPTION "t"     "test"               "test archive after operation"
   ,OPTION "t"     "type"               "archive TYPE (arc/zip/rar/...)"
   ,OPTION "d"     "delete"             "delete files & dirs after successful archiving"
   ,OPTION "df"    "delfiles"           "delete only files after successful archiving"
   ,OPTION "kb"    "keepbroken"         "keep broken extracted files"
   ,OPTION "ba"    "BrokenArchive"      "deal with badly broken archive using MODE"
#if defined(FREEARC_WIN)
   ,OPTION "ac"    "ClearArchiveBit"    "clear Archive bit on files succesfully (de)archived"
   ,OPTION "ao"    "SelectArchiveBit"   "select only files with Archive bit set"
#endif
   ,OPTION "sm"    "SizeMore"           "select files larger than SIZE"
   ,OPTION "sl"    "SizeLess"           "select files smaller than SIZE"
   ,OPTION "tb"    "TimeBefore"         "select files modified before specified TIME"
   ,OPTION "ta"    "TimeAfter"          "select files modified after specified TIME"
   ,OPTION "tn"    "TimeNewer"          "select files newer than specified time PERIOD"
   ,OPTION "to"    "TimeOlder"          "select files older than specified time PERIOD"
   ,OPTION "k"     "lock"               "lock archive"
   ,OPTION "rr"    "recovery"           "add recovery information of specified SIZE to archive"
   ,OPTION "sfx"   ""                  ("add sfx MODULE (\""++default_sfx aFreeArcExt++"\" by default)")  -- to do: сделать эту опцию OptArg
   ,OPTION "z"     "arccmt"             "read archive comment from FILE or stdin"  -- to do: сделать эту опцию OptArg
   ,OPTION ""      "archive-comment"    "input archive COMMENT in cmdline"
   ,OPTION "i"     "indicator"          "select progress indicator TYPE (0/1/2)"   -- to do: сделать эту опцию OptArg
   ,OPTION "ad"    "adddir"             "add arcname to extraction path"
   ,OPTION "ag"    "autogenerate"       "autogenerate archive name with FMT"       -- to do: сделать эту опцию OptArg
   ,OPTION ""      "noarcext"           "don't add default extension to archive name"
   ,OPTION "tk"    "keeptime"           "keep original archive time"
   ,OPTION "tl"    "timetolast"         "set archive time to latest file"
   ,OPTION "fn"    "fullnames"          "match with full names"
   ,OPTION ""      "append"             "add new files to the end of archive"
   ,OPTION ""      "recompress"         "recompress archive contents"
   ,OPTION ""      "dirs"               "add empty dirs to archive"
   ,OPTION "ed"    "nodirs"             "don't add empty dirs to archive"
   ,OPTION ""      "nodates"            "don't store filetimes in archive"
   ,OPTION ""      "cache"              "use N mbytes for read-ahead cache"
   ,OPTION "lc"    "LimitCompMem"       "limit memory usage for compression to N mbytes"
   ,OPTION "ld"    "LimitDecompMem"     "limit memory usage for decompression to N mbytes"
   ,OPTION ""      "nodir"              "don't write archive directories"
   ,OPTION ""      "nodata"             "don't store data in archive"
   ,OPTION ""      "crconly"            "save/check CRC, but don't store data"
   ,OPTION "di"    "display"           ("control AMOUNT of information displayed: ["++aDISPLAY_ALL++"]*")
   ,OPTION ""      "logfile"            "duplicate all information displayed to this FILE"
   ,OPTION ""      "print-config"       "display built-in definitions of compression methods"
   ,OPTION ""      "proxy"              "setups proxy(s) for URL access"
   ,OPTION ""      "bypass"             "setups proxy bypass list for URL access"
   ,OPTION ""      "original"           "redownload broken parts of archive from the URL"
   ,OPTION ""      "save-bad-ranges"    "save list of broken archive parts to the FILE"
   ,OPTION ""      "pause-before-exit"  "make a PAUSE just before closing program window"
   ,OPTION "ioff"  "shutdown"           "shutdown computer when operation completed"
   ,OPTION "v"     "volume"             "split archive to volumes each of SIZE bytes"
   ,OPTION ""      "queue"              "queue operations across multiple FreeArc copies"
   ]

-- |Список опций, которым имеют меньший приоритет при возникновении коллизий в разборе командной строки
aLEAST_PRIORITY_OPTIONS = words ""

-- |Список опций, которым надо отдавать предпочтение при возникновении коллизий в разборе командной строки
aPREFFERED_OPTIONS = words "method sfx charset SizeMore SizeLess overwrite shutdown type"

-- |Опции из предыдущего списка, имеющий максимальный приоритет :)
aSUPER_PREFFERED_OPTIONS = words "OldKeyfile"

-- |Скрыть пароли в командной строке (перед её выводом в лог)
hidePasswords args = map f args1 ++ args2 where
  (args1,args2)  =  break (=="--") args
  f "-p-"                                   =  "-p-"
  f ('-':'p':_)                             =  "-p"
  f "-op-"                                  =  "-op-"
  f ('-':'o':'p':_)                         =  "-op"
  f "-hp-"                                  =  "-hp-"
  f ('-':'h':'p':_)                         =  "-hp"
  f "--OldPassword-"                        =  "--OldPassword-"
  f x | "--OldPassword" `isPrefixOf` x      =  "--OldPassword"
  f "--HeadersPassword-"                    =  "--HeadersPassword-"
  f x | "--HeadersPassword" `isPrefixOf` x  =  "--HeadersPassword"
  f "--password-"                           =  "--password-"
  f x | "--password" `isPrefixOf` x         =  "--password"
  f x = x


-- |Описание команд, поддерживаемых программой
commandsList = [
    "a        add files to archive"
  , "c        add comment to archive"
  , "ch       modify archive (recompress, encrypt and so on)"
  , "create   create new archive"
  , "cw       write archive comment to file"
  , "d        delete files from archive"
  , "e        extract files from archive ignoring pathnames"
  , "f        freshen archive"
  , "j        join archives"
  , "k        lock archive"
  , "l        list files in archive"
  , "lb       bare list of files in archive"
  , "lt       technical archive listing"
  , "m        move files and dirs to archive"
  , "mf       move files to archive"
  , "modify   modify archive using +/-/* actions"
  , "r        recover archive using recovery record"
  , "rr       add recovery record to archive"
  , "s        convert archive to SFX"
  , "t        test archive integrity"
  , "u        update files in archive"
  , "v        verbosely list files in archive"
  , "x        extract files from archive"
  ]

-- |Список команд, поддерживаемых программой
aLL_COMMANDS = map (head.words) commandsList

-- |Список команд, которые просто копируют архив
is_COPYING_COMMAND ('r':'r':_) = True
is_COPYING_COMMAND ('s':_)     = True
is_COPYING_COMMAND x           = x `elem` words "c ch d j k"

-- |Команда, у которой НЕ ДОЛЖНО быть ни одного аргумента (помимо имени архива)
is_CMD_WITHOUT_ARGS x  =  is_COPYING_COMMAND x  &&  (x `notElem` words "d j")

-- Self-explained :)
is_CMD_CREATE = (=="create")
is_CMD_MODIFY = (=="modify")

-- |Классификация всех команд по четырём типам: команды упаковки, распаковки, тестирования и листинга
data CmdType = ADD_CMD | EXTRACT_CMD | TEST_CMD | LIST_CMD | RECOVER_CMD  deriving (Eq)
cmdType "t"  = TEST_CMD
cmdType "e"  = EXTRACT_CMD
cmdType "x"  = EXTRACT_CMD
cmdType "cw" = EXTRACT_CMD
cmdType "l"  = LIST_CMD
cmdType "lb" = LIST_CMD
cmdType "lt" = LIST_CMD
cmdType "v"  = LIST_CMD
cmdType "r"  = RECOVER_CMD
cmdType  _   = ADD_CMD
{-# NOINLINE cmdType #-}

-- |Версия архиватора, записываемая в HEADER BLOCK
aARCHIVE_VERSION = make4byte 0 0 6 7

{-# NOINLINE aARC_VERSION_WITH_DATE #-}
{-# NOINLINE aARC_HEADER_WITH_DATE #-}
{-# NOINLINE aARC_HEADER #-}
{-# NOINLINE aARC_VERSION #-}
{-# NOINLINE aARC_AUTHOR #-}
{-# NOINLINE aARC_EMAIL #-}
{-# NOINLINE aARC_WEBSITE #-}
{-# NOINLINE aARC_LICENSE #-}
-- |Краткое наименование программы, выводимое в начале работы
aARC_VERSION_WITH_DATE = aARC_VERSION                              -- aARC_VERSION ++ " ("++aARC_DATE++")"
aARC_HEADER_WITH_DATE  = aARC_HEADER                               -- aARC_HEADER  ++ " ("++aARC_DATE++")"
aARC_HEADER  = aARC_NAME++" "++aARC_VERSION++" "
aARC_VERSION = (if aFreeArc == "FreeArc" then "0.67" else "1.0") ++" ("++aARC_DATE++")"                            --  "0.666"
aARC_DATE    = "March 15 2014"
aARC_NAME    = aFreeArc
aARC_AUTHOR  = "Bulat Ziganshin"
aARC_EMAIL   = "Bulat.Ziganshin@gmail.com"
aARC_WEBSITE = "http://freearc.org"
aARC_LICENSE = ["0459 High-performance archiver", "0460 Free as well for commercial as for non-commercial use"]

{-# NOINLINE aHELP #-}
-- |HELP, выводимый при вызове программы без параметров
aHELP = aARC_HEADER++" "++aARC_WEBSITE++"  "++aARC_DATE++"\n"++
        joinWith ". " (map i18no aARC_LICENSE)++"\n"++
        "Usage: Arc command [options...] archive [files... @listfiles...]\n" ++
        joinWith "\n  " ("Commands:":commandsList) ++ "\nOptions:\n" ++ optionsHelp

-- |Способы группировки файлов для солид-блока или оглавления архива
data Grouping = GroupNone                   -- каждый файл отдельно
                                            -- группировка по:
              | GroupByExt                  --   одинаковому расширению
              | GroupBySize      FileSize   --   минимальному объёму блока данных
              | GroupByBlockSize MemSize    --   максимальному объёму блока данных (для блочно-ориентированных алгоритмов, таких как BWT и ST)
              | GroupByNumber    FileCount  --   количеству файлов
              | GroupAll                    -- все файлы вместе

-- |Значение опции -d[f]: не удалять, удалять только файлы, удалять файлы и каталоги
data DelOptions = NO_DELETE | DEL_FILES | DEL_FILES_AND_DIRS  deriving (Eq)

-- |Тип выполняемой операции
data OP_TYPE = COMPRESSION | DECOMPRESSION  deriving (Eq)


---------------------------------------------------------------------------------------------------
-- ЗНАЧЕНИЯ, ИСПОЛЬЗУЕМЫЕ ПО УМОЛЧАНИЮ ------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Метод сжатия данных
aDEFAULT_COMPRESSOR = "4"

-- |Метод сжатия каталога архива
aDEFAULT_DIR_COMPRESSION = "lzma:mfbt4:d1m"

-- |Размер солид-блоков (один солид-блок на всех)
aDEFAULT_DATA_GROUPING  =  ""

-- |Группировка для каталогов
aDEFAULT_DIR_GROUPING  =  GroupByNumber (100*1000)

-- |Алгоритм шифрования
aDEFAULT_ENCRYPTION_ALGORITHM = "aes"

-- |Если в командной строке не указаны имена обрабатываемых файлов - обрабатывать все, т.е. "*"
aDEFAULT_FILESPECS = [reANY_FILE]

-- |Расширение архивных файлов
aDEFAULT_ARC_EXTENSION = "."++aFreeArcInternalExt

-- |Расширение SFX архивных файлов
#ifdef FREEARC_WIN
aDEFAULT_SFX_EXTENSION = ".exe"
#else
aDEFAULT_SFX_EXTENSION = ""
#endif

-- |Файл локализации по умолчанию
aLANG_FILE = "arc.language.txt"

-- |Файл локализации для непереведённых tooltips
aENGLISH_LANG_FILE = "arc.english.txt"

-- |Файл с описанием порядка сортировки имён файлов при "-og"
aDEFAULT_GROUPS_FILE = "arc.groups"

-- |SFX-модуль, используемый по умолчанию, в зависимости от типа архива
default_sfx t | t==aFreeArcInternalExt = "freearc.sfx"
              | otherwise              = t++".sfx"

-- |Файлы конфигурации (хранящие опции, используемые по умолчанию)
aCONFIG_FILES = "arc*.ini"

-- |Переменная среды, содержащая опции, используемые по умолчанию
aCONFIG_ENV_VAR = "FREEARC"

-- |Порядок сортировки, используемый при solid-сжатии (для увеличения сжатия)
aDEFAULT_SOLID_SORT_ORDER = "gerpn"

-- |Объём информации, выводимой на экран - по умолчанию и при использовании опции "--display" без параметра.
-- По умолчанию на экран не выводятся "cmo" - доп. опции, режим сжатия и используемая память
aDISPLAY_DEFAULT = "hanwrftske"
aDISPLAY_ALL     = "hoacmnwrfdtske"

-- Секции arc.ini
aCOMPRESSION_METHODS = "[Compression methods]"
aDEFAULT_OPTIONS     = "[Default options]"
aEXTERNAL_COMPRESSOR = "[External compressor:*]"

-- |Проверка того, что это - заголовок секции
isSectionHeading  =  beginWith "[" . trim

-- |Приведение имени секции к стандартизованной форме
cleanupSectionName  =  strLower . filter (not.isSpace)


----------------------------------------------------------------------------------------------------
---- Универсальный парсер командной строки ---------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Описание опции - краткое имя, длинное имя, печатаемое описание
data Option = OPTION String String String  deriving Show

-- |Тип опции - короткие имеют префикс "-", длинные - префикс "--"
data OptType  =  SHORT | LONG deriving Show

-- |Наличие параметра в опции: нет/обязательно/опционально
data ParamType  =  ParamNo | ParamReq | ParamOpt deriving Show

-- |"Словарь" опций, содержащий их в удобной для разбора командной строки форме
optionsDict  =  concatMap compileOption optionsList
  where compileOption (OPTION short long description)  =  compile short ++ compile ('-':long)
          where -- Добавить в список описание опции с именем `name`, если оно непустое
                compile name  =  case (name, paramName description) of
                    ("",  _      )  ->  []                                -- нет имени - нет и опции :)
                    ("-", _      )  ->  []                                -- нет имени - нет и опции :)
                    (_,   Nothing)  ->  [(name, long|||short, ParamNo )]  -- опция без параметра
                    (_,   Just _ )  ->  [(name, long|||short, ParamReq)]  -- опция с параметром

-- |Описание опций для пользователя.
optionsHelp  =  init$ unlines table
  where (ss,ls,ds)     = (unzip3 . map fmtOpt) optionsList
        table          = zipWith3 paste (sameLen ss) (sameLen ls) ds
        paste x y z    = "  " ++ x ++ "  " ++ y ++ "  " ++ z
        sameLen xs     = flushLeft ((maximum . map length) xs) xs
        flushLeft n    = map (left_justify n)
          -- Возвращает формат "короткой опции", "длинной опции", и их описание
        fmtOpt (OPTION short long description)  =  (format short "" description, format ('-':long) "=" description, description)
          -- Возвращает формат опции `name` с учётом наличия у неё имени и параметра
        format name delim description  =  case (name, paramName description) of
                                            ("",   _         )  ->  ""
                                            ("-",  _         )  ->  ""
                                            ("--", _         )  ->  "--"
                                            (_,    Nothing   )  ->  "-"++name
                                            (_,    Just aWORD)  ->  "-"++name++delim++aWORD

-- |Возвращает имя параметра опции, извлекая его из строки её описания.
paramName descr =
  case filter (all isUpper) (words descr)
    of []      -> Nothing      -- Описание не содержит UPPERCASED слов
       [aWORD] -> Just aWORD   -- Описание включает UPPERCASED слово, обозначающее параметр опции
       _       -> error$ "option description \""++descr++"\" contains more than one uppercased word"

-- |Разбор командной строки, возвращающий список опций и список "свободных аргументов"
parseOptions []          _ options freeArgs  =  return (reverse options, reverse freeArgs)
parseOptions ("--":args) _ options freeArgs  =  return (reverse options, reverse freeArgs ++ args)

parseOptions (('-':option):args) option_checks options freeArgs = do
  let check (prefix, _, ParamNo)  =  (option==prefix)
      check (prefix, _, _)        =  (startFrom prefix option /= Nothing)
  let check2 (prefix, name, haveParam)  =  (lookup name option_checks `defaultVal` const True)  (tryToSkip "=" (tryToSkip prefix option))
  let accept (prefix, name, haveParam)  =  return (name, tryToSkip "=" (tryToSkip prefix option))
      unknown                           =  registerError$ CMDLINE_UNKNOWN_OPTION ('-':option)
      ambiguous variants                =  registerError$ CMDLINE_AMBIGUOUS_OPTION ('-':option) (map (("--"++).snd3) variants)      -- to do: "-" or "--" depending on short/long option
  newopt <- case (filter check2 $ filter check optionsDict) of
              [opt] -> accept opt  -- принять опцию
              []    -> unknown     -- неизвестная опция.
              xs    -> -- При неоднозначности в разборе опции посмотрим на список предпочтительных опций
                       case (filter ((`notElem` aLEAST_PRIORITY_OPTIONS) . snd3) xs) of
                         [opt] -> accept opt        -- принять опцию
                         []    -> ambiguous xs      -- неоднозначная опция, которой нет в списке предпочтений
                         xs    -> -- Повторим трюк! :)
                                  case (filter ((`elem` aPREFFERED_OPTIONS++aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                                    [opt] -> accept opt        -- принять опцию
                                    []    -> ambiguous xs      -- неоднозначная опция, которой нет в списке предпочтений
                                    xs    -> -- Повторим трюк! :)
                                             case (filter ((`elem` aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                                               [opt] -> accept opt        -- принять опцию
                                               []    -> ambiguous xs      -- неоднозначная опция, которой нет в списке предпочтений
                                               xs    -> ambiguous xs      -- неоднозначный разбор даже в списке предпочтений!

  parseOptions args option_checks (newopt:options) freeArgs

parseOptions (arg:args) option_checks options freeArgs   =   parseOptions args option_checks options (arg:freeArgs)


-- |Вернуть список значений опции с названием `flag`. Пример вызова: findReqList opts "exclude"
findReqList ((name, param):flags) flag  | name==flag  =  param: findReqList flags flag
findReqList (_:flags) flag                            =  findReqList flags flag
findReqList [] flag                                   =  []

-- |Вернуть значение опции с названием `flag`, если её нет - значение по умолчанию `deflt`
findReqArg options flag deflt  =  last (deflt : findReqList options flag)

-- |Вернуть значение опции с необязательным параметром
findOptArg = findReqArg

-- |Вернуть значение опции с названием `flag`, если её нет - Nothing
findMaybeArg options flag  =  case findReqList options flag
                                of [] -> Nothing
                                   xs -> Just (last xs)

-- |Вернуть True, если в списке опций есть опция с названием `flag`
findNoArg options flag  =  case findReqList options flag
                                of [] -> False
                                   _  -> True

-- |Вернуть Just True, если в списке опций есть опция с названием `flag1`,
--          Just False, если в списке опций есть опция с названием `flag2`,
--          Nothing, если нет ни той, ни другой
findNoArgs options flag1 flag2  =  case filter (\(o,_) -> o==flag1||o==flag2) options
                                     of [] -> Nothing
                                        xs -> Just (fst (last xs) == flag1)

{-# NOINLINE optionsDict #-}
{-# NOINLINE optionsHelp #-}
{-# NOINLINE parseOptions #-}
{-# NOINLINE findReqList #-}
{-# NOINLINE findReqArg #-}
{-# NOINLINE findMaybeArg #-}
{-# NOINLINE findNoArg #-}
{-# NOINLINE findNoArgs #-}


---------------------------------------------------------------------------------------------------
---- Интерпретация Lua-скриптов                                                                ----
---------------------------------------------------------------------------------------------------

#if defined(FREEARC_NO_LUA)
-- Lua support disabled, just imitate it
type LuaState = ()
luaInit      = return ()
luaRun _ _ _ = return ()
#else

-- |Instance of Lua interpreter (separate instances may be created for every command executed)
type LuaState = Lua.LuaState

-- |Create new Lua instance
luaInit = do
  l <- Lua.newstate
  Lua.openlibs l
  -- Init event handler lists
  for luaEvents (addLuaEvent l)
  -- Execute configuration scripts, adding handlers for events
  scripts <- findFiles configFilePlaces "arc.*.lua"
  for scripts (Lua.dofile l)
  return l

-- |Execute Lua scripts assigned to the event cmd
luaRun l cmd params = do
  Lua.callproc l cmd params
  return ()

-- |Add support of event cmd to Lua instance
addLuaEvent l cmd = Lua.dostring l $ unlines
                      [ handlers++" = {}"
                      , "function on"++cmd++"(handler)"
                      , "  table.insert ("++handlers++", handler)"
                      , "end"
                      , "function "++cmd++"(params)"
                      , "  for _,handler in ipairs("++handlers++") do"
                      , "    handler(params)"
                      , "  end"
                      , "end"
                      ]        where handlers = "on"++cmd++"Handlers"

-- |Lua events list
luaEvents = words "ProgramStart ProgramDone CommandStart CommandDone"++
            words "ArchiveStart ArchiveDone Error Warning"

#endif


-- |The global Lua instance
{-# NOINLINE lua_state #-}
lua_state :: MVar LuaState
lua_state = unsafePerformIO $ do
   lua <- luaInit
   errorHandlers   ++= [\msg -> luaEvent "Error"   [("message", msg)]]
   warningHandlers ++= [\msg -> luaEvent "Warning" [("message", msg)]]
   newMVar lua

-- |Run Lua event in the global Lua instance
luaEvent  =  liftMVar3 luaRun lua_state

-- |Perform Start/Done procedures of givel level
luaLevel level params action = do
  luaEvent (level++"Start") params
  ensureCtrlBreak "luaDone" (luaEvent (level++"Done") [""]) action


----------------------------------------------------------------------------------------------------
---- Операции с файлом истории ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

#ifdef FREEARC_GUI
-- |Файл настроек программы
aINI_FILE = "freearc.ini"
-- |Имя старого файла настроек
aOLD_HISTORY_FILE = "freearc.history"

-- |Файл настроек
data HistoryFile = HistoryFile { hf_history_file :: MVar FilePath
                               , hf_history      :: IORef (Maybe [String])
                               }

-- |Создать структуру, хранящую файл истории
openHistoryFile = do
  history_file <- findOrCreateFile configFilePlaces aINI_FILE >>= mvar
  history      <- ref Nothing
  hfUpdateConfigFile$ HistoryFile { hf_history_file = history_file
                                  , hf_history      = history
                                  }

-- |Добавить значение в список истории (удалив предыдущие точно такие же строки)
hfAddHistory hf tags text     =   hfModifyHistory hf tags text (\tag line -> (line==))
-- |Заменить значение в списке истории (удалив предыдущие значения с этим тегом)
hfReplaceHistory hf tags text  =  hfModifyHistory hf tags text (\tag line -> (tag==).fst.split2 '=')
-- |Добавить/Заменить значение в списке истории
hfModifyHistory hf tags text deleteCond = ignoreErrors $ do
  -- Занесём новый элемент в голову списка и избавимся от дублирующих значений
  let newItem  =  join2 "=" (mainTag, text)
      mainTag  =  head (split '/' tags)
  withMVar (hf_history_file hf) $ \history_file -> do
    modifyConfigFile history_file ((newItem:) . deleteIf (deleteCond mainTag newItem))

-- |Заменить старый тег на новый, сохранив значения
hfChangeTag hf old new = do
  values <- hfGetHistory hf old
  for values (hfAddHistory hf new)
  hfDeleteTagFromHistory hf old

-- |Удалить тег из списка истории
hfDeleteTagFromHistory hf tag  =  hfDeleteConditionalFromHistory hf (\tag1 value1 -> tag==tag1)

-- |Удалить из списка истории строки по условию
hfDeleteConditionalFromHistory hf cond = ignoreErrors $ do
  withMVar (hf_history_file hf) $ \history_file -> do
    modifyConfigFile history_file (deleteIf ((uncurry cond).split2 '='))

-- |Извлечь список истории по заданному тэгу/тэгам
hfGetHistory1 hf tags deflt = do x <- hfGetHistory hf tags; return (head (x++[deflt]))
hfGetHistory  hf tags       = handle (\_ -> return []) $ do
  hist <- hfGetConfigFile hf
  hist.$ map (split2 '=')                           -- разбить каждую строку на тэг+значение
      .$ filter ((split '/' tags `contains`).fst)   -- отобрать строки с тэгом из списка tags
      .$ map snd                                    -- оставить только значения.
      .$ map (splitCmt "")                          -- разбить каждое значение на описание+опции
      .$ mapM (\x -> case x of                      -- локализовать описание и слить их обратно
                       ("",b) -> return b
                       (a ,b) -> do a <- i18n a; return$ join2 ": " (a,b))

-- Чтение/запись в историю целочисленного значения
hfGetHistoryInt     hf tag deflt  =  hfGetHistory1 hf tag (show deflt)  >>==  readInt
hfReplaceHistoryInt hf tag x      =  hfReplaceHistory hf tag (show x)


-- Чтение/запись в историю булевского значения
hfGetHistoryBool     hf tag deflt  =  hfGetHistory1 hf tag (bool2str deflt)  >>==  (==bool2str True)
hfReplaceHistoryBool hf tag x      =  hfReplaceHistory hf tag (bool2str x)
bool2str True  = "1"
bool2str False = "0"


-- |Получить содержимое файла истории
hfGetConfigFile hf = do
  history <- val (hf_history hf)
  case history of
    Just history -> return history
    Nothing      -> withMVar (hf_history_file hf) readConfigFile

-- |На время выполнения этих скобок содержимое файла истории читается из поля hf_history
hfCacheConfigFile hf =
  bracket_ (do history <- hfGetConfigFile hf
               hf_history hf =: Just history)
           (do hf_history hf =: Nothing)


-- |Обновляет конфиг-файлы, если произошёл переход на новую версию
hfUpdateConfigFile hf = do
  let updates = [("000.60.01", do for (words "Modified Size Name") $ \colname -> do
                                    hfChangeTag hf (colname++"ColumnWidth") ("FileManager.ColumnWidth."++colname)
                                  hfChangeTag hf "ColumnOrder" "FileManager.ColumnOrder"
                                  hfReplaceHistoryBool hf "CheckNews" True

               ),("000.61.01", do ex <- hfGetHistory hf "extract_all_for"
                                  for (ex ||| ["*.exe *.msi *.htm *.html"]) (hfAddHistory hf "ExtractAllFor")
                                  hfDeleteTagFromHistory hf "extract_all_for"
                                  hfChangeTag hf "Settings.Associate.Zip" "Settings.AssociateNonArc"

               ),("000.67.03", do hfReplaceHistory hf "compression.7zLast" "0110 Normal: -mx5"
                                  hfReplaceHistory hf "compression.zipLast" "0110 Normal: -mx5"
                                  hfAddHistory hf "RunFor" "*.exe *.msi *.doc *.docx *.xls *.xlsx"

               ),("000.67.04", do hfReplaceHistory hf "volumesLast" "0504 RapidShare: 200mb"
                                  hfDeleteConditionalFromHistory hf (\tag value -> tag=="volumes" && is_i18 value)
                                  hfAddHistory hf "volumes" "0501 DVD: 4474mb"
                                  hfAddHistory hf "volumes" "0502 FAT: 2047mb"
                                  hfAddHistory hf "volumes" "0503 CD: 700mb"
                                  hfAddHistory hf "volumes" "0504 RapidShare: 200mb"
                                  hfAddHistory hf "volumes" "0505 SkyDrive: 50mb"
                                  hfAddHistory hf "volumes" "0506 Mail attachment: 5mb"

               ),("000.67.06", do hfChangeTag  hf "Settings.Associate"            "Settings.Associate.FreeArc.Enabled"
                                  hfChangeTag  hf "Settings.AssociateNonArc"      "Settings.Associate.OtherArc.Enabled"
                                  hfAddHistory hf "Settings.Associate.FreeArc"     associate_FreeArc_deflt
                                  hfAddHistory hf "Settings.Associate.SmartMenu"   associate_SmartMenu_deflt
               ),("000.67.07", do hfAddHistory hf "Settings.Associate.OtherArc"    associate_OtherArc_deflt
                                  hfAddHistory hf "Settings.Associate.ContextMenu" associate_ContextMenu_deflt

               ),("000.67.08", do hfReplaceHistory hf "sfx.7zLast" "0765 Windows GUI: 7z.sfx"
                                  hfAddHistory hf "sfx.7z" "0768 Convert EXE back to ARC: -"
                                  hfAddHistory hf "sfx.7z" "0766 Windows console: 7zCon.sfx"
                                  hfAddHistory hf "sfx.7z" "0765 Windows GUI: 7z.sfx"

               ),("000.67.09", do hfDeleteConditionalFromHistory hf (\tag value -> tag=="compression.7z" && is_i18 value)
                                  hfAddHistory hf "compression.7z" "0752 No compression: -mx0"
                                  hfAddHistory hf "compression.7z" "0112 Fastest: -mx1"
                                  hfAddHistory hf "compression.7z" "0111 Fast: -mx3"
                                  hfAddHistory hf "compression.7z" "0110 Normal: -mx5"
                                  hfAddHistory hf "compression.7z" "0108 Maximum: -mx7"
                                  hfAddHistory hf "compression.7z" "0548 Ultra: -mx9"

                                  hfDeleteConditionalFromHistory hf (\tag value -> tag=="compression.zip" && is_i18 value)
                                  hfAddHistory hf "compression.zip" "0752 No compression: -mx0"
                                  hfAddHistory hf "compression.zip" "0111 Fast: -mx1"
                                  hfAddHistory hf "compression.zip" "0110 Normal: -mx5"
                                  hfAddHistory hf "compression.zip" "0108 Maximum: -mx9"

               ),("000.67.10", do hfReplaceHistory hf "compressionLast" "0110 Normal: -m4"
                                  hfDeleteConditionalFromHistory hf (\tag value -> tag=="compression" && is_i18 value)
                                  hfAddHistory hf "compression" "0752 No compression: -m0"
                                  hfAddHistory hf "compression" "0527 Instant: -m1"
                                  hfAddHistory hf "compression" "0127 HDD-speed: -m2"
                                  hfAddHistory hf "compression" "0111 Fast: -m3"
                                  hfAddHistory hf "compression" "0110 Normal: -m4"
                                  hfAddHistory hf "compression" "0109 High: -m5"
                                  hfAddHistory hf "compression" "0775 Best asymmetric (with fast decompression): -m9x -ld192m"
                                  hfAddHistory hf "compression" "0774 Maximum (require 1 gb RAM for decompression): -mx -ld800m"
                                  hfAddHistory hf "compression" "0773 Ultra (require 2 gb RAM for decompression): -mx -ld1600m"

               )]

  let newVersion = fst (last updates)
  lastVersion <- hfGetHistory1 hf "ConfigVersion" ""

  -- Перенести freearc.history в freearc.ini
  lastVersion <- if (lastVersion >= newVersion)   then return lastVersion   else do
    oldhf <- findFile configFilePlaces aOLD_HISTORY_FILE
    when (oldhf>"") $ do
      oldhist <- readConfigFile oldhf
      withMVar (hf_history_file hf) (`modifyConfigFile` (++oldhist))
    hfGetHistory1 hf "ConfigVersion" ""

  -- Скопировать .ini в локальный каталог если UsePersonalSettings==True
  usePersonal  <- hfGetHistoryBool hf "UsePersonalSettings" False
  personalName <- findOrCreateFile personalConfigFilePlaces aINI_FILE
  ininame      <- readMVar (hf_history_file hf)
  if (usePersonal && ininame/=personalName)
    then do buildPathTo personalName
            fileCopy ininame personalName
            openHistoryFile  -- переключиться на локальный .ini
    else do

  -- Внести обновления, соответствующие более новым версиям конфиг-файла
  for updates $ \(version,action) -> when (lastVersion < version) $ do
                                       action
                                       hfReplaceHistory hf "ConfigVersion" version
  return hf



-- |Записывает ArcShellExt-config.lua
writeShellExtScript hf' = do
  hfCacheConfigFile hf' $ do
  contextMenu <- hfGetHistoryBool hf' "Settings.ContextMenu"          False
  cascaded    <- hfGetHistoryBool hf' "Settings.ContextMenu.Cascaded" True
  commands    <- getExplorerCommands >>== filter(not.null.fst3)
  cmdEnabled  <- foreach commands $ \(cmdname,itext,imsg) -> do
                     hfGetHistoryBool hf' ("Settings.ContextMenu.Command."++cmdname) True

  -- Return value of setting if enabled, "" otherwise
  let getHistory key always_enabled deflt = do
                               enabled <- hfGetHistoryBool hf' (key++".Enabled")   False
                               str     <- hfGetHistory1    hf' key                 deflt
                               return ((enabled || always_enabled)  &&&  unwords (deleteIf null (words str)))   -- remove any extra spaces
  --
  freearc_extensions        <- getHistory "Settings.Associate.FreeArc"     True  associate_FreeArc_deflt
  other_archive_extensions1 <- getHistory "Settings.Associate.OtherArc"    True  associate_OtherArc_deflt
  other_archive_extensions2 <- getHistory "Settings.Associate.ContextMenu" False associate_ContextMenu_deflt
  smart_menu_extensions     <- getHistory "Settings.Associate.SmartMenu"   False associate_SmartMenu_deflt

  exe <- getExeName                                -- Name of FreeArc.exe file
  let dir   =  exe.$takeDirectory                  -- FreeArc.exe directory
      shext =  dir </> "ArcShellExt"               -- Shell extension directory

  -- Generate ArcShellExt config script
  all2arc <- all2arc_path
  let q str = "\"" ++ str.$replaceAll "\"" "\\\"" ++ "\""
  let script = [ "-- This file uses UTF8 encoding"
               , ""
               , "-- 1 for cascaded menus, nil for flat"
               , "cascaded = "++(iif cascaded "true" "false")
               , ""
               , "-- Commands"
               , "command = {}"
               ] ++ zipWith (\enabled (cmdname,text,help) ->
                            (not enabled &&& "-- ")++"command."++cmdname++" = {text = "++q text++", help = "++q help++"}")
                    cmdEnabled commands ++
               [ ""
               , "-- Archiver name"
               , "FreeArcName = \""++aFreeArc++"\""
               , ""
               , "-- Path to FreeArc executable file"
               , "freearc = \"\\\""++(exe.$replaceAll "\\" "\\\\")++"\\\"\""
               , ""
               , "-- Path to All2Arc executable file"
               , "all2arc = \"\\\""++(all2arc.$replaceAll "\\" "\\\\")++"\\\"\""
               , ""
               , "-- FreeArc default archive extension"
               , "freearc_ext = \""++aFreeArcInternalExt++"\""
               , ""
               , "-- FreeArc SFX extension"
               , "freearc_sfx_ext = \"exe\""     -- todo: Unix
               , ""
               , "-- FreeArc archive file extensions"
               , "freearc_all_ext = \""++freearc_extensions++"\""
               , ""
               , "-- FreeArc SFX and possibe archive file extensions"
               , "smart_menu_ext = \""++smart_menu_extensions++"\""
               , ""
               , "-- Other archive file extensions"
               , "archives_ext = \""++trim(other_archive_extensions1++" "++other_archive_extensions2)++"\""
               , ""
               ]
  -- Save script to file
  usePersonal <- hfGetHistoryBool hf' "UsePersonalSettings" False
  configDir <- if usePersonal then findOrCreateFile personalConfigFilePlaces "ArcShellExt"
                              else return shext
  createDirectoryHierarchy configDir
  saveWindowsConfigFile (configDir </> "ArcShellExt-config.lua") script


-- |Путь к all2arc
all2arc_path = do
  exe <- getExeName                              -- Name of FreeArc.exe file
  let dir  = exe.$takeDirectory                  -- FreeArc.exe directory
  return (dir </> "all2arc.exe")

-- Default extension lists
associate_FreeArc_deflt     = aFreeArcInternalExt
associate_OtherArc_deflt    = "zip zipx jar apk rar r00 7z cab arj tar bz2 bzip2 tbz2 tbz gz gzip tgz tpz lzh lha lzma pmd xar xz txz z taz"
associate_ContextMenu_deflt = "cpio cramfs dmg fat deb rpm mbr ntfs te swf flv squashfs scap vhd dll sys nsis chm chi chq chw hxs hxi hxr hxq hxw lit msi msp doc xls ppt hfs iso udf img wim swm xpi odt ods docx xlsx 001"
associate_SmartMenu_deflt   = "exe"

translateExplorerCommand (cmdname,etext,emsg) = do [itext,imsg] <- i18ns [etext,emsg]
                                                   return (cmdname, itext, imsg)

-- |Возвращает список локализованных описаний команд, интегрируемых в Explorer
getExplorerCommands = mapM translateExplorerCommand$
  [ (""           ,  ""                         ,  ""                                                                )
#ifndef HAMSTER
  , ("add2arc"    ,  "0391 Add to \"%s\""       ,  "0392 Compress the selected files using "++aFreeArc               )
  , ("add2sfx"    ,  "0393 Add to SFX \"%s\""   ,  "0394 Compress the selected files into SFX using "++aFreeArc      )
#endif
  , ("add2zip"    ,  "0391 Add to \"%s\""       ,  "0489 Compress the selected files to .zip"                        )
  , ("add2_7z"    ,  "0391 Add to \"%s\""       ,  "0490 Compress the selected files to .7z"                         )
  , ("add"        ,  "0395 Add to archive..."   ,  "0396 Compress the selected files using "++aFreeArc++" via dialog")
  , (""           ,  ""                         ,  ""                                                                )
  , ("open"       ,  "0397 Open with "++aFreeArc,  "0398 Open the selected archive(s) with "++aFreeArc               )
  , ("extractTo"  ,  "0399 Extract to \"%s\""   ,  "0400 Extract the selected archive(s) to new folder"              )
  , ("extractHere",  "0401 Extract here"        ,  "0402 Extract the selected archive(s) to the same folder"         )
  , ("extract"    ,  "0403 Extract..."          ,  "0404 Extract the selected archive(s) via dialog"                 )
  , ("test"       ,  "0405 Test"                ,  "0406 Test the selected archive(s)"                               )
#ifndef HAMSTER
  , (""           ,  ""                         ,  ""                                                                )
  , ("arc2sfx"    ,  "0407 Convert to SFX"      ,  "0408 Convert the selected archive(s) to SFX"                     )
  , ("sfx2arc"    ,  "0409 Convert from SFX"    ,  "0410 Convert the selected SFX(es) to normal archive(s)"          )
  , (""           ,  ""                         ,  ""                                                                )
  , ("modify"     ,  "0411 Modify..."           ,  "0412 Modify the selected archives via dialog"                    )
  , ("join"       ,  "0413 Join..."             ,  "0414 Join the selected archives via dialog"                      )
  , (""           ,  ""                         ,  ""                                                                )
  , ("zip2arc"    ,  "0415 Convert to .arc"     ,  "0416 Convert the selected archive(s) to "++aFreeArc++" format"   )
  , ("zip2sfx"    ,  "0417 Convert to .arc SFX" ,  "0418 Convert the selected archive(s) to "++aFreeArc++" SFX"      )
  , ("zip2a"      ,  "0419 Convert to .arc..."  ,  "0420 Convert the selected archive(s) to "++aFreeArc++" format via dialog")
#endif
  , (""           ,  ""                         ,  ""                                                                )
  , ("anyfile"    ,  "0491 Archive operations"  ,  "0492 Menu of archive operations for arbitrary file"              )
  ]



-- |Опции, общие для всех операций
readGuiOptions = do
  hf' <- openHistoryFile
  logfile' <- hfGetHistory1 hf' "logfile" ""
  tempdir' <- hfGetHistory1 hf' "tempdir" ""
  queue'   <- hfGetHistoryBool hf' "GlobalQueueing" False
  return $
       (logfile'  &&&  ["--logfile="++clear logfile'])++
       (tempdir'  &&&  ["--workdir="++clear tempdir'])++
       (queue'    &&&  ["--queue"])++
       []

#else
readGuiOptions = return []
#endif


----------------------------------------------------------------------------------------------------
---- Вспомогательные определения -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Выбирает один из нескольких вариантов по индексу
opt `select` variants  =  words (split ',' variants !! opt)
-- Выбирает один из нескольких вариантов по индексу
select1 prefix variants opt  =  words ((variants !! opt) .$unlessNull (prefix++))
-- Преобразует текст настройки в список опций, предварительно удаляя комментарий в её начале
cvt1 opt  =  map (opt++) . (||| [""]) . words . clear
-- То же самое, только имя опции добавляется только к словам, не начинающимся с "-"
cvt  opt  =  map (\w -> (w!~"-?*" &&& opt)++w) . (||| [""]) . words . clear
-- Удаляет комментарий вида "*: " в начале строки
clear     =  removeQuotes . trim . snd . splitCmt ""
-- |removeQuotes "text" возвращает просто text, остальное - без изменений
removeQuotes ('\x22':text) | endWith "\"" text  =  dropEnd 1 text
removeQuotes text                               =  text
-- |Разбивает значение на описание+опции
splitCmt "" text@('\x22':ws) | endWith "\"" ws  =  ("", text)   -- "text" возвращает просто "text" без описания
splitCmt xs ""           = ("", reverse xs)
splitCmt xs ":"          = (reverse xs, "")
splitCmt xs (':':' ':ws) = (reverse xs, ws)
splitCmt xs (w:ws)       = splitCmt (w:xs) ws


----------------------------------------------------------------------------------------------------
---- System information ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Number of physical processors/cores in the system. Determines number of heavy-computations thread runned
foreign import ccall unsafe "Environment.h GetProcessorsCount"
  getProcessorsCount :: CInt

-- |Size of physical computer memory in bytes
foreign import ccall unsafe "Environment.h GetPhysicalMemory"
  getPhysicalMemory :: Word64

-- |Size of maximum memory block we can allocate in bytes
foreign import ccall unsafe "Environment.h GetMaxBlockToAlloc"
  getMaxBlockToAlloc :: IO CUInt

-- |Size of physical computer memory that is currently unused
foreign import ccall unsafe "Environment.h GetAvailablePhysicalMemory"
  getAvailablePhysicalMemory :: IO Word64

-- |Prints detailed stats about memory available
foreign import ccall safe "Environment.h TestMalloc"
  testMalloc :: IO ()
