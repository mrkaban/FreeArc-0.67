{-# OPTIONS_GHC -cpp -XRecordWildCards #-}
----------------------------------------------------------------------------------------------------
---- Работа с архивами через 7z.dll.                                                          ------
---- Этот модуль содержит процедуры для:                                                      ------
----   * чтения структуры входного архива (т.е. каталогов и других служебных блоков)          ------
----   * распаковки архивов                                                                   ------
----------------------------------------------------------------------------------------------------
module Arhive7zLib where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Monad
import Control.OldException
import Data.HashTable as Hash
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Mem
import System.IO.Unsafe

import TABI
import Utils
import Errors
import Files
import qualified ByteStream
import Charsets
import FileInfo
import CompressionLib   (aFREEARC_OK, aFREEARC_ERRCODE_NOT_IMPLEMENTED, compressionQuery)
import Compression
import Encryption       (generateSzDecryption)
import UI
import Options
import ArhiveStructure

----------------------------------------------------------------------------------------------------
---- Описание структуры входного архива ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Вся необходимая информация о входном архиве
data ArchiveInfo = ArchiveInfo
         { arcArchive     :: !(Maybe Archive)    -- открытый файл архива .arc
         , arcFooter      :: FooterBlock         -- FOOTER BLOCK архива
         , arcDirectory   :: [CompressedFile]    -- файлы, содержащиеся в архиве
         , arcDataBlocks  :: [ArchiveBlock]      -- список солид-блоков
         , arcDirBytes    :: FileSize            -- размер служебных блоков в распакованном виде
         , arcDirCBytes   :: FileSize            -- размер служебных блоков в упакованном виде
         , arcDataBytes   :: FileSize            -- размер данных в распакованном виде
         , arcDataCBytes  :: FileSize            -- размер данных в упакованном виде
         , arcSzArchive   :: !(Maybe SzArchive)  -- Хендл архива из 7z.dll
         , arcArchiveType :: String              -- Тип архива
         }

-- |Проверка, что это архив, поддерживаемый самим FreeArc / через 7z.dll
isArcArchive  =  isJust . arcArchive
isSzArchive   =  isJust . arcSzArchive

-- |True, если архива на самом деле нет (используется для main_archive)
isArcPhantom = not . any_function [isArcArchive, isSzArchive]

-- Процедуры, упрощающие работу с архивами
arcGetPos  = archiveGetPos . fromJust . arcArchive
arcSeek    = archiveSeek   . fromJust . arcArchive
arcComment = ftComment . arcFooter

-- |Фантомный, несуществующий архив, необходимый для применения в некоторых операциях
-- (слияние списков файлов, закрытие входных архивов)
phantomArc  =  dirlessArchive Nothing (FooterBlock [] False "" "" 0)

-- |Архив без каталога файлов - используется только для вызова writeSFX из runArchiveRecovery
dirlessArchive archive footer   =   ArchiveInfo archive footer [] [] (error "emptyArchive:arcDirBytes") (error "emptyArchive:arcDirCBytes") (error "emptyArchive:arcDataBytes") (error "emptyArchive:arcDataCBytes") Nothing "-"

-- |Закрыть архивный файл, если только это не фантомный архив
arcClose arc  =  do whenJust (arcArchive arc) archiveClose
                    whenJust (arcSzArchive arc) szArcClose
                    return ()

-- |Check that given archive type (7z, zip...) supported by 7z.dll
szCheckType arctype  =  (unsafePerformIO$ withCWString arctype c_szCheckType) /= 0

-- |Return default extension for given arctype
szDefaultExtension arctype  =  unsafePerformIO$ withCWString arctype$ withTempCWString . c_szDefaultExtension

-- |Return possible arcType what 7z.dll is able to CREATE for given arcname or ""
szFindFormatForArchiveName arcname  =  unsafePerformIO$ withCWString (takeFileName arcname)$ withTempCWString . c_szFindFormatForArchiveName


----------------------------------------------------------------------------------------------------
---- Получение тех. информации об архиве в виде текстового блока -----------------------------------
----------------------------------------------------------------------------------------------------

arcGetTechinfo archive dirs_and_files = do
    let filelist    = map cfFileInfo (arcDirectory archive)
        footer      = arcFooter archive
        dataBlocks  = arcDataBlocks archive                                                         -- список солид-блоков
        numOfBlocks = length dataBlocks
        empty       = "-"
        arc x       = isArcArchive archive &&& x                                                    -- включить строчку x только для архивов .arc
        a7z x       = (isArcArchive archive || arcArchiveType archive=="7z")  &&&  x                -- .. только для .arc и .7z
        ifArc       = iif (isArcArchive archive)
    ;   yes        <- i18n"0101 Yes" >>== replaceAll "_" ""

    let origsize = arcDataBytes  archive                                                            -- суммарный объём файлов в распакованном виде
        compsize = arcDataCBytes archive                                                            -- суммарный объём файлов в упакованном виде
        getCompressors = partition isEncryption.blCompressor                                        -- разделить алг-мы шифрования и сжатия для блока
        (encryptors, tmp_compressors) = unzip$ map getCompressors dataBlocks                        -- списки алг. шифрования и сжатия
        header_encryptors = deleteIf null$ map (fst.getCompressors) (ftBlocks footer)               -- алгоритмы шифрования служебных блоков
        all_encryptors = deleteIf null encryptors ++ header_encryptors                              -- а теперь все вместе :)
        ciphers = joinWith "\n"$ removeDups$ map (join_compressor.map method_name) all_encryptors   -- имена алг. шифрования.
        ciphers_text = [("0097 Encryption algorithms:",  ciphers ||| empty)]

    let -- Максимальные словари алгоритмов сжатия
        compressors  = tmp_compressors.$ ifArc id (map (split_7z_compressor . head))                -- для не-arc архивов: разбить цепочку методов по пробелам
        dictionaries = compressors
                       .$ map (map (ifArc algomem algomem_7z))                                      -- каждый метод сжатия превращаем в алгоритм+квазисловарь
                       .$ (filter (not.null) . (map (filter ((>0).cmAlgoMem))))                     -- оставляем только методы с ненулевым словарём и состоящие из них непустые группы
                       .$ ifArc id (map (lastElems 1 . sortOn cmAlgoMem))                           -- для .7z оставляем только один алгоритм с макс. словарём (из нескольких выходов BCJ2)
                       .$ sort_and_groupOn (map cmAlgorithm)                                        -- группируем по сочетанию алгоритмов
                       .$ map (maxOn (maximum.map cmAlgoMem))                                       -- выбираем в каждой группе по максимуму любого из словарей
                       .$ sortOn (negate.maximum.map cmAlgoMem)                                     -- выносим вперёд цепочки с наибольшими словарями
                       .$ (joinWith " " . map (join_compressor . map showAlgoMem))                  -- репрезентация
        formatMem s =  x++" "++y  where (x,y) = span isDigit$ showMem s

    return$ filter(not.null) . map (concatMap (\x -> fst x &&& [x]))                                -- удаление пустых строк для не-.arc архивов
          $[[(    "0465 Archive type:",          arcArchiveType archive)]
            ++ dirs_and_files ++
            [(    "0089 Total bytes:",           show3$ origsize)
            ,(    "0090 Compressed bytes:",      show3$ compsize)
            ,(    "0091 Ratio:",                 compression_ratio compsize origsize)]

           ,[(arc "0104 Directory blocks:",      show3$ length$ filter ((DIR_BLOCK==).blType) (ftBlocks footer))
            ,(arc "0463 Directory, bytes:",      show3$ arcDirBytes archive)
            ,(a7z "0464 Directory, compressed:", show3$ arcDirCBytes archive)
            ,(a7z "0092 Solid blocks:",          show3$ numOfBlocks)
            ,(a7z "0093 Avg. blocksize:",        formatMem$ origsize `div` i(max numOfBlocks 1))]

           ,a7z [("0099 Compression memory:",    formatMem$ maximum$ 0: map (ifArc getMinCompressionMem   (getMem_7z cmCompressionMem  )) compressors)
                ,("0100 Decompression memory:",  formatMem$ maximum$ 0: map (ifArc getMinDecompressionMem (getMem_7z cmDecompressionMem)) compressors)
                ,("0105 Dictionary:",            dictionaries ||| empty)]

           ,arc [("0094 Archive locked:",        ftLocked footer   &&& yes ||| empty)
                ,("0098 Archive comment:",       ftComment footer  &&& yes ||| empty)
                ,("0095 Recovery info:",         ftRecovery footer ||| empty)
                ,("0096 SFX size:",              ftSFXSize footer .$show3 .$changeTo [("0", empty)])
                ,("0156 Headers encrypted:",     header_encryptors &&& yes ||| empty)]
             ++ arc (if ciphers>""   then []   else ciphers_text)
           ] ++ arc (ciphers &&& [ciphers_text])



-- Структура данных для хранения алгоритма+словаря метода сжатия
data AlgoMem  =  AlgoMem {cmAlgorithm :: !String,  cmAlgoMem :: !Integer,  cmCompressionMem :: Integer,  cmDecompressionMem :: Integer}

showAlgoMem AlgoMem{..}  =  join_method [cmAlgorithm, showMem cmAlgoMem]

-- | Вернуть алгоритм+квазисловарь. Для 4x4 алгоритм записываем как xПОДАЛГОРИТМ, а словарь берём из подалгоритма
algomem method  =  AlgoMem { cmAlgorithm        = name
                           , cmAlgoMem          = i$compressionQuery (const 0) "GetAlgoMem" name_with_params
                           , cmCompressionMem   = error "cmCompressionMem"
                           , cmDecompressionMem = error "cmDecompressionMem"
                           }
                       where (name,name_with_params) = case (split_method method) of
                                                         "4x4":m | name_with_params@(name:_) <- subMethod4x4 m  ->  ("x"++name, join_method name_with_params)
                                                         name:_                                                 ->  (name, method)

-- |Подметод в 4x4 (удаляем из начала списка параметров имеющие вид \d.* или [a-z]\d.*, остаток и есть подметод 4x4)
subMethod4x4  =  dropWhile (\param -> (length(param)>0 && isDigit (param!!0))  ||  (length(param)>1 && isDigit (param!!1)))


-- |Транслировать наименования методов сжатия zip/7z в объём памяти для упаковки/распаковки
getMem_7z getter = sum . map (getter . algomem_7z)

-- | Вернуть алгоритм+словарь для методов сжатия в zip/7z
algomem_7z method  =  AlgoMem {..}
    where (cmAlgorithm:params) = split_method method
          (cmAlgoMem, cmCompressionMem, cmDecompressionMem) =
              case (strLower cmAlgorithm) of
                  "lzma"       ->  (   dict,  cmem, dict+2*mb)  where dict = parseMem_7z $ fromMaybe "0" $ find (isDigit.head) params
                                                                      cmem = dict*(if dict >= 64*mb then 10 else 11) + (dict `div` 2) + 6*mb

                  "lzma2"      ->  (   dict,  cmem, dict+2*mb)  where dict = parseMem_7z $ fromMaybe "0" $ find (isDigit.head) params
                                                                      cmem = dict*(if dict >= 64*mb then 9 else 10) + chunk*2 + 8*mb
                                                                      chunk = dict*4

                  "ppmd"       ->  (   dict,   mem,       mem)  where dict = parseMem_7z $ fromMaybe "0" $ firstMaybe (startFrom "mem") params
                                                                      mem = dict+2*mb
                  "bzip2"      ->  ( 900*kb, 10*mb,      7*mb)
                  "deflate"    ->  (  32*kb,  3*mb,      2*mb)
                  "deflate64"  ->  (  64*kb,  3*mb,      2*mb)
                  "bcj2"       ->  (      0,  2*mb,      4*mb)
                  _            ->  (      0,     0,         0)

-- |Расшифровать запись объёма памяти: "512b", "32k", "8m" и так далее. "24" означает 2^24
parseMem_7z memstr =
    case (parseNumber memstr '^') of
        (bytes, 'b')  ->  bytes
        _             ->  0


----------------------------------------------------------------------------------------------------
---- Чтение структуры входного архива --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Open archive and list archive contents via callback
szListArchive arcname callback = do
  (archive,footer) <- szOpenArchive (Right callback) arcname

  archiveType <- szArcGetStrProperty archive (-1) kpidType
  numFiles    <- szArcItems archive
  TABI.call callback [ Pair "request"   "archive"
                     , Pair "arcname"   (W arcname)
                     , Pair "arctype"   archiveType
                     , Pair "files"     numFiles
                     ]

  for [0..numFiles-1] $ \i -> do
    Just cf <- szArcGetItem (const True) archive arcname i
    let fi = cfFileInfo cf
    TABI.call callback [ Pair "request"        "item"
                       , Pair "filename"       (W$ storedName fi)
                       , Pair "original"       (fiSize fi)
                       , Pair "compressed"     (fromJust$ cfCompsize cf)
                       , Pair "time"           (fromEnum$ fiTime fi)
                       , Pair "attr"           (fiAttr fi)
                       , Pair "is_folder?"     (fiIsDir fi)
                       , Pair "crc"            (cfCRC cf)
                       , Pair "is_encrypted?"  (cfIsEncrypted cf)
                       ]
    return ()


-- |Open archive
szOpenArchive command_or_callback arcname = do
#if !defined(FREEARC_WIN)
  sz_so <- findOrCreateFile libraryFilePlaces "7z.so"
  mySetEnv "P7ZIP_HOME_DIR" (dropFileName sz_so) True      -- set directory to search for 7z.so (system-wide 7z.so may be incompatible with our program!!!)
#endif
  withCWString arcname $ \c_arcname -> do
  alloca $ \archive -> do
  szCallback <- case command_or_callback of
                  Left  command  -> createSzCallback command (error "szOpenArchive: undefined arcinfo") (error "szOpenArchive: undefined can_be_extracted") (error "szOpenArchive::volumes")
                  Right callback -> return callback
  szCheckedTABI c_szOpenArchive [ Pair "archive"   archive
                                , Pair "arcname"   c_arcname
                                , Pair "callback"  (szCallback :: TABI.C_FUNCTION)
                                ]
  archive <- peek archive
  comment <- szArcGetStrProperty archive (-1) kpidComment
  sfxSize <- szArcGetIntProperty archive (-1) kpidOffset
  let footer = FooterBlock [] False comment "" sfxSize
  return (archive,footer)


-- |Read archive directory
szReadInfo archive footer filter_f processFooterInfo arcname = do
  processFooterInfo Nothing footer
  dirCBytes   <- szArcGetIntProperty archive (-1) kpidHeadersSize
  archiveType <- szArcGetStrProperty archive (-1) kpidType
  numFiles    <- szArcItems archive
  files       <- foreach [0..numFiles-1] (szArcGetItem filter_f archive arcname)  >>==  catMaybes
  -- Получение списка солид-блоков в архиве
  list        <- handle (\e -> return []) $ do   -- на случай исключения из-за отсутствия какого-либо из запрашиваемых атрибутов
                     foreach [0..numFiles-1] $ \item -> do
                         blck      <- szArcGetIntProperty  archive item kpidBlock
                         method    <- szArcGetStrProperty  archive item kpidMethod
                         origsize  <- szArcGetIntProperty  archive item kpidSize
                         compsize  <- szArcGetIntProperty  archive item kpidPackSize
                         return (blck, method, origsize, compsize)
  let blocks  =  list.$ groupOn fst4
                     .$ map (\xs -> ArchiveBlock {
                                        blArchive     = error "undefined ArchiveBlock::blArchive"
                                      , blType        = DATA_BLOCK
                                      , blCompressor  = [snd4 (head xs)]
                                      , blPos         = 0
                                      , blOrigSize    = sum (map thd4 xs)
                                      , blCompSize    = sum (map fth4 xs)
                                      , blCRC         = error "undefined ArchiveBlock::blCRC"
                                      , blFiles       = length xs
                                      , blIsEncrypted = False
                                    })
  return ArchiveInfo { arcArchive     = Nothing
                     , arcFooter      = footer
                     , arcDirectory   = files
                     , arcDataBlocks  = blocks
                     , arcDirBytes    = 0
                     , arcDirCBytes   = dirCBytes
                     , arcDataBytes   = sum (map (fiSize.cfFileInfo) files)
                     , arcDataCBytes  = sum (mapMaybe cfCompsize files)
                     , arcSzArchive   = Just archive
                     , arcArchiveType = archiveType
                     }

-- |Get info about one file in archive, return Nothing if it's filtered out by filter_f
szArcGetItem filter_f archive arcname item = do
  fi        <- szArcGetItemFileInfo archive arcname item
  if not (filter_f fi)  then return Nothing  else do
  fiCRC     <- szArcGetIntProperty  archive item kpidCRC
  compsize  <- szArcGetIntProperty  archive item kpidPackSize
  encrypted <- szArcGetBoolProperty archive item kpidEncrypted
  return$ Just$ CompressedFile fi
                               (error "szArcGetItem: undefined cfArcBlock")
                               (error "szArcGetItem: undefined cfPos")
                               fiCRC
                               (Just$ SzData compsize encrypted (i item))

-- |Get info about one file in archive
szArcGetItemFileInfo archive arcname item = do
  fullname  <- szArcGetStrProperty  archive item kpidPath  >>== changeTo [("", takeBaseName arcname)]
  fiSize    <- szArcGetIntProperty  archive item kpidSize
  fiTime    <- szArcGetTimeProperty archive item kpidMTime  `catch`  const (getFileDateTime arcname)   -- Since bz2/gz archive items may have no own datetime, we should use archive time instead
  fiAttr    <- szArcGetIntProperty  archive item kpidAttrib
  fiIsDir   <- szArcGetBoolProperty archive item kpidIsDir
  let root = packParentDirPath ""
      fp   = packFilePath root fullname
  return$ FileInfo fp fp fp fiSize fiTime fiAttr fiIsDir fiUndefinedGroup


----------------------------------------------------------------------------------------------------
---- Распаковка архива -----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Test/extract archive contents
szExtract command arcinfo can_be_extracted = do
  withCWString  (opt_disk_basedir command) $ \c_disk_basedir -> do
  withArrayLen  (map (szIndex.fromJust.cfSzData) (arcDirectory arcinfo)) $ \files c_filelist -> do
  szCallback <- createSzCallback command arcinfo can_be_extracted (error "szExtract::volumes")
  uiStartFiles 0
  szCheckedTABI c_szExtract [ Pair "cmd"         (cmd_name command)
                            , Pair "archive"     (fromJust$ arcSzArchive arcinfo)
                            , Pair "OutputDir"   c_disk_basedir
                            , Pair "callback"    (szCallback :: TABI.FUNCTION)
                            , Pair "filelist"    (c_filelist :: Ptr Word32)
                            , Pair "files"       files
                            , Pair "keepBroken?" (opt_keep_broken command)
                            ]
  return ()


----------------------------------------------------------------------------------------------------
---- Упаковка архива -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Create archive using 7z.dll
szCompress command arcinfo in_arcname out_arcname out_arcnames' diskfiles results = do
  -- Tuple: (files to compress, names to assign to them in the archive, indexes of files from archive to keep, their new names if necesssary)
  let (disk_names, stored_names, remain_file_ixs, remain_file_new_names) =
        if not$ is_CMD_MODIFY$ cmd_name command
          then (map diskName diskfiles, map storedName diskfiles, map (i.szIndex.fromJust.cfSzData) (arcDirectory arcinfo), [])
          else (disk_names, stored_names, ixs, names)
                 where
                   -- Transform list of + / - / * directives into lists of files added, renamed and removed from the archive
                   (disk_names, stored_names, old_names, new_names, removed_names)  =  transform (cmd_filespecs command) [] [] [] [] []
                     where
                       transform (('+':disk_name):stored_name:xs) d s o n r  =  transform xs (disk_name:d) (stored_name:s) o n r
                       transform (('*':old_name):new_name    :xs) d s o n r  =  transform xs d s (old_name:o) (new_name:n) r
                       transform (('-':removed_name)         :xs) d s o n r  =  transform xs d s o n (removed_name:r)
                       transform []                               d s o n r  =  (d, s, o, n, r)

                   -- Indexes and new names of files that should be copied to the output archive
                   (ixs,names)  =  unzip$ concatMap f (arcDirectory arcinfo)
                     where                                                                    -- File from input archive was:
                       f direntry | Just new_name <- renaming old_name  =  [(ix, new_name)]   --   moved/renamed
                                  |           is_stored_name  old_name  =  []                 --   replaced with file from disk
                                  |           is_removed_name old_name  =  []                 --   removed from archive
                                  | otherwise                           =  [(ix, "")]         --   copied intact
                         where old_name = fpFullname$ fiStoredName$ cfFileInfo direntry       -- Name of file inside input archive
                               ix       = szIndex$ fromJust$ cfSzData direntry                -- It's index inside archive
                               renaming         =  htLookup   $ htCreate    (zip old_names new_names)
                               is_stored_name   =  htLookupSet$ htCreateSet stored_names
                               is_removed_name  =  htLookupSet$ htCreateSet removed_names

  ----------------------------------------------------------------------------------------------------
  -- Now filenames are calculated, go to options passing
  ----------------------------------------------------------------------------------------------------
  let solid_str               =  concatMap cvt_solid_option (opt_group_data command)
      solid_option            =  solid_str &&& ["s"++solid_str]
      encrypt_headers_option  =  opt_headers_password command &&& ["he"]
  withCWStringArray (map cvt_method_option (opt_7z_compression command) ++ solid_option ++ encrypt_headers_option) $ \c_options -> do
  withCWStringArray disk_names                   $ \c_disk_names   -> do
  withCWStringArray stored_names                 $ \c_stored_names -> do
  withArrayLen      remain_file_ixs              $ \files c_ixs    -> do
  withCWStringArray remain_file_new_names        $ \c_new_names    -> do
  withArrayLen      (map i$opt_volumes command)  $ \volumes c_volume_sizes -> do
  szCallback <- createSzCallback command (error "szCompress::arcinfo") (error "szCompress::can_be_extracted") out_arcnames'
  szCheckedTABI c_szCompress [ Pair "update_type"   [opt_update_type command]
                             , Pair "options"       (c_options)
                             , Pair "arctype"       (W$ opt_archive_type command)
                             , Pair "in_arcname"    (W in_arcname)
                             , Pair "out_arcname"   (W out_arcname)
                             , Pair "disk_names"    (c_disk_names)
                             , Pair "stored_names"  (c_stored_names)
                             , Pair "files"         (files)
                             , Pair "filelist"      (c_ixs :: Ptr Word32)
                             , Pair "new_names"     (if remain_file_new_names==[]  then nullPtr  else c_new_names)    -- nullPtr means that there are no files renamed
                             , Pair "sfx"           (W$ opt_sfx command)
                             , Pair "volumes"       (volumes)
                             , Pair "volume_sizes"  (c_volume_sizes :: Ptr Word64)
                             , Pair "password"      (W$ opt_data_password command)
                             , Pair "callback"      (szCallback :: TABI.FUNCTION)
                             ]
  -- Напечатаем статистику выполнения команды и сохраним её для возврата в вызывающую процедуру
  uiDoneArchive  >>=  (results=:)


-- |Строка опции -ms для 7z.dll
cvt_solid_option x = case x of
  GroupNone       -> "1f"
  GroupByExt      -> "e"
  GroupBySize   s -> show s++"b"
  GroupByNumber n -> show n++"f"
  _               -> ""

-- |Преобразование опции -m в стиле FreeArc в опции для 7z.dll
cvt_method_option  =  first_step >>> step >>> step >>> step
        where first_step = changeTo (prepareSubsts _7zOptions)
              step       = changeTo (prepareSubsts _7zMethodSubsts)

-- |Перекодирование подопций опции -m FreeArc в их аналоги в 7z.dll
_7zOptions = [
      ""
    , "t   = mt    ; Multithreading option"
    , "t-  = mt-"
    , "t+  = mt+"
    , "t#  = mt#"
    , "t1# = mt1#"
    , "t2# = mt2#"
    , "t3# = mt3#"
    , ""
    ]

-- |Сокращения для методов сжатия в 7z.dll
_7zMethodSubsts = [
      ""
    , "#x = #"
    , ""
    , "0  = x0"
    , "1  = x1"
    , "2  = x3"
    , "3  = x3"
    , "4  = x5"
    , "5  = x7"
    , "#  = x9"
    , ""
    , "1t = ppmd:3:4m"
    , "2t = ppmd:4:24m"
    , "3t = ppmd:6:48m"
    , "#t = ppmd:o=8:mem=96m"
    ]

-- |Разбить цепочку на отдельные методы сжатия
split_7z_compressor  =  split ' '


----------------------------------------------------------------------------------------------------
---- Callback для операций над архивом -------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Создаёт Callback для распаковки архивов через 7z.dll
createSzCallback command arcinfo can_be_extracted volumes = do
  saved_filename <- ref ""
  first_volume   <- ref True
  return $ \p -> do
    --TABI.dump p
    service <- TABI.required p "request"
    case service of
      -- Можно ли извлечь этот файл?
      "can_be_extracted?" -> do
                       let Just archive = arcSzArchive arcinfo
                       W outname <- TABI.required p "outname"
                       index     <- TABI.required p "index"
                       b <- can_be_extracted command (outname::String) (szArcGetItemFileInfo archive (cmd_arcname command) index)
                       return$ iif b 1 0
      -- Запрос пароля расшифровки
      "ask_password" -> do
                       password_buf  <- TABI.required p "password_buf"
                       password_size <- TABI.required p "password_size"
                       password <- generateSzDecryption (opt_decryption_info command)
                       when (length password < password_size) $ do
                         pokeArray0 0 password_buf (map (fromIntegral.ord) password :: [CWchar])
                       return aFREEARC_OK
      -- Начало распаковки нового файла
      "filename" -> do W filename <- TABI.required p "filename"
                       is_folder  <- TABI.required p "is_folder?"
                       mode       <- TABI.required p "mode"
                       uiStartFile (mode==kSkip &&& msgSkipping)
                                   (Left (filename, is_folder))
                       saved_filename =: filename
                       return aFREEARC_OK
      -- Окончание распаковки файла
      "filedone" -> do operationResult <- TABI.required p "operationResult"
                       when (operationResult /= kOK) $ do
                         encrypted     <- TABI.required p "encrypted?"
                         filename      <- val saved_filename
                         let err = case () of
                                     _ | operationResult==kUnSupportedMethod          -> UNSUPPORTED_METHOD
                                       | operationResult==kDataError && not encrypted -> DATA_ERROR
                                       | operationResult==kDataError &&     encrypted -> DATA_ERROR_ENCRYPTED
                                       | operationResult==kCRCError  && not encrypted -> BAD_CRC
                                       | operationResult==kCRCError  &&     encrypted -> BAD_CRC_ENCRYPTED
                                       | otherwise                                    -> UNKNOWN_ERROR
                         registerWarning$ err filename
                       return aFREEARC_OK
      -- Общий обхём упаковываемых данных
      "total"    -> do when (is_CMD_MODIFY$ cmd_name command)$ do
                         files      <- TABI.required p "files"
                         original   <- TABI.required p "original"
                         uiStartProcessing files original 0 0
                       uiStartFiles 1  -- should be called after uiStartProcessing
                       return aFREEARC_OK
      -- Информируем пользователя о ходе распаковки
      "progress" -> do original   <- TABI.required p "original"
                       compressed <- TABI.required p "compressed"
                       uiUnpackedBytes           original
                       uiUpdateProgressIndicator original
                       uiCompressedBytes         compressed
                       guiUpdateProgressIndicator
                       return aFREEARC_OK
      -- Начало нового тома
      "volume"   -> do W filename <- TABI.required p "filename"
                       whenM (val first_volume) $ do
                           first_volume =: False
                           volumes =: []
                       (volumes:: MVar [String]) .= (++[filename])
                       guiStartVolume filename
                       return aFREEARC_OK
      -- Прочие (неподдерживаемые) callbacks
      _          -> do return aFREEARC_ERRCODE_NOT_IMPLEMENTED


----------------------------------------------------------------------------------------------------
---- Интерфейс к сишной части реализации -----------------------------------------------------------
----------------------------------------------------------------------------------------------------

type SzArchive  = Ptr ()
type SzItem     = Int32
type SzProperty = Int32
type SzErrCode  = Int32
type SzErrMsg   = CString

foreign import ccall   safe ""   c_szCheckType           :: CWString -> IO Int
foreign import ccall   safe ""   c_szDefaultExtension    :: CWString -> CWString -> Int32 -> SzErrMsg -> IO SzErrCode
foreign import ccall   safe ""   c_szFindFormatForArchiveName :: CWString -> CWString -> Int32 -> SzErrMsg -> IO SzErrCode
foreign import ccall   safe ""   c_szOpenArchive         :: TABI.C_FUNCTION
foreign import ccall   safe ""   c_szArcClose            :: SzArchive -> SzErrMsg -> IO SzErrCode
foreign import ccall   safe ""   c_szArcItems            :: SzArchive -> Ptr SzItem -> SzErrMsg -> IO SzErrCode
foreign import ccall unsafe ""   c_szArcGetInt64Property :: SzArchive -> SzItem -> SzProperty -> Ptr Int64 -> SzErrMsg -> IO SzErrCode
foreign import ccall unsafe ""   c_szArcGetTimeProperty  :: SzArchive -> SzItem -> SzProperty -> Ptr Int32 -> SzErrMsg -> IO SzErrCode
foreign import ccall unsafe ""   c_szArcGetBoolProperty  :: SzArchive -> SzItem -> SzProperty -> Ptr Int32 -> SzErrMsg -> IO SzErrCode
foreign import ccall unsafe ""   c_szArcGetStrProperty   :: SzArchive -> SzItem -> SzProperty -> CWString -> Int32 -> SzErrMsg -> IO SzErrCode
foreign import ccall   safe ""   c_szExtract             :: TABI.C_FUNCTION
foreign import ccall   safe ""   c_szCompress            :: TABI.C_FUNCTION
foreign import ccall unsafe ""   c_szSetBreakFlag        :: CInt -> IO ()

szArcClose           archive                =  szChecked (c_szArcClose archive)
szArcItems           archive                =  withTemp  (c_szArcItems archive)
szArcGetIntProperty  archive item property  =  withTemp  (c_szArcGetInt64Property archive item property) >>== i
szArcGetTimeProperty archive item property  =  withTemp  (c_szArcGetTimeProperty  archive item property) >>== i
szArcGetBoolProperty archive item property  =  withTemp  (c_szArcGetBoolProperty  archive item property) >>== (/=0)
szArcGetStrProperty  archive item property  =  withTempCWString (c_szArcGetStrProperty archive item property)


----------------------------------------------------------------------------------------------------
---- Helper functions ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

withTemp action = alloca $ \ptr -> do
                    szChecked$ action ptr
                    peek ptr

withTempCWString action = allocaBytes (long_path_size*4) $ \ptr -> do
                             szChecked$ action ptr (i long_path_size)
                             peekCWString ptr

-- |Выполнить action с сериализованным в память массивом строк
withCWStringArray strings action =
  bracket (mapM newCWString strings)
          (mapM_ free)
          (\array -> withArray0 nullPtr array action)


szChecked action = allocaBytes 1000 $ \ptr -> do
                     poke ptr (toEnum 0)
                     result <- action ptr
                     when (result/=0) $ do
                       peekCString ptr >>= throwIO . ErrorCall

szCheckedTABI action p = do
                     setupBreakHandling
                     result <- TABI.call action p
                     when (result/=0) $ do
                       throwIO$ ErrorCall "szCheckedTABI: error"

-- |Настроить обработку принудительного завершения операции, выполняемой 7z.dll
setupBreakHandling = do
  c_szSetBreakFlag 0
  whenM (val breakHandling_need_initialisation) $ do
    breakHandling_need_initialisation =: False
    errorHandlers ++= [\_ -> c_szSetBreakFlag 1]

breakHandling_need_initialisation = unsafePerformIO$ ref True


----------------------------------------------------------------------------------------------------
---- Constants -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |NArchive::NExtract::NAskMode
kExtract:
  kTest:
  kSkip:_ = [0..] :: [Int]

-- |NArchive::NExtract::NOperationResult
kOK:
  kUnSupportedMethod:
  kDataError:
  kCRCError:_ = [0..] :: [Int]

-- |7-zip property IDs
kpidNoProperty:
  kpidMainSubfile:
  kpidHandlerItemIndex:
  kpidPath:
  kpidName:
  kpidExtension:
  kpidIsDir:
  kpidSize:
  kpidPackSize:
  kpidAttrib:
  kpidCTime:
  kpidATime:
  kpidMTime:
  kpidSolid:
  kpidCommented:
  kpidEncrypted:
  kpidSplitBefore:
  kpidSplitAfter:
  kpidDictionarySize:
  kpidCRC:
  kpidType:
  kpidIsAnti:
  kpidMethod:
  kpidHostOS:
  kpidFileSystem:
  kpidUser:
  kpidGroup:
  kpidBlock:
  kpidComment:
  kpidPosition:
  kpidPrefix:
  kpidNumSubDirs:
  kpidNumSubFiles:
  kpidUnpackVer:
  kpidVolume:
  kpidIsVolume:
  kpidOffset:
  kpidLinks:
  kpidNumBlocks:
  kpidNumVolumes:
  kpidTimeType:
  kpidBit64:
  kpidBigEndian:
  kpidCpu:
  kpidPhySize:
  kpidHeadersSize:
  kpidChecksum:
  kpidCharacts:
  kpidVa:
  kpidId:
  kpidShortName:
  kpidCreatorApp:
  kpidSectorSize:
  kpidPosixAttrib:
  kpidLink:_ = [0..]


----------------------------------------------------------------------------------------------------
---- Упаковываемый файл (или с диска, или из уже существующего архива) -----------------------------
----------------------------------------------------------------------------------------------------

-- |File to compress: either file on disk or compressed file in existing archive
data FileToCompress
  = DiskFile
      { cfFileInfo           :: !FileInfo
      }
  | CompressedFile
      { cfFileInfo           :: !FileInfo
      , cfArcBlock           ::  ArchiveBlock   -- Archive datablock which contains file data
      , cfPos                ::  FileSize       -- Starting byte of file data in datablock
      , cfCRC :: {-# UNPACK #-} !CRC            -- File's CRC
      , cfSzData             :: !(Maybe SzData) -- Only for 7z.dll-handled archives
      }

-- |Additional data for files in archives handled by 7z.dll
data SzData = SzData { szCompsize  :: !FileSize
                     , szEncrypted :: !Bool
                     , szIndex     :: !Word32
                     }

-- |Assign type synonym because variant label can't be used in another types declarations
type CompressedFile = FileToCompress


-- |Проверка того, что упаковываемый файл - из уже существующего архива, а не с диска
isCompressedFile CompressedFile{} = True
isCompressedFile DiskFile{}       = False

-- |Проверка что упакованный файл принадлежит архиву, поддерживаемому 7z.dll
isCfSz = isJust.cfSzData

-- |Алгоритм сжатия, использованный для данного (сжатого) файла
cfCompressor cf | isCfSz cf = ["lzma"]
                | otherwise = blCompressor (cfArcBlock cf)

-- |Это сжатый файл, использующий фейковый метод компрессии?
isCompressedFake file  =  isCompressedFile file  &&  isFakeCompressor (cfCompressor file)

-- |Сжатый размер файл, если доступен
cfCompsize = fmap szCompsize . cfSzData

-- |Это запаролированный файл?
cfIsEncrypted cf  =  case cfSzData cf of
                       Just szData  ->  szEncrypted szData              -- данные получены из 7z.dll
                       Nothing      ->  blIsEncrypted (cfArcBlock cf)   -- определяем по методу сжатия солид-блока

-- |Определить тип файла по группе, если она не проставлена - вычислить по имени
cfType command file | group/=fiUndefinedGroup  =  opt_group2type command group
                    | otherwise                =  opt_find_type command fi
                                                    where fi    = cfFileInfo file
                                                          group = fiGroup fi


----------------------------------------------------------------------------------------------------
---- Файл и его CRC - используется для передачи результатов упаковки -------------------------------
----------------------------------------------------------------------------------------------------

-- |File and it's CRC
data FileWithCRC = FileWithCRC { fwCRC  :: {-# UNPACK #-} !CRC
                               , fwType :: {-# UNPACK #-} !FileType
                               , fwFileInfo            :: !FileInfo
                               }

data FileType = FILE_ON_DISK | FILE_IN_ARCHIVE  deriving (Eq)

-- |Проверка того, что упакованный файл - из исходного архива, а не с диска
isFILE_ON_DISK fw  =  fwType fw == FILE_ON_DISK

-- |Convert FileToCompress to FileWithCRC
fileWithCRC (DiskFile       fi)            =  FileWithCRC 0   FILE_ON_DISK    fi
fileWithCRC (CompressedFile fi _ _ crc _)  =  FileWithCRC crc FILE_IN_ARCHIVE fi
