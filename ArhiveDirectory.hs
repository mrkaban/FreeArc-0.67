{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Работа с оглавлением архива.                                                             ------
---- Этот модуль содержит процедуры для:                                                      ------
----   * чтения структуры входного архива (т.е. каталогов и других служебных блоков)          ------
----   * записи и чтения каталогов архива                                                     ------
----------------------------------------------------------------------------------------------------
module ArhiveDirectory where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Monad
import Control.OldException
import Data.HashTable as Hash
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Mem

import GHC.PArr

import TABI
import Utils
import Errors
import Files
import qualified ByteStream
import FileInfo
import CompressionLib
import Compression      (CRC, isFakeCompressor)
import UI
import Options
import ArhiveStructure
import Arhive7zLib


{-# NOINLINE archiveReadInfo #-}
-- |Прочитать каталог архива FreeArc/7z.dll
archiveReadInfo command               -- выполняемая команда со всеми её опциями
                arc_basedir           -- базовый каталог внутри архива ("" для команд добавления)
                disk_basedir          -- базовый каталог на диске ("" для команд добавления/листинга)
                filter_f              -- предикат для фильтрации списка файлов в архиве
                processFooterInfo     -- процедура, выполняемая на данных из FOOTER_BLOCK
                arcname = do          -- имя файла, содержащего архив

  (archive,footer) <- arcOpen command arcname
  case archive of
    Left  sz -> szReadInfo        sz footer filter_f processFooterInfo arcname
    Right my -> myArchiveReadInfo my footer command arc_basedir disk_basedir filter_f processFooterInfo


{-# NOINLINE arcOpen #-}
-- |Открыть архив FreeArc/7z.dll
arcOpen command arcname = do
  savedErr <- ref Nothing
  savedErrcodeHandler <- val errcodeHandler
  errcodeHandler =: (\err -> do savedErr =: Just err; fail "")
  szArc <- try$ szOpenArchive (Left command) arcname   -- попробуем открыть архив через 7z.dll
  myArc <- try$ myOpenArchive command arcname          -- ... а теперь как архив FreeArc
  errcodeHandler =: savedErrcodeHandler
  err <- val savedErr
  case (szArc,myArc,err) of                                -- а теперь выберем из них тот, что начинается раньше (поскольку внутри одного архива может быть другой и притом без сжатия)
       (Left _,               Left _,       Just err)  ->  registerError err
       (Left _,               Left my,             _)  ->  throwIO my
       (Right (sz,szFooter),  Left _,              _)  ->  return (Left  sz, szFooter)
       (Left _,               Right (my,myFooter), _)  ->  return (Right my, myFooter)
       (Right (sz,szFooter),  Right (my,myFooter), _)  ->  if ftSFXSize szFooter < ftSFXSize myFooter  &&  False   -- fix01: на данный момент FreeArc распознаёт свои архивы только если сигнатура архива находится в самом конце файла
                                                             then do archiveClose my; return (Left sz, szFooter)
                                                             else do szArcClose   sz; return (Right my, myFooter)


-- |Закрытие архива FreeArc/7z.dll
arcOpenClose (Left  sz) = szArcClose sz
arcOpenClose (Right my) = archiveClose my



----------------------------------------------------------------------------------------------------
---- Открытие архива FreeArc -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE myOpenArchive #-}
-- |Открыть архив FreeArc
myOpenArchive command arcname = do
  if opt_broken_archive command /= "-"
     then findBlocksInBrokenArchive arcname
     else archiveReadFooter command arcname


{-# NOINLINE myArchiveReadInfo #-}
-- |Прочитать каталог архива FreeArc
myArchiveReadInfo archive footer command arc_basedir disk_basedir filter_f processFooterInfo = do
  -- Выполним на FOOTER_BLOCK переданную процедуру
  processFooterInfo (Just archive) footer

  -- Прочитаем содержимое блоков каталога, описанных в FOOTER_BLOCK
  let dir_blocks  =  filter ((DIR_BLOCK==).blType) (ftBlocks footer)
  files  <-  foreach dir_blocks $ \block -> do
    withPool $ \pool -> do
      (buf,size) <- archiveBlockReadAll pool (opt_decryption_info command) block
      archiveReadDir arc_basedir disk_basedir (opt_dir_exclude_path command) archive (blPos block) filter_f (return (buf,size))

  let data_blocks = concatMap fst files
      directory   = concatMap snd files

  -- Добавим в arcinfo информацию о списке файлов в архиве
  return ArchiveInfo { arcArchive     = Just archive
                     , arcFooter      = footer
                     , arcDirectory   = directory
                     , arcDataBlocks  = data_blocks
                     , arcDirBytes    = sum (map blOrigSize dir_blocks)
                     , arcDirCBytes   = sum (map blCompSize dir_blocks)
                     , arcDataBytes   = sum (map blOrigSize data_blocks)
                     , arcDataCBytes  = sum (map blCompSize data_blocks)
                     , arcSzArchive   = Nothing
                     , arcArchiveType = aFreeArc
                     }



----------------------------------------------------------------------------------------------------
---- Запись блока каталога -------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE archiveWriteDir #-}
-- |Закодировать `dirdata` и послать полученные данные на дальнейшую обработку с помощью `sendBuf`
archiveWriteDir dirdata     -- список пар (block :: ArchiveBlock, directory :: [FileWithCRC])
                arcpos      -- позиция в архиве, где начинается этот каталог
                (receiveBuf -- "(buf,size) <- receiveBuf" получает для работы очередной буфер размером `size`
                ,sendBuf)   -- "sendBuf buf size len" посылает сформированные в буфере данные на выход
                nodates     -- не записывать в архив даты модификации файлов?
                = do
#ifndef FREEARC_DLL
  debugLog "\n  Writing directory"
  let blocks      = map fst dirdata            :: [ArchiveBlock]  -- список солид-блоков, попавших в данный каталог
      crcfilelist = concatMap snd dirdata      :: [FileWithCRC]   -- объединённый список файлов - в том порядке, в каком они расположены в блоках!
      filelist    = map fwFileInfo crcfilelist :: [FileInfo]      -- информация о самих файлах

  -- 0. Cоздадим выходной буфер, использующий для общения с внешним миром функции `receiveBuf` и `sendBuf`
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  let write         :: (ByteStream.BufferData a) =>  a -> IO ()   -- shortcuts для функций записи в буфер
      write          =  ByteStream.write          stream
      writeLength    =  ByteStream.writeInteger   stream . length
      writeList     :: (ByteStream.BufferData a) =>  [a] -> IO ()
      writeList      =  ByteStream.writeList      stream
      writeIntegers  =  mapM_ (ByteStream.writeInteger stream)
      writeTagged     tag x   =  write tag >> write x     -- запись с тегами - для опциональных полей
      writeTaggedList tag xs  =  write tag >> writeList xs

  -- 1. Закодируем описания блоков архива и кол-во файлов в каждом из них
  writeLength dirdata               -- кол-во блоков.                  Для каждого блока записывается:
  mapM_ (writeLength.snd) dirdata                                      -- кол-во файлов
  writeList$ map (map purifyCompressionMethod . blCompressor) blocks   -- метод сжатия
  writeList$ map (blEncodePosRelativeTo arcpos)               blocks   -- относительная позиция блока в файле архива
  writeList$ map (blCompSize                  )               blocks   -- размер блока в упакованном виде

  -- 2. Запишем в архив список имён каталогов
    -- Получим список имён каталогов и номера каталогов, соответствующие файлам в filelist
  (n, dirnames, dir_numbers)  <-  enumDirectories filelist
  debugLog$ "  Found "++show n++" directory names"
  writeLength dirnames  -- временно, для обхода проблемы с Compressor==[String]
  writeList   (map unixifyPath dirnames)

  -- 3. Закодируем отдельно каждое оставшееся поле в CompressedFile/FileInfo
  -- to do: добавить RLE-кодирование полей?
  let fiTimeInternal  =  (if nodates  then const aMINIMUM_POSSIBLE_FILETIME  else fiTime)
  writeList$ map (fpBasename.fiStoredName)  filelist     -- имена файлов
  writeIntegers                             dir_numbers  -- номера каталогов
  writeList$ map fiSize                     filelist     -- размеры файлов
  writeList$ map fiTimeInternal             filelist     -- времена модификации
  writeList$ map fiIsDir                    filelist     -- признаки каталога
  -- cfArcBlock и cfPos кодируются неявно, путём сортировки по этим двум полям
  writeList$ map fwCRC                      crcfilelist  -- CRC

  -- 4. Опциональные поля, префиксируются своими тегами, в конце - тег окончания опциональных полей
  write aTAG_END  -- пока опциональных полей нет, нам остаётся только сразу записать тег их окончания

  -- 5. Вотысё! :)
  ByteStream.closeOut stream
  -- Это приводит к вылету Arc.exe!!! - всё ещё?
  when (length filelist >= 10000) performGC  -- Соберём мусор, если блок содержит достаточно много файлов
  debugLog "  Directory written"
#endif
  return ()


-- Создание по списку файлов - списка уникальных каталогов + номер каталога для каждого файла в списке
enumDirectories filelist = do
  -- Для каждого Stored имени файла мы ищем имя с тем же каталогом в хеш-таблице `table`.
  -- Если оно найдено, то мы получаем из хеш-таблицы номер этого каталога,
  -- а если нет - вставляем это имя в хеш-таблицу с очередным порядковым номером, которые
  -- генерятся через переменную n, и добавляем имя каталога в список `dirnames`.
  -- Таким образом, хеш-таблица `table` отображает имена каталогов в их номера
  -- в создаваемом списке всех каталогов `dirnames`.
  table <- Hash.new (==) fpHash                     -- отображает каталоги в их номера

  -- Возвратить для списка файлов количество уникальных имён каталогов, их полный список,
  -- и номер каталога для каждого файла (например, [0,1,0,0,2] для a\1 b\1 a\2 a\3 c\1)
  let go []              dirnames dir_numbers n = return (n, reverse dirnames, reverse dir_numbers)
      go (fileinfo:rest) dirnames dir_numbers n = do
        let storedName  =  fiStoredName fileinfo    -- имя, предназначенное для сохранения в архиве
            dirname     =  fpParent storedName      -- каталог, к которому принадлежит файл
        x <- Hash.lookup table dirname              -- есть ли уже в хеше этот каталог?
        case x of                                   -- Если нет, то
          Nothing -> do Hash.insert table dirname n -- занести в хеш номер каталога
                        -- Добавить имя каталога в список имён каталогов,
                        -- номер каталога в список номеров каталога для каждого файла,
                        -- и инкрементировать счётчик каталогов
                        go rest (fpDirectory storedName:dirnames) (n:dir_numbers) $! n+1
          Just x  -> do go rest dirnames (x:dir_numbers) n
  --
  go filelist [] [] (0::FileCount)


----------------------------------------------------------------------------------------------------
---- Чтение блока каталога -------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE archiveReadDir #-}
-- |Прочитать каталог, записанный функцией `archiveWriteDir`
archiveReadDir arc_basedir   -- базовый каталог в архиве
               disk_basedir  -- базовый каталог на диске
               ep            -- исключить каталоги из имён/разрешить абсолютные пути
               archive       -- файл архива
               arcpos        -- позиция в архиве, где начинается этот каталог
               filter_f      -- предикат фильтрации файлов
               receiveBuf    -- "(buf,size) <- receiveBuf" получает для работы очередной буфер размером `size`
               = do
  debugLog "  Decoding directory"

  -- 0. Cоздадим входной буфер, использующий для общения с внешним миром функцию `receiveBuf`
  stream <- ByteStream.open receiveBuf (\a b c->return ()) (return ())
  let read         :: (ByteStream.BufferData a) =>  IO a   -- shortcuts для функций чтения из буфера
      read           = ByteStream.read stream
      readList     :: (ByteStream.BufferData a) =>  Int -> IO [a]
      readList       = ByteStream.readList stream
      readInteger    = ByteStream.readInteger stream
      readLength     = readInteger
      readIntegers n = replicateM n readInteger

  -- 1. Прочитаем описания блоков архива
  num_of_blocks <- readLength                     -- кол-во блоков
  -- Для каждого блока прочитаем:
  num_of_files  <- readIntegers num_of_blocks     -- кол-во файлов
  blCompressors <- readList     num_of_blocks     -- метод сжатия
  blOffsets     <- readList     num_of_blocks     -- относительную позицию блока в файле архива
  blCompSizes   <- readList     num_of_blocks     -- размер блока в упакованном виде

  -- 2. Прочитаем имена каталогов
  total_dirs    <-  readLength                    -- Сколько всего имён каталогов сохранено в этом оглавлении архива
  storedName    <-  readList total_dirs >>== map (remove_unsafe_dirs>>>make_OS_native_path) >>== toP -- Массив имён каталогов

  -- 3. Прочитаем списки данных для каждого поля в CompressedFile/FileInfo
  let total_files = sum num_of_files              -- суммарное кол-во файлов в каталоге
  names         <- readList     total_files       -- Имена файлов (без имени каталога)
  dir_numbers   <- readIntegers total_files       -- Номер каталога для каждого из файлов
  sizes         <- readList     total_files       -- Размеры файлов
  times         <- readList     total_files       -- Время модификации файлов
  dir_flags     <- readList     total_files       -- Булевские флаги "это каталог?"
  crcs          <- readList     total_files       -- CRC файлов

  -- 4. Дополнительные поля, префиксируются своими тегами, в конце - тег окончания дополнительных полей
{-repeat_while (read) (/=aTAG_END) $ \tag -> do
    (isMandatory::Bool) <- read
    when isMandatory $ do
      registerError$ GENERAL_ERROR ("can't skip mandatory field TAG="++show tag++" in archive directory")
    readInteger >>= ByteStream.skipBytes stream   -- пропустить данные этого поля
    return ()
-}
  -- 5. Вотысё! :)
  ByteStream.closeIn stream
  debugLog "  Directory decoded"

  ------------------------------------------------------------------------------------------------
  -- Теперь построим каталог по прочитанным данным -----------------------------------------------
  ------------------------------------------------------------------------------------------------
  -- Массивы, содержащие информацию о каталогах
  let drop_arc_basedir  = if arc_basedir>""  then drop (length arc_basedir + 1)  else id
      make_disk_name    = case ep of         -- Превращает имя в архиве в имя на диске
                            0 -> const ""    --   команда "e"  -> использовать только базовое имя
                            3 -> id          --   опция -ep3   -> использовать полное имя
                            _ -> stripRoot   --   по умолчанию -> обрезать "d:\" часть
      -- Массивы, отображающие номер каталога в его Filtered/Disk name (массив для Stored name построен сразу при чтении)
      filteredName      = fmap (drop_arc_basedir)                    storedName
      diskName          = fmap ((disk_basedir </>) . make_disk_name) filteredName
      -- Массивы, отображающие номер каталога в структуру PackedFilePath
      storedInfo        = fmap packParentDirPath storedName
      filteredInfo      = fmap packParentDirPath filteredName
      diskInfo          = fmap packParentDirPath diskName
      -- Для каждого каталога - булевский флаг: начинается ли его имя с базового каталога ("-ap")
      dirIncludedArray  = fmap (arc_basedir `isParentDirOf`) storedName
      dirIncluded       = if arc_basedir==""  then const True  else (dirIncludedArray!:)

  -- Список структур Maybe FileInfo (Nothing для тех файлов, которые не принадлежат
  -- базовому каталогу ("-ap") или не проходят через предикат фильтрации файлов)
  let make_fi dir name size time dir_flag =
        if dirIncluded dir && filter_f fileinfo  then Just fileinfo  else Nothing

        where fileinfo = FileInfo { fiFilteredName  =  fiFilteredName
                                  , fiDiskName      =  fiDiskName
                                  , fiStoredName    =  fiStoredName
                                  , fiSize          =  size
                                  , fiTime          =  time
                                  , fiAttr          =  0
                                  , fiIsDir         =  dir_flag
                                  , fiGroup         =  fiUndefinedGroup
                                  }
              fiStoredName    =                                   packFilePathPacked2 stored   (fpPackedFullname stored)   name
              fiFilteredName  =  if arc_basedir>""           then packFilePathPacked2 filtered (fpPackedFullname filtered) name  else fiStoredName
              fiDiskName      =  if disk_basedir>"" || ep/=3 then packFilePathPacked2 disk     (fpPackedFullname disk)     name  else fiFilteredName
              stored   = storedInfo  !:dir
              filtered = filteredInfo!:dir
              disk     = diskInfo    !:dir

  -- Составим структуры FileInfo из отдельных полей, прочитанных из архива
  let fileinfos = zipWith5 make_fi dir_numbers names sizes times dir_flags

  -- Реконструируем дескрипторы блоков данных.
  -- Сначала разобьём список длин файлов на подсписки, относящиеся к отдельным блокам.
  -- Это позволит нам вычислить суммарный объём файлов в каждом из блоков
  let filesizes = splitByLens num_of_files sizes
  let blocks    = map (tupleToDataBlock archive arcpos) $
                    zip5 blCompressors
                         blOffsets
                         (map sum filesizes)
                         blCompSizes
                         num_of_files

  -- Размножим ссылки на дескрипторы блоков данных, чтобы хватило на все файлы :)
  let arcblocks = concat [ replicate files_in_block blockDescriptor
                           | files_in_block  <- num_of_files  -- кол-во файлов в очередном блоке данных
                           | blockDescriptor <- blocks        -- дескриптор очередного блока
                         ]

  -- Позиция файла в блоке равна суммарной длине предыдущих файлов в этом блоке.
  -- filesizes - список списков длин файлов, относящихся к каждому блоку.
  -- Для того, чтобы получить из него позицию файла внутри блока, мы просто считаем
  -- "сканирующую сумму". Добавляем [0] в начало каждого списка позиций,
  -- чтобы получить позиции ПЕРЕД файлами, а не после них :)
  -- Одним словом, если  num_of_files = [1..4]
  --                  и  sizes = [1..10]
  --               то  filesizes = [[1],[2,3],[4,5,6],[7,8, 9,10]]
  --                и  positions = [ 0,  0,2,  0,4,9,  0,7,15,24]
  let positions = concatMap scanningSum filesizes
      scanningSum [] = []
      scanningSum xs = 0 : scanl1 (+) (init xs)

  -- Теперь у нас готовы все компоненты для создания списка файлов, содержащихся в этом каталоге
  let files = [ CompressedFile fileinfo arcblock pos crc Nothing
              | (Just fileinfo, arcblock, pos, crc)  <-  zip4 fileinfos arcblocks positions crcs
              ]

  return $! evalList files               -- Переведём созданный список файлов в вычисленное состояние
  when (total_files >= 10000) performGC  -- Соберём мусор, если блок содержит достаточно много файлов
  debugLog "  Directory built"

  return (blocks, files)

--  let f CompressedFile{cfFileInfo=FileInfo{fiFilteredName=PackedFilePath{fpParent=PackedFilePath{fpParent=RootDir}}}} = True
--      f _ = False


