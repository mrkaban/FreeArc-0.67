{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- ���������� ������ ���������� � ��������� �������� ������                                   ----
----------------------------------------------------------------------------------------------------
module ArcExtract ( runArchiveExtract
                  , runArchiveList
                  , runCommentWrite
                  , formatDateTime
                  ) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.QSemN
import Control.OldException
import Control.Monad
import Data.List
import Data.Maybe
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Numeric
import System.IO.Unsafe
import System.Posix.Internals (s_isdir)

import TABI
import Process
import Utils
import Files
import FileInfo
import Charsets            (i18n)
import Errors
import CompressionLib
import Compression         (aINIT_CRC, updateCRC, finishCRC, join_compressor)
import Options
import UI
import ArhiveStructure
import Arhive7zLib
import ArhiveDirectory
import ArcvProcessExtract
import ArcvProcessRead

-- |���������� ������� ���������� ������
runArchiveExtract pretestArchive
                  command@Command{ cmd_arcname         = arcname
                                 , cmd_archive_filter  = archive_filter
                                 , opt_arc_basedir     = arc_basedir
                                 , opt_disk_basedir    = disk_basedir
                                 , opt_arccmt_file     = arccmt_file
                                 , opt_unParseFile     = unParseFile
                                 } = do
  -- ������������� ������: find_archives -> buffer 10_000 -> read_dir -> buffer 10_000 -> arcExtract
  doFinally uiDoneArchive2 $ do
  uiStartArchive command []  -- �������� ������������ � ������ ��������� ���������� ������
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  uiStage "0249 Reading archive directory"
  let openArchive = archiveReadInfo command arc_basedir disk_basedir archive_filter (pretestArchive command)
  bracketCtrlBreak "arcClose:ArcExtract" (openArchive arcname) (arcClose)$ \archive -> do
    uiPrintArcComment (arcComment archive)            -- ���������� �����������
    when (arccmt_file/="-" && arccmt_file/="--") $    -- � �������� ��� � ����, ��������� ������ -z
      unParseFile 'c' arccmt_file (arcComment archive)
    arcExtract command archive
  uiDoneArchive  -- ���������� � ������� � ���������� ��������� ���������� ���������� �������

-- |���������� ������
arcExtract command arcinfo = do
  let filelist = arcDirectory arcinfo
  -- ���������� � UI ����� ����� ��������������� ������ � ����� ��� �������������� �������� ������
  uiStartProcessing (length filelist)  (sum$ map (fiSize.cfFileInfo) filelist)  (arcDataBytes arcinfo)  (arcDataCBytes arcinfo)
  uiStartDirectory
  uiUnpackedBytes   (arcDirBytes  arcinfo)
  uiCompressedBytes (arcDirCBytes arcinfo)
  guiUpdateProgressIndicator
  uiStartFiles 0
  if isSzArchive arcinfo  then szExtract command arcinfo can_be_extracted else do
  withPool $ \pool -> do
  -- �������� ����� ��� ���������� ������ � ������ ������������� ������
  bracketedRunAsyncP (decompress_PROCESS command (uiCompressedBytes.i)) Nothing $ \decompress_pipe -> do
  bracketedRunAsyncP (write_extracted_files_PROCESS command)            TheEnd  $ \writer_pipe -> do
  -- ���������, ������������ ��� ��������� ������� �����
  process_file <- case (cmd_name command) of
                    "t"  -> return$ test_file decompress_pipe
                    _    -> cached_extract_file command pool writer_pipe decompress_pipe
  -- ����������� �����, ������� ����� �����������, � ���������� �� �����������������
  let (filesToSkip,   allToExtract)    =  partition isCompressedFake    (arcDirectory arcinfo)
      (dirsToExtract, filesToExtract)  =  partition (fiIsDir.cfFileInfo) allToExtract
  for (filesToExtract++dirsToExtract) process_file    -- �������� � ����� ����� ���������� �� ���� �����������, ����������� � ������
  unless (null filesToSkip)$  do registerWarning$ SKIPPED_FAKE_FILES (length filesToSkip)


----------------------------------------------------------------------------------------------------
---- ���������� ������ � ������������� ������������ ������ � ��������� ��������.                ----
---- �� ������� �������� �� ������ ������������ ������������������ ���������                    ----
----   (FileStart DataChunk* DataEnd FileCrc)* TheEnd                                           ----
---- �������� ��������������� "�� �����".                                                       ----
----------------------------------------------------------------------------------------------------

-- |������������ ���������� ������ ����� �� ������
cached_extract_file command pool writer_pipe decompress_pipe = do
  case (opt_decompression_cache command) of
    0 -> do -- ����������� ��������� - ��������� ������ ���� �����
            return$ \compressed_file -> do
             let fileinfo = cfFileInfo compressed_file
             -- ��������� ��� �������, ��� ���� ���� ��������� �����������
             whenM (extract_file_allowed command fileinfo)$ do
               extract_file command fileinfo (decompress_file decompress_pipe compressed_file)

    _ -> do -- �������� ��� ��� ����������� ������
            bufOps <- makeFileCache (opt_decompression_cache command) pool writer_pipe
            -- ��������� �������, ��������������� ���� ���� �� ������ � ���������� ��� ������ ����� ���������� � writer_pipe
            return $ \compressed_file -> do
              let fileinfo  =  cfFileInfo compressed_file
              -- ��������� ��� �������, ��� ���� ���� ��������� �����������
              whenM (extract_file_allowed command fileinfo)$ do
                (Just (FileWithCRC crc _ _)) <- read_file command writer_pipe bufOps decompress_pipe compressed_file
                sendP writer_pipe DataEnd
                sendP writer_pipe (FileCrc crc)


-- |���� ������ ������������� ������ � �����.
-- todo: �������� ��� ��������� TheEnd ������ extract_file (��� ���������� ��������� ���������� � bracketedRunAsyncP)
write_extracted_files_PROCESS command writer_pipe = do
  -- �����, ����������� ������� �� ������ ������.
  -- ������ ������� - ��� pipe, �� �������� ����� ��� ����������, ����������� � ����������� �����.
  -- ���� �� ������� ������� ������� �� ������� � ������������ ��� ������ � ���� ����.
  -- todo: ���������� ���������� ����� ���������� ����� ��������
  jobs_chan <- newChan
  -- �����, ����������� ������� ���������� �������. ��������� ��� �������� ������� ����� � ���������� �������
  jobs_order_chan <- newChan
  -- ������� ��� �������� �������� ���� �������� ������
  all_files <- newQSemN 0

  -- ����� �� ������ �����, ������� ����� ��������� ������� �� ������, ��������� � ������ �� ��� ���������� �� ������ �����
  forkOS $ do
    repeat_while (receiveP writer_pipe) notTheEnd $ \(FileStart fi) -> do
      file_pipe <- newPipe
      writeChan jobs_chan file_pipe
      writeChan jobs_order_chan (Just file_pipe)
      do sendP file_pipe (FileStart fi)
         repeat_while (receiveP writer_pipe) notDataEnd (sendP file_pipe)
         sendP file_pipe DataEnd
         sendP file_pipe =<< receiveP writer_pipe  -- FileCrc crc
    writeChan jobs_order_chan Nothing

  -- ����� �� ������ ������� ������, ������� ����� ������� ������� �� jobs_chan � ������������ ����������� ������ � ����
  cthreads <- getCompressionThreads
  for [1..cthreads] $ \_ -> do
    forkOS $ do
      forever $ do
        file_pipe <- readChan jobs_chan
        (FileStart fi) <- receiveP file_pipe
        extract_file command fi $ \writer -> do
          repeat_while (receiveP file_pipe) notDataEnd (\cmd@(DataChunk buf len) -> do writer buf len; send_backP file_pipe cmd)
          send_backP file_pipe DataEnd
          (FileCrc crc)  <-  receiveP file_pipe
          return crc
        signalQSemN all_files 1

  -- � ����� �� ���������� ���������� ������ �������������� ������ ������ � ��� �������, � ������� �� ��������
  files_count <- ref 0
  repeat_while (readChan jobs_order_chan) isJust $ \(Just file_pipe) -> do
    repeat_while (receive_backP file_pipe) notDataEnd (\(DataChunk buf len) -> send_backP writer_pipe (buf,len))
    files_count += 1

  -- � ���������� �������� ���� �������� ������
  waitQSemN all_files =<< val files_count

  return ()


----------------------------------------------------------------------------------------------------
---- ������ ������������� ������ � �����                                                        ----
----------------------------------------------------------------------------------------------------

-- |������������ ������ ����� �� ������
test_file decompress_pipe compressed_file = do
  let fileinfo = cfFileInfo compressed_file
  uiStartFile "" (Right fileinfo)
  run_decompress fileinfo (decompress_file decompress_pipe compressed_file) (\buf size -> return ())
  return ()

-- |���������� ������ �����: ���������� � �������� ����������
extract_file_allowed command fileinfo = do
  allowed <- arc_can_be_extracted command fileinfo
  if not allowed
    then return False else do
  uiStartFile "" (Right fileinfo)
  if (fiIsDir fileinfo)
    then do -- ���������� ��������
            let filename  =  fpFullname (fiDiskName fileinfo)
            createDirectoryHierarchy filename
            setFileDateTimeAttr filename fileinfo
            return False
    else do -- ���������� �������� �����
            return True

-- |���������� ������ �����: �������� �������
extract_file command fileinfo decompress_file = do
  let filename  =  fpFullname (fiDiskName fileinfo)
  buildPathTo filename
  outfile  <- fileCreate filename
  let closeOutfile ok = do   -- ���������, ����������� ����� ���������� ����� ��� ��� ������ �� ^Break
        fileClose outfile                                 -- to do: ���� ������������ fileSetSize, �� �������� ������ ����� � ������������ � ����������� ������� ������������� ����
        if ok || opt_keep_broken command
          then do setFileDateTimeAttr filename fileinfo   -- ����������� ������� ��� ����� ��������� ���� �����, ������������� � ��������
                  when (opt_clear_archive_bit command) $ do
                      clearArchiveBit filename            -- ����� -ac - �������� ������� Archive ����� ����������
          else fileRemove filename                        -- ������� ����, ������������� � ��������
  do  --fileSetSize outfile (fiSize fileinfo)             -- ��������� �� ��� ���� ������� �� ����� ����� ��� ����� ����� ������
      handleCtrlBreak ("closeOutfile "++filename) (closeOutfile False) $ do
        ok <- run_decompress fileinfo decompress_file (fileWriteBuf outfile)
        closeOutfile ok

{-# NOINLINE run_decompress #-}
-- |���������� ����� �� ������ � ��������� CRC
run_decompress fileinfo decompress_file write_data = do
  crc <- ref aINIT_CRC                        -- �������������� �������� CRC
  let writer buf len = do
        uiUnpackedBytes  (i len)              -- ����������� ������������ � ���� ����������
        uiUpdateProgressIndicator len         -- -.-
        guiUpdateProgressIndicator
        crc          .<- updateCRC buf len    -- ������� CRC ���������� ������
        write_data       buf len              -- ������� ������ � ����
  correct_crc <- decompress_file writer
  acrc  <-  val crc >>== finishCRC            -- �������� ������������� �������� CRC
  let ok = acrc==correct_crc                  -- True, ���� CRC ���������
  unless ok $ do
    registerWarning$ BAD_CRC (fpFullname$ fiStoredName fileinfo)
  return ok


-- |��� ������� ���������� - ����� �� ������� ���� �� ������?
-- ����� ������� �� 1) �������������� ����� (-u/-f/-sync)
--                  2) ������� �� ����� ����������� �����
--                  3) ����, ����� �� ������ ������ - �� ����� ��� � ������
--                  4) �������� ����� "-o" � "y"
--                  5) ������ ������������ �� ������ � ���������� �����
--
can_be_extracted cmd filename return_arcfile = do
  diskfile_exist <- fileExist filename
  if not diskfile_exist                         -- ���� ���� �� ����� �� ����������
    then return (opt_update_type cmd /= 'f')    -- �� ������� ���� �� ������ ������� �� ���� �������, ����� '-f'
    else do
  arcfile <- return_arcfile
  fileWithStatus "getFileInfo" filename $ \p_stat -> do
  diskFileTime  <-  stat_mtime p_stat
  let arcfile_newer  =  fiTime arcfile > diskFileTime   -- ���� � ������ ������, ��� �� �����?
  let overwrite = case (opt_update_type cmd) of
                    'f' -> arcfile_newer
                    'u' -> arcfile_newer
                    's' -> error "--sync can't be used on extract"
                    'a' -> True
  if not overwrite  then return False  else do
  diskFileIsDir  <-  stat_mode  p_stat  >>==  s_isdir
  diskFileSize   <-  if diskFileIsDir then return 0
                                      else stat_size p_stat
  askOverwrite filename diskFileSize diskFileTime arcfile (opt_overwrite cmd) arcfile_newer

-- |������������������� ������� ��� ������� .arc
arc_can_be_extracted command fileinfo = do
  let filename  =  fpFullname (fiDiskName fileinfo)
  if fiIsDir fileinfo
    then return True
    else can_be_extracted command filename (return fileinfo)


----------------------------------------------------------------------------------------------------
---- ������ ����������� � ������ � ���� (������� "cw")                                          ----
----------------------------------------------------------------------------------------------------

-- |���������� ������� "cw" - ������ ����������� � ������ � ����
runCommentWrite command@Command{ cmd_filespecs   = filespecs
                               , cmd_arcname     = arcname
                               , opt_unParseFile = unParseFile
                               } = do
  doFinally uiDoneArchive2 $ do
  when (length filespecs /= 1) $
    registerError$ CMDLINE_SYNTAX "cw archive outfile"
  let [outfile] = filespecs
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  printLineLn$ "Writing archive comment of "++arcname++" to "++outfile
  bracket (arcOpen command arcname) (arcOpenClose.fst) $ \(_,footer) -> do
    unParseFile 'c' outfile (ftComment footer)
  return (0,0,0,0)


----------------------------------------------------------------------------------------------------
---- ������ �������� ������:                                                                    ----
----    - ��� ������������ (������� "l")                                                        ----
----    - ��� ������ (������� "lt")                                                             ----
----    - ��� �������� ����-������ (������� "lb")                                               ----
----    - ��� ������ �������� (������� "v")                                                     ----
---------------------------------------------------------------------------------------------------

-- |���������� ������� ��������� �������� ������
runArchiveList pretestArchive
               command@Command{ cmd_arclist        = arclist
                              , cmd_arcname        = arcname
                              , opt_arc_basedir    = arc_basedir
                              , cmd_archive_filter = archive_filter
                              } = do
  uiStartArchive command []  -- �������� ������������ � ������ ��������� ���������� ������
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  bracket (archiveReadInfo command arc_basedir "" archive_filter (pretestArchive command) arcname) (arcClose) $
      archiveList command (null$ tail arclist)

-- |������� ������
archiveList command @ Command{ cmd_name=cmd, cmd_arcname=arcname}
            show_empty
            arc @ ArchiveInfo{ arcDirectory=directory, arcArchiveType=arctype} = do
  let files = length directory
      bytes = sum$ map (fiSize.cfFileInfo) directory
#ifdef FREEARC_DLL
  gui_callback "archive" [ Pair "arcname"   (W arcname)
                         , Pair "arctype"   arctype
                         , Pair "files"     files
                         ]

  (`myMapM` directory) $ \direntry compsize -> do
    let fi = cfFileInfo direntry
    gui_callback "item" [ Pair "filename"       (W$ storedName fi)
                        , Pair "original"       (fiSize fi)
                        , Pair "compressed"     (compsize)
                        , Pair "time"           (fromEnum$ fiTime fi)
                        , Pair "attr"           (fiAttr fi)
                        , Pair "is_folder?"     (fiIsDir fi)
                        , Pair "crc"            (cfCRC direntry)
                        , Pair "is_encrypted?"  (cfIsEncrypted direntry)
                        ]
#else
  when (files>0 || show_empty) $ do
    doFinally uiDoneArchive2 $ do
    let list line1 line2 list_func linelast = do
                uiPrintArcComment (arcComment arc)
                myPutStrLn line1
                myPutStrLn line2
                compsize <- list_func
                myPutStrLn linelast
                myPutStr$   show3 files ++ " files, " ++ show3 bytes ++ " bytes, " ++ show3 compsize ++ " compressed"
    case cmd of
      "l" -> list "Date/time                  Size Filename"
                  "----------------------------------------"
                  (myMapM terse_list directory)
                  "----------------------------------------"

      "v" -> list "Date/time              Attr            Size          Packed      CRC Filename"
                  "-----------------------------------------------------------------------------"
                  (myMapM verbose_list directory)
                  "-----------------------------------------------------------------------------"

      "lb"-> myPutStr$ joinWith "\n"$ map filename directory

      "lt"-> techinfoHeader arc >>
             list "              Pos            Size      Compressed   Files Method"
                  "-----------------------------------------------------------------------------"
                  (do mapM_ data_block_list (arcDataBlocks arc)
                      return (sum$ map blCompSize (arcDataBlocks arc)))
                  "-----------------------------------------------------------------------------"
#endif
  return (1, files, bytes, -1)


-- |��� �����
filename = fpFullname . fiStoredName . cfFileInfo

-- |��������� � �������� �������� ���������� � ������ �������� �����-������
myMapM f = go 0 True undefined
 where
  go total first lastSolidBlock [] = return total
  go total first lastSolidBlock (file:rest) = do
    let solidBlock = cfArcBlock file
    let compsize = case cfCompsize file of
                     Just compsize -> compsize
                     Nothing       -> if first  ||  solidBlock /= lastSolidBlock
                                        then blCompSize solidBlock
                                        else 0
    f file compsize
    (go $! total+compsize) False solidBlock rest


-- |������������ ������� ������� �����
terse_list direntry compsize = do
  let fi = cfFileInfo direntry
  myPutStrLn$        (formatDateTime$ fiTime fi)
           ++ " " ++ right_justify 11 (if (fiIsDir fi) then ("-dir-") else (show3$ fiSize fi))
                  ++ (if (cfIsEncrypted direntry)  then "*"  else " ")
                  ++ filename direntry

-- |������������ ��������� ������� �����
verbose_list direntry compsize = do
  let fi = cfFileInfo direntry
  myPutStrLn$        (formatDateTime$ fiTime fi)
           ++ " " ++ fiAttrStr fi
           ++ " " ++ right_justify 15 (show$ fiSize fi)
           ++ " " ++ right_justify 15 (show$ compsize)
           ++ " " ++ left_fill  '0' 8 (showHex (cfCRC direntry) "")
                  ++ (if (cfIsEncrypted direntry)  then "*"  else " ")
                  ++ filename direntry

{-
-- |������������� ����������� ������� �����
technical_list direntry = do
  let fi = (cfFileInfo direntry)
  timestr <- formatDateTime (fiTime fi)
  myPutStrLn$ ""
  myPutStrLn$ "Filename: "  ++ (fpFullname$ fiStoredName fi)
  myPutStrLn$ "Size: "      ++ (show$ fiSize fi)
  myPutStrLn$ "Date/time: " ++ timestr
  myPutStrLn$ "CRC: "       ++ showHex (cfCRC direntry) ""
  myPutStrLn$ "Type: "      ++ if (fiIsDir fi) then "directory" else "file"
-}

-- |����������� �������� ������
techinfoHeader archive = do
  tables <- arcGetTechinfo archive []
  for tables $ \table -> do
    myPutStrLn ""
    for table $ \(a,b) -> do
      ai <- i18n a
      myPutStrLn (ai++" "++b)
  myPutStrLn ""

-- |�������� �����-�����
data_block_list bl = do
  myPutStrLn$        (if (blIsEncrypted bl)  then "*"  else " ")
           ++ " " ++ right_justify 15 (show3$ blPos      bl)
           ++ " " ++ right_justify 15 (show3$ blOrigSize bl)
           ++ " " ++ right_justify 15 (show3$ blCompSize bl)
           ++ " " ++ right_justify  7 (show3$ blFiles    bl)
           ++ " " ++ join_compressor (blCompressor bl)
