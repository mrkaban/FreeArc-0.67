{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: Add dialog                                                      ------
----------------------------------------------------------------------------------------------------
module FileManDialogAdd where

import Prelude hiding (catch)
import Control.Concurrent
import Control.OldException
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Cmd
#if defined(FREEARC_WIN)
import System.Win32
#endif
import System.Time

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets            (i18n)
import Compression
import Encryption
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcExtract
import ArcCreate
import FileManPanel
import FileManUtils
import FileManDialogs

----------------------------------------------------------------------------------------------------
---- Диалог упаковки файлов и модификации/слияния архивов ------------------------------------------
----------------------------------------------------------------------------------------------------

addDialog fm' exec winoptions cmd files mode = do
  --start_time  <- getClockTime
  fm <- val fm'
  if isFM_Archive fm && cmd=="a"  then fmErrorMsg fm' "0133 You can't compress files directly from archive!" else do
  if isFM_Archive fm && cmd=="j"  then fmErrorMsg fm' "0145 You can't join archives directly from archive!" else do
  title <- i18n$ case (cmd,files) of
                   ("a" , []    ) -> "0136 Add all files to archive"
                   ("a" , [file]) -> "0134 Add %1 to archive"
                   ("a" , _     ) -> "0135 Add %2 files to archive"
                   ("ch", []    ) -> "0146 Modify all archives"
                   ("ch", [file]) -> "0147 Modify %1"
                   ("ch", _     ) -> "0148 Modify %2 archives"
                   ("j" , []    ) -> "0149 Join all archives"
                   ("j" , [file]) -> "0150 Join %1 with another archive"
                   ("j" , _     ) -> "0151 Join %2 archives"
                   ("cvt",[file]) -> "0428 Convert %1 to "++aFreeArc++" format"
                   ("cvt",_     ) -> "0429 Convert %2 archives to "++aFreeArc++" format"
  let wintitle  =  formatn title [head files, show3$ length files]
  -- Создадим диалог со стандартными кнопками OK/Cancel
  fmDialog fm' wintitle winoptions $ \(dialog,okButton) -> do
    fmCacheConfigFile fm' $ do
    (nb,newPage) <- startNotebook dialog

------ Главная закладка ----------------------------------------------------------------------
    vbox <- newPage "0182 Main";  let pack x = boxPackStart vbox x PackNatural 1
    ------ Архив и каталог в нём ----------------------------------------------------------------------
    arctype                  <- fmArchiveType fm';                  pack (widget arctype)     `on_`  (cmd `notElem` words "ch cvt")
    (arcnameBox, _, arcname) <- fmOutputArchiveFileBox fm' dialog;  pack arcnameBox           `on_`  (cmd `notElem` words "ch cvt")
    (outdirBox,  _, outdir)  <- fmOutputArchiveDirBox  fm' dialog;  pack outdirBox            `on_`  (cmd `notElem` words "ch cvt")
    (hbox,          arcpath) <- fmLabeledEntryWithHistory fm' "arcpath" "0141 Base directory inside archive:";  pack hbox  `on_`  cmd=="a"
    ep                       <- fmExcludePaths;                     pack (widget ep)          `on_`  cmd=="a"
    updateMode               <- fmUpdateMode;                       pack (widget updateMode)  `on_`  cmd=="a"
    ------ Compression/Encryption/Protection ----------------------------------------------------------------------
    (compressionBox, compression, compressionMethod) <- fmCheckedEntryWithHistoryAndChooser fm' ""           (checkBox "0532 Compression:") (const$ return True) (return) (compressionMethodDialog fm');  pack compressionBox
    (encryptionBox,  encryption,  encryptionMethod)  <- fmCheckedEntryWithHistoryAndChooser fm' "encryption" (checkBox "0533 Encryption:")  (const$ return True) (return) (encryptionMethodDialog  fm');  pack encryptionBox
    (protectionBox,  protection,  protectionMethod)  <- fmCheckedEntryWithHistory fm' "protection"  "0185 Protection:"      ;  pack protectionBox
    (commentBox,     comment,     commentFile)       <- fmCheckedEntryWithHistory fm' "comment"     "0186 Comment:"         ;  pack commentBox
    (makeSFXBox,     makeSFX,     sfxFile)           <- fmCheckedEntryWithHistory fm' ""            "0227 Make EXE:"        ;  pack makeSFXBox
    (volumesBox,     volumes,     volumesSize)       <- fmCheckedEntryWithHistory fm' "volumes"     "0486 Split to volumes:";  pack volumesBox
    -- The rest
    testAfter   <- checkBox "0128 Test archive after operation";        pack (widget testAfter)
    deleteFiles <- checkBox "0122 Delete files successfully archived";  pack (widget deleteFiles)  `on_`  cmd=="a"
    lock        <- checkBox "0187 Finalize archive";                    pack (widget lock)
    (hbox, options, optionsStr) <- fmCheckedEntryWithHistory fm' "options" "0072 Additional options:";  pack hbox


------ Закладка архивных опций ----------------------------------------------------------------------
    vbox <- newPage "0200 Archive";  let pack x = boxPackStart vbox x PackNatural 1
    separate <- checkBox "0201 Compress each marked file/directory into separate archive";  pack (widget separate)  `on_`  cmd=="a"
    (hbox, ag, agTemplate) <- fmCheckedEntryWithHistory fm' "ag"  "0202 Add to archive name:";  pack hbox  `on_`  cmd/="ch"
    archiveTimeMode <- comboBox "0203 Set archive time to:"
                                [ "0204 Current system time"
                                , "0205 Original archive time"
                                , "0206 Latest file time" ];  pack (widget archiveTimeMode)

    create <- checkBox "0207 Delete previous archive contents";  pack (widget create)  `on_`  cmd/="ch"
    (sortBox, sort, sortOrder)  <- fmCheckedEntryWithHistory fm' "sort" "0208 Order of files in archive:";  pack sortBox  `on_`  (cmd `elem` words "a cvt")
    recompressMode <- comboBox "0209 Recompression mode:"
                               [ "0210 Quickly append new files"
                               , "0211 Smart recompression of solid blocks (default)"
                               , "0212 Recompress all files"
                               , "0213 Store only fileinfo"
                               , "0214 Store only fileinfo & crcs"
                               , "0215 No archive headers" ];  pack (widget recompressMode)
    backupMode <- comboBox "0216 Backup mode:"
                               [ "0217 No (default)"
                               , "0218 Full: clear \"Archive\" attribute of files succesfully archived"
                               , "0219 Differential: select only files with \"Archive\" attribute set"
                               , "0220 Incremental: select by \"Archive\" attribute & clear it after compression" ];  pack (widget backupMode)  `on_`  (cmd `notElem` words "ch cvt")
    globalQueueing <- fmCheckButtonWithHistory  fm' "GlobalQueueing" False global_queueing_msg;  pack (widget globalQueueing)
    shutdown <- checkBox shutdown_msg;  pack (widget shutdown)


------ Закладка отбора файлов ----------------------------------------------------------------------
    vbox <- newPage "0221 Files";  let pack x = boxPackStart vbox x PackNatural 1
    (hbox, include, includeMasks) <- fmCheckedEntryWithHistory fm' "include" "0222 Include only files:";  pack hbox
    (hbox, exclude, excludeMasks) <- fmCheckedEntryWithHistory fm' "exclude" "0223 Exclude files:";  pack hbox
    (hbox, larger,  largerSize)   <- fmCheckedEntryWithHistory fm' "larger"  "0224 Include only files larger than:";  pack hbox
    (hbox, smaller, smallerSize)  <- fmCheckedEntryWithHistory fm' "smaller" "0225 Include only files smaller than:";  pack hbox
    --times: -tn/to/ta/tb

------ Закладка архивного комментария --------------------------------------------------------------------------
    vbox <- newPage "0199 Comment";  let pack x = boxPackStart vbox x PackGrow 1
    commentText <- scrollableTextView "" [];  pack (widget commentText)


------ Инициализация полей --------------------------------------------------------------------------
    compression     =: mode==RecompressMode || (cmd `elem` words "a cvt")
    encryption      =: mode==EncryptionMode
    protection      =: mode==ProtectionMode
    comment         =: mode==CommentMode
    makeSFX         =: mode==MakeSFXMode
    volumes         =: False
    ep              =: 2
    updateMode      =: 0
    archiveTimeMode =: 0
    recompressMode  =: 1
    backupMode      =: 0

    -- Имя по умолчанию создаваемого архива (выходного каталога) зависит от имён архивируемых файлов/сливаемых архивов
    arcnameBase <- case files of
      [file] -> do let realname = dropTrailingPathSeparator file
                   isFile <- fileExist realname
                   return$ if isFile then dropExtension realname  -- один файл    - избавимся от расширения
                                     else realname                -- один каталог - избавимся от слеша в конце
      _      -> return$ takeFileName (fm_curdir fm)               -- много файлов - используем имя текущего каталога
    outdir  =: fm_curdir fm
    arcpath =: ""

    -- Показать те или иные контролы в зависимости от опции separate и типа создаваемого архива
    let showHideOptions = do
          let showOn flag  =  (if flag then widgetShow else widgetHide)
          separate' <- val separate
          arcnameBox.$ showOn (not separate')
          outdirBox .$ showOn separate'

          arcext <- fmap (archiveTypes!!) (val arctype)
          arcname =: if isFM_Archive fm then fm_arcname fm
                                        else (arcnameBase ||| "archive") ++ "." ++ arcext
          mapM_ (showOn$ arcext == aFreeArcInternalExt)  [protectionBox, commentBox, widget deleteFiles, widget lock
                                                         ,widget archiveTimeMode, widget recompressMode, sortBox, widget recompressMode, widget backupMode]
          mapM_ (showOn$ arcext /= aZipExt)              [makeSFXBox]
          mapM_ (showOn$ arcext /= aFreeArcInternalExt)  [volumesBox]
          let plusArctype  =  if arcext == aFreeArcInternalExt   then id   else (++"."++arcext)
          changeTag compressionMethod (plusArctype "compression")
          changeTag sfxFile           (plusArctype "sfx")
    --
    setOnUpdate arctype  showHideOptions
    setOnUpdate separate showHideOptions

------ Чтение значений полей и сохранение их для истории ------------------------------------------
    widgetShowAll dialog; showHideOptions
    --current_time  <- getClockTime;  debugMsg (show (1000*(diffTimes current_time start_time))++" ms")
    choice <- fmDialogRun fm' dialog "AddDialog"
    when (choice `elem` [ResponseOk, aResponseDetach]) $ do
      -- Запустить команду в отдельной копии FreeArc?
      let detach = (choice == aResponseDetach)
      -- Main settings
      saveHistory arctype
      arcext   <- fmap (archiveTypes!!) (val arctype)   -- default archive extension
      separate'<- val separate
      arcname' <- val arcname;  saveHistory arcname   `on_`  (not separate' && (cmd `notElem` words "ch cvt"))
      outdir'  <- val outdir;   saveHistory outdir    `on_`  (    separate' && (cmd `notElem` words "ch cvt"))
      arcpath' <- val arcpath;  saveHistory arcpath   `on_`  cmd=="a"
      -- Если "имя архива" на самом деле указывает каталог внутри архива, то не ударим в грязь лицом :)
      x <- splitArcPath fm' arcname'
      (arcname', arcpath') <- return$ case x of
          ArcPath arc path -> (arc, path </> arcpath')
          _                -> (arcname', arcpath')
      ep'          <- val ep
      updateMode'  <- val updateMode
      testAfter'   <- val testAfter
      deleteFiles' <- val deleteFiles
      optionsEnabled     <- val options
      ; optionsStr'        <- val optionsStr;         saveHistory optionsStr        `on_` optionsEnabled
      compressionEnabled <- val compression
      ; compressionMethod' <- val compressionMethod;  saveHistory compressionMethod `on_` compressionEnabled
      encryptionEnabled  <- val encryption
      ; encryptionMethod'  <- val encryptionMethod;   saveHistory encryptionMethod  `on_` encryptionEnabled
      protectionEnabled  <- val protection
      ; protectionMethod'  <- val protectionMethod;   saveHistory protectionMethod  `on_` protectionEnabled
      commentEnabled     <- val comment
      ; commentFile'       <- val commentFile;        saveHistory commentFile       `on_` commentEnabled
      ; commentText'       <- val commentText
      sfxEnabled  <- val makeSFX
      ; sfxFile'  <- val sfxFile;   saveHistory sfxFile  `on_` sfxEnabled
      volumesEnabled     <- val volumes
      ; volumesSize'       <- val volumesSize;        saveHistory volumesSize  `on_` volumesEnabled
      -- Archive settings
      agEnabled  <- val ag
      ; agTemplate' <- val agTemplate;      saveHistory agTemplate   `on_` agEnabled
      archiveTimeMode' <- val archiveTimeMode
      lock'      <- val lock
      create'    <- val create
      sortEnabled  <- val sort
      ; sortOrder' <- val sortOrder;        saveHistory sortOrder    `on_` sortEnabled
      recompressMode' <- val recompressMode
      backupMode'     <- val backupMode
      globalQueueing' <- val globalQueueing -- don't save to history - this selection is for one command only
      shutdown'       <- val shutdown
      -- File selection settings
      includeEnabled  <- val include
      ; includeMasks' <- val includeMasks;  saveHistory includeMasks `on_` includeEnabled
      excludeEnabled  <- val exclude
      ; excludeMasks' <- val excludeMasks;  saveHistory excludeMasks `on_` excludeEnabled
      largerEnabled   <- val larger
      ; largerSize'   <- val largerSize;    saveHistory largerSize   `on_` largerEnabled
      smallerEnabled  <- val smaller
      ; smallerSize'  <- val smallerSize;   saveHistory smallerSize  `on_` smallerEnabled
      -- Encryption and decryption options
      pwd' <- val encryptionPassword
      encryptionOptions <- return$ (words encryptionMethod' `contains` "-p?") &&& pwd' &&& ["-p"++pwd']
      decryptionOptions <- fmGetDecryptionOptions fm'
{-
      -- Запомним настройки в истории
      fmAddHistory fm' "acmd"$ joinWith "," [ "simpleMethod="  ++simpleMethod'
                                            , "akeyfile="      ++keyfile'
                                            , "xkeyfile="      ++xkeyfile'
                                            , "encryptHeaders="++show encryptHeaders'
                                            , "testAfter="     ++show testAfter']
-}
      -- Отобразим изменение имени архива
      when sfxEnabled $ do
        when (isFM_Archive fm) $ do
        let newname' = changeSfxExt True (clear sfxFile') arcext arcname'
        when (newname'/=arcname') $ do
          fmChangeArcname fm' newname'

------ Формирование выполняемой команды/команд ----------------------------------------------------
      let msgs = case cmd of
                  "ch"-> ["0237 Modifying %1",
                          "0238 SUCCESFULLY MODIFIED %1",
                          "0239 %2 WARNINGS WHILE MODIFYING %1"]
                  "j" -> ["0240 Joining archives to %1",
                          "0241 SUCCESFULLY JOINED ARCHIVES TO %1",
                          "0242 %2 WARNINGS WHILE JOINING ARCHIVES TO %1"]
                  _   -> ["0243 Adding to %1",
                          "0244 FILES SUCCESFULLY ADDED TO %1",
                          "0245 %2 WARNINGS WHILE ADDING TO %1"]
      let use_winrar = (arcext==aRarExt)
          options =
            -- Main page settings
            [not use_winrar     &&& ("-t"++arcext)]++
            (compressionEnabled &&&  cvt "-m"   compressionMethod')++
            (encryptionEnabled  &&& (cvt "-ae=" encryptionMethod' ++ encryptionOptions))  ++decryptionOptions++
            (protectionEnabled  &&&  cvt "-rr"  protectionMethod')++
            (commentEnabled     &&&  [((clear commentFile' !~ "-z*" &&& "--archive-comment=")++) (clear commentFile' ||| commentText')])++
            (sfxEnabled         &&&  cvt "-sfx" sfxFile')++
            (volumesEnabled     &&&  cvt "-v" volumesSize')++
            (testAfter'         &&&  ["-t"])++
            (deleteFiles'       &&&  ["-d"])++
            (null files         &&&  ["-r"])++
            (arcpath'           &&&  ["-ap"++clear arcpath'])++
            (ep'            `select`  "-ep,-ep1,,-ep2,-ep3")++
            (updateMode'    `select`  ",-u,-f,--sync")++
            -- Archive settings
            (lock'                &&&   ["-k"])++
            (agEnabled            &&&   ["-ag"++clear agTemplate'])++
            (sortEnabled          &&&   ["-ds"++clear sortOrder'])++
            (archiveTimeMode' `select`  ",-tk,-tl")++
            (backupMode'      `select`  ",-ac,-ao,-ac -ao")++
            (recompressMode'  `select`  "--append,,--recompress,--nodata,--crconly,--nodir")++
            ((cmd `notElem` words "a cvt") &&& (compressionEnabled || encryptionEnabled)  &&&  ["--recompress"])++
            (globalQueueing'  &&&  ["--queue"])++
            (shutdown'        &&&  ["--shutdown"])++
            -- File selection settings
            (includeEnabled   &&&  cvt1 "-n" includeMasks')++
            (excludeEnabled   &&&  cvt1 "-x" excludeMasks')++
            (largerEnabled    &&&  ["-sm"++clear largerSize'])++
            (smallerEnabled   &&&  ["-sl"++clear smallerSize'])++
            -- Other
            (cmd/="cvt"       &&&  ["-dp"++fm_curdir fm])++
            (cmd=="ch"        &&&  ["--noarcext"])++
            (optionsEnabled   &&&  words (clear optionsStr'))
      --
      let command archive filelist =
            [if create' then "create" else cmd] ++ options ++ ["--", clear archive] ++ filelist
      --
      if cmd=="cvt" then
        do all2arc <- all2arc_path
           Files.runCommand (unparseCommand$ [all2arc] ++ options ++ ["--"] ++ files) (fm_curdir fm) False
        else do
      exec detach use_winrar doNothing$
             if cmd=="ch" then (files ||| ["*"]) .$map (\archive -> command (fm_curdir fm </> archive) [])
        else if separate' then files.$map (\file -> command (outdir' </> dropTrailingPathSeparator file++"."++arcext) [file])
                          else [command arcname' files]


----------------------------------------------------------------------------------------------------
---- Вспомогательные определения -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Поле выбора имени выходного архива
fmOutputArchiveFileBox fm' dialog =
  fmFileBox fm' dialog
            "arcname" FileChooserActionSave
     (label "0131 Output archive:")
            "0132 Select output archive"
            aARCFILE_FILTER
            (const$ return True)
            (fmCanonicalizeDiskPath fm')

-- |Поле выбора выходного каталога при создании нескольких архивов
fmOutputArchiveDirBox fm' dialog =
  fmFileBox fm' dialog
            "dir" FileChooserActionSelectFolder
     (label "0004 Output directory:")
            "0021 Select output directory"
            aANYFILE_FILTER
            (const$ return True)
            (fmCanonicalizeDiskPath fm')

-- |Поле выбора опции -ep
fmExcludePaths =
  comboBox "0188 Store file paths:"
           [ "0189 No"
           , "0190 Relative to compressed dir"
           , "0191 Relative to curdir (default)"
           , "0192 Absolute (relative to root dir)"
           , "0193 Full (including drive letter)" ]

-- |Поле выбора режима обновления.
fmUpdateMode =
  comboBox "0194 Update mode:"
           [ "0195 Add and replace files (default)"
           , "0196 Add and update files"
           , "0197 Fresh existing files"
           , "0198 Synchronize archive with disk contents" ]

-- |Поле выбора типа архива.
fmArchiveType fm' =
  fmComboBoxWithHistory fm' "arctype" 0 "0495 Archive type:"
#ifndef HAMSTER
           [ "0496 arc (default)"
           , "0999 zip"
--         , "0999 rar"
#else
           [ "0999 zip (default)"
#endif
           , "0999 7z"]

-- Тип архива (-t) и одновременно расширение по умолчанию архивных файлов. В том же порядке, что в комбобоксе выше.
#ifndef HAMSTER
archiveTypes  =  words "arc zip 7z"  -- add rar before 7z
#else
archiveTypes  =  words "zip 7z"
#endif

