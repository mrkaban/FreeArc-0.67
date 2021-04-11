{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: Extract/ArcInfo/Settings dialogs                                ------
----------------------------------------------------------------------------------------------------
module FileManDialogs where

import Prelude hiding (catch)
import Control.Concurrent
import Control.OldException
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Cmd
#if defined(FREEARC_WIN)
import System.Win32
#endif

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets
import Compression
import Encryption
import Options
import UIBase
import UI
import ArhiveStructure
import Arhive7zLib
import ArcExtract
import FileManPanel
import FileManUtils

----------------------------------------------------------------------------------------------------
---- Диалог распаковки файлов ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

extractDialog fm' exec winoptions cmd arcnames arcdir files = do
  fm <- val fm'
  title <- i18n$ case (cmd, files, arcnames) of
                   ("t", [],     [])        -> "0157 Test all archives"
                   ("t", [],     [arcname]) -> "0152 Test %3"
                   ("t", [file], [arcname]) -> "0153 Test %1 from %3"
                   ("t", files,  [arcname]) -> "0154 Test %2 files from %3"
                   ("t", files,  arcnames)  -> "0155 Test %4 archives"
                   (_,   [],     [])        -> "0158 Extract all archives"
                   (_,   [],     [arcname]) -> "0024 Extract files from %3"
                   (_,   [file], [arcname]) -> "0025 Extract %1 from %3"
                   (_,   files,  [arcname]) -> "0026 Extract %2 files from %3"
                   (_,   files,  arcnames)  -> "0027 Extract files from %4 archives"
  let wintitle  =  formatn title [head files, show3$ length files, takeFileName$ head arcnames, show3$ length arcnames]
  -- Создадим диалог со стандартными кнопками OK/Cancel
  fmDialog fm' wintitle winoptions $ \(dialog,okButton) -> do
    upbox <- dialogGetUpper dialog

    ; outFrame <- frameNew
    ; boxPackStart upbox outFrame           PackNatural 5         `on_` cmd/="t"
    ;   vbox <- vBoxNew False 0
    ;   set outFrame [containerChild := vbox, containerBorderWidth := 5]
    (hbox, _, dir) <- fmFileBox fm' dialog
                                "dir" FileChooserActionSelectFolder
                         (label "0004 Output directory:")
                                "0021 Select output directory"
                                aANYFILE_FILTER
                                (const$ return True)
                                (fmCanonicalizeDiskPath fm')
    ; boxPackStart vbox hbox                      PackNatural 0
    addDirButton <- checkBox "0014 Append archive name to the output directory"
    ; boxPackStart vbox (widget addDirButton)     PackNatural 0
    openOutDirButton <- checkBox "0468 Open output directory in Explorer"
    ; boxPackStart vbox (widget openOutDirButton) PackNatural 0

    overwrite <- radioFrame "0005 Overwrite mode"
                            [ "0001 Ask before overwrite",
                              "0002 Overwrite without prompt",
                              "0003 Update old files",
                              "0051 Skip existing files" ]
    ; boxPackStart upbox (widget overwrite) PackNatural 5         `on_` cmd/="t"

    (decryption, decryptionOnOK) <- decryptionBox fm' dialog   -- Настройки расшифровки
    ; boxPackStart upbox decryption           PackNatural 5

    keepBrokenButton <- fmCheckButtonWithHistory fm' "KeepBroken" False "0425 Keep broken extracted files"
    ; boxPackStart upbox (widget keepBrokenButton) PackNatural 0  `on_` cmd/="t"

    globalQueueing <- fmCheckButtonWithHistory  fm' "GlobalQueueing" False global_queueing_msg
    ; boxPackStart upbox (widget globalQueueing) PackNatural 0

    shutdown <- checkBox shutdown_msg
    ; boxPackStart upbox (widget shutdown)    PackNatural 0

    (hbox, options, optionsStr)  <- fmCheckedEntryWithHistory fm' "xoptions" "0072 Additional options:"
    ; boxPackStart upbox hbox                 PackNatural 0


    -- Установим выходной каталог в значение по умолчанию
    case arcnames of
      [arcname] -> do dir =:: fmCanonicalizeDiskPath fm' (takeBaseName arcname)
      _         -> do dir =:: fmCanonicalizeDiskPath fm' "."; addDirButton=:True


    widgetShowAll upbox
    showTestDialog <- fmGetHistoryBool fm' "ShowTestDialog" False
    choice <- if cmd/="t" || showTestDialog
                then fmDialogRun fm' dialog (if cmd/="t" then "ExtractDialog" else "TestDialog")
                else return ResponseOk
    when (choice `elem` [ResponseOk, aResponseDetach]) $ do
      overwriteOption    <- val overwrite
      dir'               <- val dir;                saveHistory dir
      isAddDir           <- val addDirButton
      isOpenOutDir       <- val openOutDirButton
      decryptionOptions  <- decryptionOnOK
      keepBroken         <- val keepBrokenButton
      globalQueueing'    <- val globalQueueing -- don't save to history - this selection is for one command only
      shutdown'          <- val shutdown
      optionsEnabled     <- val options
      ; optionsStr'        <- val optionsStr;       saveHistory optionsStr  `on_`  optionsEnabled
      let outdir = dir' .$ (isAddDir &&& length(arcnames)==1 &&& (</> takeBaseName(head arcnames)))
          use_winrar = False
      exec (choice == aResponseDetach)                                               -- Запустить команду в отдельной копии FreeArc?
           use_winrar
           (\ok -> when isOpenOutDir (runFile (outdir++[pathSeparator]) "" False))   -- post-operation action: open outdir in Explorer
           ((arcnames ||| ["*"]) .$map (\arcname ->
                [cmd]++
                (cmd/="t" &&& (
                    ["-dp"++clear dir']++
                    (isAddDir &&& ["-ad"])++
                    (arcdir &&& files &&& ["-ap"++clear arcdir])++
                    (keepBroken &&& ["-kb"])++
                    (overwriteOption  `select`  ",-o+,-u -o+,-o-")))++
                decryptionOptions++
                ["--fullnames"]++
                ["--noarcext"]++
                (globalQueueing'  &&&  ["--queue"])++
                (shutdown'        &&&  ["--shutdown"])++
                (optionsEnabled   &&&  words (clear optionsStr'))++
                ["--", clear arcname]++files))


----------------------------------------------------------------------------------------------------
---- Диалог информации об архиве -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

arcinfoDialog fm' exec mode arcnames arcdir files = do
  handle (\e -> fmErrorMsg fm' "0013 There are no archives selected!") $ do
  fm <- val fm'
  let arcname = head arcnames
  fm_arc <- case () of _ | isFM_Archive fm -> return (subfm fm)
                         | otherwise       -> with' (newFMArc fm' arcname "") (return) (\_ -> closeFMArc fm')
  let archive    = subfm_archive fm_arc
      footer     = arcFooter archive
      dataBlocks = arcDataBlocks archive   -- список солид-блоков
      dirs_and_files = [("0173 Directories:", show3$ ftDirs$  subfm_filetree fm_arc)
                       ,("0088 Files:",       show3$ ftFiles$ subfm_filetree fm_arc)]

  title <- i18n"0085 All about %1"
  let wintitle  =  format title (takeFileName arcname)
  -- Создадим диалог со стандартными кнопками OK/Cancel
  fmDialog fm' wintitle [] $ \(dialog,okButton) -> do
    (nb,newPage) <- startNotebook dialog
------ Главная закладка ----------------------------------------------------------------------------
    vbox <- newPage "0174 Main";  let pack n makeControl = do control <- makeControl
                                                              boxPackStart vbox control PackNatural n
    tables <- arcGetTechinfo archive dirs_and_files
    for (zip [10,0,10,0,10] tables) $ \(n,table) -> do
      pack n (twoColumnTable table)


------ Закладка с описаниями солид-блоков ----------------------------------------------------------
#ifndef HAMSTER
    vBox <- newPage "0449 Solid blocks"
    let columnTitles = ["0450 Position", "0451 Size", "0452 Compressed", "0453 Files", "0454 Method"]
        n = map i18no columnTitles
    s <- i18ns columnTitles
    let compressor = join_compressor.blCompressor
    (listUI, listView, listModel, listSelection, columns, onColumnTitleClicked) <-
        createListView compressor [(n!!0, s!!0, (show3.blPos),      [New.cellTextEditable := True, cellXAlign := 1]),
                                   (n!!1, s!!1, (show3.blOrigSize), [New.cellTextEditable := True, cellXAlign := 1]),
                                   (n!!2, s!!2, (show3.blCompSize), [New.cellTextEditable := True, cellXAlign := 1]),
                                   (n!!3, s!!3, (show3.blFiles),    [New.cellTextEditable := True, cellXAlign := 1]),
                                   (n!!4, s!!4, (compressor),       [New.cellTextEditable := True])]
    boxPackStart vBox listUI PackGrow 0
    changeList listModel listSelection dataBlocks
    -- При закрытии диалога сохраним порядок и ширину колонок, при открытии восстановим их
    restoreColumnsOrderAndWidths fm' "SolidBlocks" listView columns
#endif


------ Закладка комментария архива -----------------------------------------------------------------
    vbox <- newPage "0199 Comment"

    comment <- scrollableTextView (ftComment footer) []
    boxPackStart vbox (widget comment) PackGrow 0

    widgetShowAll dialog
    notebookSetCurrentPage nb 1    `on_` mode==CommentMode
    choice <- fmDialogRun fm' dialog "ArcInfoDialog"
#ifndef HAMSTER
    saveColumnsOrderAndWidths fm' "SolidBlocks" listView columns
#endif
    when (choice==ResponseOk) $ do
      newComment <- val comment
      when (newComment /= ftComment footer) $ do
        let use_winrar = False
        exec False use_winrar doNothing [["ch"
                                         ,"--noarcext"
                                         ,newComment &&& ("--archive-comment="++newComment)
                                                     ||| "-z-"
                                         ,"--"
                                         ,arcname]]


----------------------------------------------------------------------------------------------------
---- Диалог настроек программы ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

settingsDialog fm' = do
  fm <- val fm'
  fmDialog fm' "0067 Settings" [] $ \(dialog,okButton) -> do
    (nb,newPage) <- startNotebook dialog
------ Главная закладка ----------------------------------------------------------------------
    vbox <- newPage "0174 Main";  let pack x = boxPackStart vbox x PackNatural 1
    aboutLabel         <- labelNewWithMnemonic aARC_HEADER_WITH_DATE
    langLabel          <- label "0068 Language:"
    langComboBox       <- New.comboBoxNewText
    editLangButton     <- button "0069 Edit"
    convertLangButton  <- button "0070 Import"
    -- Логфайл
    (logfileBox, _, logfile) <- fmFileBox fm' dialog
                                          "logfile" FileChooserActionSave
                                   (label "0166 Logfile:")
                                          "0167 Select logfile"
                                          aANYFILE_FILTER
                                          (const$ return True)
                                          (fmCanonicalizeDiskPath fm')
    ; viewLogfileButton <- button "0292 View"
    -- Каталог для временных файлов
    (tempdirBox, _, tempdir) <- fmFileBox fm' dialog
                                          "tempdir" FileChooserActionSelectFolder
                                   (label "0447 Temporary directory:")
                                          "0448 Select directory for temporary files"
                                          aANYFILE_FILTER
                                          (const$ return True)
                                          (fmCanonicalizeDiskPath fm')
    -- Прочее
    checkNewsButton <- fmCheckButtonWithHistory fm' "CheckNews"       True "0370 Watch for new versions via Internet"
    notes           <- label . joinWith "\n" =<<
      i18ns["0168 You should restart "++aFreeArc++" in order for a language settings to take effect.",
            "0169 Passwords need to be entered again after restart."]

-----------------------------------------------------------------------------------------------
    -- Информация о текущем языке локализации
    langTable <- tableNew 2 2 False
    let dataset = [("0170 Full name:", "0000 English"), ("0171 Copyright:", "0159 ")]
    labels <- foreach [0..1] $ \y -> do
      -- Первая колонка
      label1 <- labelNew Nothing;  let x=0
      tableAttach langTable label1 (x+0) (x+1) y (y+1) [Fill] [Fill] 5 5
      miscSetAlignment label1 0 0
      -- Вторая колонка
      label2 <- labelNew Nothing
      tableAttach langTable label2 (x+1) (x+2) y (y+1) [Expand, Fill] [Expand, Fill] 5 5
      set label2 [labelSelectable := True]
      miscSetAlignment label2 0 0
      return (label1, label2)
    --
    let showLang i18n = do
          for (zip labels dataset) $ \((l1,l2),(s1,s2)) -> do
            labelSetTextWithMnemonic l1      =<< i18n s1
            labelSetMarkup           l2.bold =<< i18n s2
    --
    showLang i18n

    -- Файл языковой локализации
    langFile <- fmGetHistory1 fm' aINITAG_LANGUAGE ""

    -- Заполнить список языков именами файлов в каталоге arc.languages и выбрать активный язык
    langDir   <- findDir libraryFilePlaces aLANG_DIR
    langFiles <- langDir &&& (dir_list langDir >>== map baseName >>== sort >>== filter (match "arc.*.txt"))
    -- Отобразим языки в 5 столбцов, с сортировкой по столбцам
    let cols = 5
        rows = (length langFiles) `divRoundUp` cols;  add = rows*cols - length langFiles
        sortOnColumn x  =  r*cols+c  where (c,r) = x `divMod` rows  -- пересчитать из поколоночных позиций в построчные
    ;   langFiles <- return$ map snd $ sort $ zip (map sortOnColumn [0..]) (langFiles ++ replicate add "")
    --
    for langFiles (New.comboBoxAppendText langComboBox . mapHead toUpper . replace '_' ' ' . dropEnd 4 . drop 4)
    langComboBox  `New.comboBoxSetWrapWidth`  cols
    whenJust_ (elemIndex (takeFileName langFile) langFiles)
              (New.comboBoxSetActive langComboBox)

    -- Определить файл локализации, соответствующий выбранному в комбобоксе языку
    let getCurrentLangFile = do
          lang <- New.comboBoxGetActive langComboBox
          case lang of
            -1   -> return ""
            lang -> myCanonicalizePath (langDir </> (langFiles !! lang))

    -- При выборе другого языка локализации вывести информацию о нём
    on langComboBox changed $ do
      choice <- New.comboBoxGetActive langComboBox
      when (choice /= -1) $ do
        langFile   <- getCurrentLangFile
        localeInfo <- parseLocaleFiles [langFile]
        showLang (i18n_general (return localeInfo) .>>== fst)

    -- Редактирование текущего файла локализации/логфайла
    editLangButton    `onClick` (runEditCommand =<< getCurrentLangFile)
    viewLogfileButton `onClick` (runViewCommand =<< val logfile)

    ; langFrame <- frameNew
    ;   vbox1 <- vBoxNew False 0
    ;   set langFrame [containerChild := vbox1, containerBorderWidth := 5]
    ;     langbox <- hBoxNew False 0
    boxPackStart langbox    (widget  langLabel)          PackNatural 0
    boxPackStart langbox             langComboBox        PackGrow    5
    boxPackStart langbox    (widget  editLangButton)     PackNatural 5
    --boxPackStart langbox    (widget  convertLangButton)  PackNatural 5
    boxPackStart vbox1               langbox             PackNatural 5
    boxPackStart vbox1               langTable           PackNatural 5
    boxPackStart logfileBox (widget  viewLogfileButton)  PackNatural 5
    boxPackStart vbox                aboutLabel          PackNatural 5
    boxPackStart vbox                langFrame           PackNatural 5
    boxPackStart vbox                logfileBox          PackNatural 5
    boxPackStart vbox                tempdirBox          PackNatural 5
    boxPackStart vbox       (widget  checkNewsButton)    PackNatural 5
    boxPackStart vbox       (widget  notes)              PackNatural 5

------ Закладка настроек интерфейса -----------------------------------------------------------
    vbox <- newPage "0466 Interface";  let pack x = boxPackStart vbox x PackNatural 1

    toolbarTextButton          <- fmCheckButtonWithHistory  fm' "ToolbarCaptions"       True  "0361 Add captions to toolbar buttons";            pack (widget toolbarTextButton)
    horizontalGridLinesButton  <- fmCheckButtonWithHistory  fm' "HorizontalGridLines"   False "0507 Grid lines in filelist";                     pack (widget horizontalGridLinesButton)
    showHiddenFilesButton      <- fmCheckButtonWithHistory  fm' "ShowHiddenFiles"       False "0499 Show hidden files, folders and disks";       pack (widget showHiddenFilesButton)
    showTestDialogButton       <- fmCheckButtonWithHistory  fm' "ShowTestDialog"        False "0469 Show \"Test archive\" dialog";               pack (widget showTestDialogButton)
    targzButton                <- fmCheckButtonWithHistory  fm' "QuickOpenTarGz"        True  "0485 Open .tar.gz-like archives in single step";  pack (widget targzButton)
    (hbox, extract_all_for)    <- fmLabeledEntryWithHistory fm' "ExtractAllFor"               "0467 Unpack whole archive when running:";         pack hbox
    (hbox, run_for)            <- fmLabeledEntryWithHistory fm' "RunFor"                      "0500 Run instead of open as archive:";            pack hbox
    globalQueueingButton       <- fmCheckButtonWithHistory  fm' "GlobalQueueing"        False global_queueing_msg;                               pack (widget globalQueueingButton)

------ Закладка интеграции с Explorer ---------------------------------------------------------
#if defined(FREEARC_WIN)
    vbox <- newPage "0421 Explorer integration";  let pack x = boxPackStart vbox x PackNatural 1

    (associateFreeArcBox,     associateFreeArc,     associateFreeArcExtensions)     <- fmCheckedEntryWithHistory2 fm' "Settings.Associate.FreeArc"      False  "0543 Associate with FreeArc archives:"
    (associateOtherArcBox,    associateOtherArc,    associateOtherArcExtensions)    <- fmCheckedEntryWithHistory2 fm' "Settings.Associate.OtherArc"     False  "0544 Associate with other archives:"
    (associateContextMenuBox, associateContextMenu, associateContextMenuExtensions) <- fmCheckedEntryWithHistory2 fm' "Settings.Associate.ContextMenu"  False  "0545 Context menu for container files:"
    (associateSmartMenuBox,   associateSmartMenu,   associateSmartMenuExtensions)   <- fmCheckedEntryWithHistory2 fm' "Settings.Associate.SmartMenu"    False  "0546 Smart context menu for files:"

    contextMenuButton  <- fmCheckButtonWithHistory fm' "Settings.ContextMenu"           False   "0422 Enable context menu in Explorer"
    cascadedButton     <- fmCheckButtonWithHistory fm' "Settings.ContextMenu.Cascaded"  True    "0423 Make it cascaded"
    empty              <- label ""
#ifndef HAMSTER
    pack `mapM_` [associateFreeArcBox]
#endif
    pack `mapM_` [associateOtherArcBox, associateContextMenuBox, associateSmartMenuBox, widget empty]

    frame <- frameNew;  frameSetLabelWidget frame (widget contextMenuButton)
    boxPackStart vbox frame PackGrow 1

    hbox <- hBoxNew False 0;  containerAdd frame hbox

    let show_or_hide = widgetSetSensitivity hbox =<< val contextMenuButton
    show_or_hide
    show_or_hide .$ setOnUpdate contextMenuButton
    oldContextMenu <- val contextMenuButton

    vbox <- vBoxNew False 0;  boxPackStart hbox vbox PackGrow 10
    let pack x = boxPackStart vbox x PackNatural 1

    empty <- label ""
    notes <- label =<< i18n"0424 Enable individual commands:"
    mapM_ (pack.widget) [cascadedButton, empty, notes]

    -- Put all subsequent checkboxes into scrolled window
    vbox <- createScroller vbox
    let pack x = boxPackStart vbox x PackNatural 1

    let makeButton ("",_,_)             = do pack =<< hSeparatorNew; return []
        makeButton (cmdname,itext,imsg) = do
          button <- fmCheckButtonWithHistory fm' ("Settings.ContextMenu.Command."++cmdname) True imsg
          pack (widget button)
          return [button]

    commands <- getExplorerCommands >>= concatMapM makeButton
#endif

------ Закладка сжатия ------------------------------------------------------------------------
    (_, saveCompressionHistories) <- compressionPage fm' =<< newPage "0106 Compression"

------ Закладка шифрования --------------------------------------------------------------------
    (_, saveEncryptionHistories)  <-  encryptionPage fm' dialog okButton =<< newPage "0119 Encryption"

------ Закладка информации о системе ----------------------------------------------------------
    vbox <- newPage "0388 Info";  let pack n makeControl = do control <- makeControl
                                                              boxPackStart vbox control PackNatural n

    maxBlock <- getMaxBlockToAlloc
    pack 10 $twoColumnTable [("0461 Largest address space block:", showMem (maxBlock `roundDown` mb))]


-----------------------------------------------------------------------------------------------
    widgetShowAll dialog
    choice <- fmDialogRun fm' dialog "SettingsDialog"
    when (choice==ResponseOk) $ do
      -- Сохраняем настройки и keyfile в INI-файл, пароли - в глоб. переменных
      langFile <- getCurrentLangFile
      fmReplaceHistory fm' aINITAG_LANGUAGE (takeFileName langFile)
      loadTranslation
      saveHistory `mapM_` [logfile, tempdir, extract_all_for, run_for]
      saveHistory `mapM_` [checkNewsButton, toolbarTextButton, horizontalGridLinesButton, showHiddenFilesButton, showTestDialogButton, targzButton, globalQueueingButton]
      saveCompressionHistories
      saveEncryptionHistories
#if defined(FREEARC_WIN)
      saveHistory `mapM_` ([associateFreeArc, associateOtherArc, associateContextMenu, associateSmartMenu, contextMenuButton, cascadedButton] ++ commands)
      saveHistory `mapM_` ([associateFreeArcExtensions, associateOtherArcExtensions, associateContextMenuExtensions, associateSmartMenuExtensions])
      registerShellExtensions' (fm_history fm) (Just oldContextMenu)
#endif
      return ()


----------------------------------------------------------------------------------------------------
---- (Де)регистрация shell extension и ассоциации FreeArc с архивными файлами ----------------------
----------------------------------------------------------------------------------------------------

#if defined(FREEARC_WIN)
-- |Регистрация/Удаление регистрации через командную строку
changeRegisterShellExtensions action = do
  hf' <- openHistoryFile
  when (action==["--unregister"]) $ do
    hfReplaceHistoryBool hf' "Settings.Associate.FreeArc.Enabled"      False
    hfReplaceHistoryBool hf' "Settings.Associate.OtherArc.Enabled"     False
    hfReplaceHistoryBool hf' "Settings.Associate.ContextMenu.Enabled"  False
    hfReplaceHistoryBool hf' "Settings.Associate.SmartMenu.Enabled"    False
    hfReplaceHistoryBool hf' "Settings.ContextMenu"                    False
  --
  registerShellExtensions' hf' Nothing

-- |Изменение настроек интеграции с Explorer
registerShellExtensions' hf' oldContextMenu = do
  hfCacheConfigFile hf' $ do
  associateArc <- hfGetHistoryBool hf' "Settings.Associate.FreeArc.Enabled"   False
  associateZip <- hfGetHistoryBool hf' "Settings.Associate.OtherArc.Enabled"  False
  contextMenu  <- hfGetHistoryBool hf' "Settings.ContextMenu"                 False

  freearc_extensions_raw       <- hfGetHistory1 hf' "Settings.Associate.FreeArc"  ""
  other_archive_extensions_raw <- hfGetHistory1 hf' "Settings.Associate.OtherArc" ""
  let freearc_extensions       = associateArc &&& words freearc_extensions_raw
      other_archive_extensions = associateZip &&& words other_archive_extensions_raw
      new_extensions           = sort$ removeDups$ freearc_extensions++other_archive_extensions

  exe <- getExeName                                -- Name of FreeArc.exe file
  let ico   =  exe `replaceExtension` ".ico"       -- Name of FreeArc.ico file
      dir   =  exe.$takeDirectory                  -- FreeArc.exe directory
      shext =  dir </> "ArcShellExt"               -- Shell extension directory
      empty =  dir </> "empty.arc"                 -- Name of empty archive file
      version  = aARC_VERSION_WITH_DATE
      reg   = registryGetStr hKEY_CLASSES_ROOT
  --
  old_shext      <- hfGetHistory1 hf' "Settings.ContextMenu.Directory" ""
  hfReplaceHistory                hf' "Settings.ContextMenu.Directory" shext
  old_version    <- hfGetHistory1 hf' "Settings.ContextMenu.Version"   ""
  hfReplaceHistory                hf' "Settings.ContextMenu.Version"   version
  old_extensions <- hfGetHistory1 hf' "Settings.Associate.Extensions"  "" >>== words >>== removeDups >>== sort
  hfReplaceHistory                hf' "Settings.Associate.Extensions"  (unwords new_extensions)

  -- UAC-compatibility: instead of modifying registry directly, we are calling external executables that have admin privileges
  reglist32 <- newList;  reglist64 <- newList
  let add_to_list x  =  for [reglist32, reglist64] (<<=x)
  let register      key name value  =  mapM_ add_to_list ["RegistryCreateKey", key, name, value]
      regDeleteTree key             =  mapM_ add_to_list ["RegistryDeleteTree", key]
      runDll32      dll func        =  mapM_ (reglist32 <<=) ["RunDll", dll, func]
      runDll64      dll func        =  mapM_ (reglist64 <<=) ["RunDll", dll, func]

  -- (Un)registering ArcShellExt dlls - performed only if any setting was changed
  let arcShellExt action = when ((oldContextMenu,old_shext,old_version) /= (Just contextMenu,shext,version)) $ do
                             runDll32 (shext </> "ArcShellExt.dll")    action
                             runDll64 (shext </> "ArcShellExt-64.dll") action

  -- Unregister DLL
  arcShellExt "DllUnregisterServer"

  -- Reassociate archives with FreeArc - performed only if any setting was changed
  when (new_extensions/=old_extensions  ||  version/=old_version) $ do
    -- Remove any old associations
    regDeleteTree ("*\\shell\\"++aFreeArc)               -- these registry entries were used
    regDeleteTree ("Directory\\shell\\"++aFreeArc)       --   for Explorer integration in FreeArc 0.50
    add_ext_list <- newList;  delete_ext_list <- newList;  add_type_list <- newList;  delete_type_list <- newList

    -- Cycle through all extensions - associated before and associated now, and make decisions about things to delete and things to create
    for (removeDups$ new_extensions++old_extensions) $ \ext -> do
      -- Make decisions about the "HKCR\.ext" registry branch
      eq  <-  (Just (aFreeArc++"."++ext) ==)  `fmap`  reg ("."++ext) ""                          -- Does extension already associated with FreeArc?
      case () of
        _ | ext `notElem` new_extensions     -> delete_ext_list <<= ext                          -- remove association
          | ext `elem` old_extensions && eq  -> doNothing0                                       -- leave as is (because existing association is OK)
          | otherwise                        -> for [delete_ext_list, add_ext_list] (<<=ext)     -- remove and create again
      -- Make decisions about the "HKCR\FreeArc.ext" registry branch
      eq  <-  (Just exe ==)  `fmap`  reg (aFreeArc++"."++ext) "Owner"                            -- Does "FreeArc.ext" filetype already owned by the current FreeArc installation?
      case () of
        _ | ext `notElem` new_extensions     -> delete_type_list <<= ext                         -- remove association
          | ext `elem` old_extensions && eq  -> doNothing0                                       -- leave as is (because existing association is OK)
          | otherwise                        -> for [delete_type_list, add_type_list] (<<=ext)   -- remove and create again
    --
    -- Perform all the arranged deletions and additions
    forL delete_ext_list  $ \ext -> regDeleteTree ("."++ext)
    forL delete_type_list $ \ext -> regDeleteTree (aFreeArc++"."++ext)
    forL add_type_list    $ \ext -> do
      register  (aFreeArc++"."++ext)                            ""          ((if ext `elem` freearc_extensions then aFreeArc else map toUpper ext)++" archive")
      register  (aFreeArc++"."++ext)                            "Owner"     exe   -- used to check that exension is already associated with this program installation
      register  (aFreeArc++"."++ext++"\\DefaultIcon")           ""          (ico++",0")
      register  (aFreeArc++"."++ext++"\\shell")                 ""          ("open")
      register  (aFreeArc++"."++ext++"\\shell\\open\\command")  ""          ("\""++exe++"\" \"%1\"")
    --
    forL add_ext_list $ \ext -> do
      register  ("."++ext)                                      ""          (aFreeArc++"."++ext)
    --register  (".arc\\ShellNew")                              "FileName"  (empty)   -- disabled because we don't yet support Drag&Drop

  -- Add items to Explorer's right-click menu and register DLL
  when contextMenu $ do
    writeShellExtScript hf'
    arcShellExt "DllRegisterServer"

  -- Run external executables with admin privileges that make actual changes to the registry
  onList reglist32 $ \list32 -> list32 &&& runCommand (unparseCommand$ (shext</>("Manager of FreeArc integration settings.exe"         )):"0.60":list32) "." True
  onList reglist64 $ \list64 -> list64 &&& runCommand (unparseCommand$ (shext</>("Manager of FreeArc integration settings (64-bit).exe")):"0.60":list64) "." True

#else
changeRegisterShellExtensions = doNothing
#endif


----------------------------------------------------------------------------------------------------
---- Dialogs for choosing compression and encryption method called from the Add dialog -------------
----------------------------------------------------------------------------------------------------

compressionMethodDialog fm'  =  methodDialog fm' "0106 Compression" "CompressionMethodDialog" (\fm' _ _ vbox -> compressionPage fm' vbox)
encryptionMethodDialog  fm'  =  methodDialog fm' "0119 Encryption"  "EncryptionMethodDialog"  encryptionPage

methodDialog fm' title dialogName methodPage getMethod setMethod = do
  fm <- val fm'
  fmDialog fm' title [] $ \(dialog,okButton) -> do
    upbox <- dialogGetUpper dialog
    vbox  <- vBoxNew False 0
    boxPackStart upbox vbox PackGrow 0

    (method, saveSomeHistories)  <-  methodPage fm' dialog okButton vbox
    method =:: getMethod

    widgetShowAll dialog
    choice <- fmDialogRun fm' dialog dialogName
    when (choice==ResponseOk) $ do
      saveSomeHistories
      val method >>= setMethod
      return ()


----------------------------------------------------------------------------------------------------
---- Закладка сжатия -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

compressionPage fm' vbox = do
  let pack x = boxPackStart vbox x PackNatural 1
  -- Алгоритм сжатия.
  (hbox, cmethod) <- fmLabeledEntryWithHistory fm' "compression" "0175 Compression profile:";  pack hbox
  ; save <- button "0178 Save";  boxPackStart hbox (widget save) PackNatural 5

  -- Put all subsequent controls into scrolled window
  vbox <- createScroller vbox
  let pack x = boxPackStart vbox x PackNatural 1

  -- Настройки алгоритма сжатия.
  hbox <- hBoxNew False 0;  pack hbox
  ; method     <- radioFrame "0107 Compression level" levels;    boxPackStart hbox (widget method)  PackNatural 0
  ; methodText <- labelNew Nothing;                              boxPackStart hbox methodText       PackGrow 0

  table <- tableNew 3 2 False;  pack table;  tableSetColSpacings table 20
  ; dMethod    <- checkBox "0509 Fast decompression"                                              ;  packTable table (widget dMethod)    0 0
  ; xMethod    <- checkBox "0510 Low-memory decompression"                                        ;  packTable table (widget xMethod)    0 1
  ; maxSolid   <- checkBox "0512 Maximize solid blocks"                                           ;  packTable table (widget maxSolid)   1 0
  ; autodetect <- checkBox "0176 Filetype auto-detection"                                         ;  packTable table (widget autodetect) 1 1
  ; (hbox, numThreadsOn, numThreads) <- fmCheckedEntryWithHistory fm' "numthreads" "0531 Threads:";  packTable table hbox 0 2;  widgetSetSizeRequest (widget numThreads) 50 (-1)

  -- Настройки размера солид-блока
  table <- createFrame "0177 Limit solid blocks" pack (tableNew 1 3 False);  tableSetColSpacings table 20
  ; (hbox, solidBytesOn, solidBytes) <- fmCheckedEntryWithHistory fm' "bytes" "0528 Bytes:"       ;  packTable table hbox 0 0;  widgetSetSizeRequest (widget solidBytes) 70 (-1)
  ; (hbox, solidFilesOn, solidFiles) <- fmCheckedEntryWithHistory fm' "files" "0529 Files:"       ;  packTable table hbox 1 0;  widgetSetSizeRequest (widget solidFiles) 70 (-1)
  ; solidByExtension                 <- checkBox                              "0530 Extension"    ;  packTable table (widget solidByExtension) 2 0

  -- Ограничение используемой памяти
  table <- createFrame "0513 Limit memory usage, mb" pack (tableNew 1 2 False);  tableSetColSpacings table 30
  ; (hbox, limitCMemOn, limitCMem) <- fmCheckedEntryWithHistory fm' "cmem" "0514 Compression:"    ;  packTable table hbox 0 0;  widgetSetSizeRequest (widget limitCMem) 60 (-1)
  ; (hbox, limitDMemOn, limitDMem) <- fmCheckedEntryWithHistory fm' "dmem" "0515 Decompression:"  ;  packTable table hbox 1 0;  widgetSetSizeRequest (widget limitDMem) 60 (-1)
  ; maxBlock <- getMaxBlockToAlloc;  s <- i18n"0461 Largest address space block:";
  ; l <- labelNew Nothing; labelSetMarkup l (s++" "++bold(showMem (maxBlock `roundDown` mb)))
  ; tableAttachDefaults table l 0 2 1 2

  -- Следующие две таблицы надо разместить бок-о-бок
  hbox <- hBoxNew False 0;  pack hbox;  let pack_horizontally x = boxPackStart hbox x PackGrow 1

  -- Отключение отдельных фильтров/групп файлов
  table <- createFrame "0516 Disable filter/group" pack_horizontally (tableNew 3 3 False)
  let algos  =  words "rep exe delta dict lzp $text $wav $bmp $compressed"
  disabledAlgos <- foreach (zip [0..] algos) $ \(i,algo) -> do
    disabledAlgo <- checkBox$ (left_fill '0' 4 (show$ i+517))++" "++algo   -- create checkbox with tooltip: "0517 rep"..."0525 $compressed"
    packTable table (widget disabledAlgo) (i `div` 3) (i `mod` 3)          -- position it inside 3x3 table
    return disabledAlgo

  -- Продвинутые/экспериментальные методы
  table <- createFrame "0534 Experimental algorithms" pack_horizontally (tableNew 3 2 False)
  ; lzma1g  <- checkBox "0535 lzma:1gb";  packTable table (widget lzma1g ) 0 0
  ; exe2    <- checkBox "0536 exe2"    ;  packTable table (widget exe2   ) 0 1
  ; srep    <- checkBox "0537 srep"    ;  packTable table (widget srep   ) 0 2
  ; precomp <- checkBox "0538 precomp" ;  precomp_table <- tableNew 2 1 False
  ; intense <- checkBox "0539 intense" ;  packTable precomp_table (widget intense) 0 0
  ; jpeg    <- checkBox "0540 jpeg"    ;  packTable precomp_table (widget jpeg   ) 0 1
  ; frame <- frameNew;  tableAttachDefaults table frame 1 2 0 3
  ; set frame [containerChild := precomp_table, frameLabelWidget := widget precomp, containerBorderWidth := 5]


  -- Инициализация полей: -m4 -ma+
  let m=4
  method     =: (6-m) .$ clipTo 0 5
  autodetect =: True

  -- Опубликовать описание первоначально выбранного метода сжатия и обновлять его при изменениях настроек
  let parsePhysMem = parseMemWithPercents (toInteger getPhysicalMemory `roundTo` (4*mb))
  let describeMethod = do
        -- Сначала поменяем видимость контролов
        widgetSetSensitivity precomp_table =<< val precomp
        m <- val method
        d <- val dMethod
        x <- val xMethod
        let simpleMethod = show(if m==0 then 9 else 6-m) ++ (x&&&"x" ||| (d&&&"d"))
        let compressor = simpleMethod.$ decode_method 1 []
                                     .$ limitCompressionMem   (parsePhysMem "75%")
                                     .$ limitDecompressionMem (1*gb)
            cmem = compressor.$ getCompressionMem
            dmem = compressor.$ getMinDecompressionMem
        let level  =         "      ccm     uharc      7-zip        rar       bzip2      zip"
            cspeed =         "    5mb/s  7-20mb/s     30mb/s     50mb/s     100mb/s  400mb/s" --m9,m5..m1
            dspeed = d.$bool " 7-50mb/s    40mb/s 50-200mb/s 50-200mb/s 100-500mb/s  600mb/s" --m9,m5..m1
                             "   50mb/s    50mb/s    200mb/s    200mb/s     500mb/s  600mb/s" --m9x,m5x..m1x
        labelSetMarkup methodText . deleteIf (=='_') . unlines =<< mapM i18fmt
            [ ["0114 Compression level: %1",               bold((words level!!m).$replace '_' ' ')]
            , ["0115 Compression speed: %1, memory: %2",   bold(words cspeed!!m), bold(showMem cmem)]
            , ["0116 Decompression speed: %1, memory: %2", bold(words dspeed!!m), bold(showMem dmem)]
            , [""]
            , ["0526 All speeds were measured on i7-2600"]]
        w1 <- i18n (levels!!m)
        w2 <- i18n "0226 (fast, low-memory decompression)"
        w3 <- i18n "0511 (fast decompression)"
        autodetect'       <- val autodetect
        numThreadsOn'     <- val numThreadsOn
        numThreads'       <- val numThreads
        maxSolid'         <- val maxSolid
        solidBytesOn'     <- val solidBytesOn
        solidBytes'       <- val solidBytes
        solidFilesOn'     <- val solidFilesOn
        solidFiles'       <- val solidFiles
        solidByExtension' <- val solidByExtension
        limitCMemOn'      <- val limitCMemOn
        limitCMem'        <- val limitCMem
        limitDMemOn'      <- val limitDMemOn
        limitDMem'        <- val limitDMem
        disabledList      <- foreach (zip algos disabledAlgos) $ \(algo,disabledAlgo) -> do
                               disabled <- val disabledAlgo
                               return (disabled &&& ("-mc-"++algo))
        lzma1g'           <- val lzma1g
        exe2'             <- val exe2
        srep'             <- val srep
        precomp'          <- val precomp
        intense'          <- val intense
        jpeg'             <- val jpeg
        let s = (maxSolid'         &&&  ";")++
                (solidBytesOn'     &&&  clear solidBytes')++
                (solidFilesOn'     &&& (clear solidFiles'++"f"))++
                (solidByExtension' &&&  "e")
        cmethod =: w1++(x&&&" "++w2 ||| (d&&&" "++w3))++": "++unwords(["-m"++(simpleMethod.$changeTo [("9","x")])]
                     ++(not autodetect' &&& ["-ma-"])
                     ++(numThreadsOn'   &&& ["-mt"++clear numThreads'])
                     ++(s               &&& ["-s" ++s])
                     ++(limitCMemOn'    &&& ["-lc"++clear limitCMem'])
                     ++(limitDMemOn'    &&& ["-ld"++clear limitDMem'])
                     ++(filter (not.null) disabledList)
                     ++(lzma1g'         &&& ["-mc:lzma/lzma:max:512mb"])
                     ++(exe2'           &&& ["-mc:exe/exe2"])
                     ++(srep'           &&& ["-mc:rep/maxsrep"])
                     ++(precomp'        &&& ["-mc$default,$obj:+"++(intense' &&& "max")++"precomp"++(jpeg' &&& "j")]))
  --
  describeMethod
  describeMethod .$ setOnUpdate method
  describeMethod .$ setOnUpdate maxSolid
  describeMethod .$ setOnUpdate autodetect
  describeMethod .$ setOnUpdate numThreadsOn
  describeMethod .$ setOnUpdate numThreads
  describeMethod .$ setOnUpdate limitCMemOn
  describeMethod .$ setOnUpdate limitCMem
  describeMethod .$ setOnUpdate limitDMemOn
  describeMethod .$ setOnUpdate limitDMem
  describeMethod .$ setOnUpdate solidBytesOn
  describeMethod .$ setOnUpdate solidBytes
  describeMethod .$ setOnUpdate solidFilesOn
  describeMethod .$ setOnUpdate solidFiles
  describeMethod .$ setOnUpdate solidByExtension
  for (lzma1g:exe2:srep:precomp:intense:jpeg:disabledAlgos) (`setOnUpdate` describeMethod)
  setOnUpdate dMethod $ do unlessM (val dMethod) (xMethod =: False);  describeMethod   -- disable 'x' if 'd' was disabled
  setOnUpdate xMethod $ do whenM   (val xMethod) (dMethod =: True);   describeMethod   -- enable  'd' if 'x' was enabled

  -- Сохранение истории строковых полей и обработка нажатия на Save
  let saveHistories = do
        whenM (val numThreadsOn) $ do saveHistory numThreads
        whenM (val limitCMemOn)  $ do saveHistory limitCMem
        whenM (val limitDMemOn)  $ do saveHistory limitDMem
        whenM (val solidBytesOn) $ do saveHistory solidBytes
        whenM (val solidFilesOn) $ do saveHistory solidFiles
  save `onClick` (saveHistories >> saveHistory cmethod)

  -- Возвратим виджет строки ввода метода сжатия и процедуру, которую нужно выполнить при нажатии на OK в диалоге
  return (cmethod, saveHistories)


{-
    let simpleMethod = initSetting "simpleMethod" `defaultVal` "4"
        m =  case (take 1 simpleMethod) of [d] | isDigit d -> digitToInt d
                                           _               -> 4
        x =  drop 1 simpleMethod == "x"

      ; solidBytes'        <- val solidBytes;  solidBytes' !~ "*b"   &&&   solidBytes =: solidBytes'++"b"
      simpleMethod' <- getSimpleMethod
-}

-- |Compression level names
levels = [ "0108 Maximum",
           "0109 High",
           "0110 Normal",
           "0111 Fast",
           "0127 HDD-speed",
           "0527 Instant"]


----------------------------------------------------------------------------------------------------
---- Закладка шифрования -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

encryptionPage fm' dialog okButton vbox = do
  let pack x = boxPackStart vbox x PackNatural 1
  (hbox, pwds)  <-  pwdBox 2;  pack hbox   -- Создаёт таблицу с полями для ввода двух паролей

  -- Фрейм шифрования.
  vbox1 <- vBoxNew False 0
  frame <- frameNew;  s <- i18n"0119 Encryption"
  set frame [containerChild := vbox1, frameLabel := s, containerBorderWidth := 5]
  let pack1 x = boxPackStart vbox1 x PackNatural 1
  boxPackStart vbox frame PackNatural 10

  -- Алгоритм шифрования.
  (hbox, method) <- fmLabeledEntryWithHistory fm' "encryption" "0179 Encryption profile:";  pack1 hbox
  ; save <- button "0180 Save";  boxPackStart hbox (widget save) PackNatural 0

  -- Настройки шифрования.
  encryptHeaders <- checkBox "0120 Encrypt archive directory";  pack1 (widget encryptHeaders)
  usePwd <- checkBox "0181 Use password";  pack1 (widget usePwd)
  (hbox, keyfileOn, keyfile) <- fmFileBox fm' dialog
                                          "akeyfile" FileChooserActionOpen
                                (checkBox "0123 Keyfile:")
                                          "0124 Select keyfile"
                                          aANYFILE_FILTER
                                          (const$ return True)
                                          (fmCanonicalizeDiskPath fm')
  ; createKeyfile <- button "0125 Create"
  ; boxPackStart hbox (widget createKeyfile) PackNatural 0;  pack1 hbox
  (hbox, encAlg) <- fmLabeledEntryWithHistory fm' "encryptor" "0121 Encryption algorithm:";  pack1 hbox

  -- Настройки расшифровки
  (decryption, decryptionOnOK) <- decryptionBox fm' dialog
  ; boxPackStart vbox decryption PackNatural 10

  -- Разрешаем нажать OK только если оба введённых пароля одинаковы
  let [pwd1,pwd2] = pwds
  for pwds $ flip afterKeyRelease $ \e -> do
    [pwd1', pwd2'] <- mapM val pwds
    okButton `widgetSetSensitivity` (pwd1'==pwd2')
    return False

  -- Создать новый файл-ключ, записав криптографически случайные данные в указанный пользователем файл
  createKeyfile `onClick` do
    let default_keyfile = do fm <- val fm'; return$ fm_curdir fm </> "new.key"
    chooseFile dialog FileChooserActionSave "0126 Create new keyfile" aANYFILE_FILTER default_keyfile $ \filename -> do
      --to do: fileChooserSetDoOverwriteConfirmation chooserDialog True
      filePutBinary filename =<< generateRandomBytes 1024
      keyfile   =: filename
      keyfileOn =: True

  -- Инициализация: прочитаем пароли из глобальных переменных
  pwd1 =:: val encryptionPassword
  pwd2 =:: val encryptionPassword

  -- Сохранение истории строковых полей и обработка нажатия на Save
  let saveHistories = do
        whenM (val keyfileOn) $ do saveHistory keyfile
        saveHistory encAlg
  --
  save `onClick` (saveHistories >> saveHistory method)

  -- Действия, выполняемые при нажатии кнопки OK
  let onOK = do
        saveHistories
        encryptionPassword =:: val pwd1
        decryptionOnOK

  -- Формирует профиль шифрования и вызывается при изменении любых опций в этом фрейме
  let makeProfile = do
        usePwd'         <- val usePwd
        keyfileOn'      <- val keyfileOn
        keyfile'        <- val keyfile
        encAlg'         <- val encAlg
        encryptHeaders' <- val encryptHeaders
        method =: unwords( (encryptHeaders' &&& ["-hp"])++
                           (usePwd'         &&& ["-p?"])++
                                                ["--encryption="++clear encAlg']++
                           (keyfileOn'      &&& ["--keyfile="   ++clear keyfile']))
  --
  makeProfile
  makeProfile .$ setOnUpdate usePwd
  makeProfile .$ setOnUpdate keyfileOn
  makeProfile .$ setOnUpdate keyfile
  makeProfile .$ setOnUpdate encAlg
  makeProfile .$ setOnUpdate encryptHeaders

  -- Возвратим метод назначения реакции на изменение настроек шифрования и процедуру, выполняемую при нажатии на OK
  return (method, onOK)


----------------------------------------------------------------------------------------------------
---- Фрейм расшифровки -----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

decryptionBox fm' dialog = do
  vbox <- vBoxNew False 0
  decryptionFrame <- frameNew;  s <- i18n"0144 Decryption"
  set decryptionFrame [containerChild := vbox, frameLabel := s, containerBorderWidth := 5]

  lbl <- label "0074 Enter password:"
  pwd <- entryNew   --newTextViewWithText
  ; set pwd [entryVisibility := False, entryActivatesDefault := True]
  (keyfileBox, _, keyfile) <- fmFileBox fm' dialog
                                        "keyfile" FileChooserActionOpen
                                 (label "0123 Keyfile:")
                                        "0124 Select keyfile"
                                        aANYFILE_FILTER
                                        (const$ return True)
                                        (fmCanonicalizeDiskPath fm')
  hbox <- hBoxNew False 0
  ; boxPackStart hbox (widget lbl) PackNatural 0
  ; boxPackStart hbox pwd          PackGrow    5
  boxPackStart vbox hbox       PackNatural 0
  boxPackStart vbox keyfileBox PackNatural 5
  -- Прочитаем пароли из глобальных переменных
  pwd =:: val decryptionPassword
  -- Действие, выполняемое при нажатии на OK. Возвращает опции, которые нужно добавить к командной строке
  let onOK = do decryptionPassword =:: val pwd
                saveHistory keyfile
                fmGetDecryptionOptions fm'
  --
  return (decryptionFrame, onOK)


----------------------------------------------------------------------------------------------------
---- Вспомогательные определения -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Режимы диалогов
data DialogMode = EncryptionMode | ProtectionMode | RecompressMode | CommentMode | MakeSFXMode | NoMode  deriving Eq

-- |Определяет то, как имена каталогов подставляются в команды
addCmdFiles dirname =  [dirname++[pathSeparator]]
xCmdFiles   dirname =  [dirname++[pathSeparator]++"*"]
dCmdFiles   dirname =  [dirname, dirname++[pathSeparator]++"*"]

-- |Выполнить операцию над текущим архивом/всеми отмеченными файлами на диске
compressionOperation fm' action exec winoptions cmd mode = do
  fm <- val fm'
  files <- if isFM_Archive fm then return [fm_arcname fm]
                              else getSelection fm' addCmdFiles  -- todo: j/ch когда Selection включает каталоги
  action fm' exec winoptions cmd files mode

-- |Выполнить операцию над выбранными файлами в архиве/всеми файлами в выбранных архивах
archiveOperation fm' action = do
  fm <- val fm'
  files <- getSelection fm' (if isFM_Archive fm  then xCmdFiles  else const [])
  if isFM_Archive fm
    then action [fm_arcname fm] (fm_arcdir fm) files
    else do fullnames <- mapM (fmCanonicalizePath fm') files
            action fullnames "" []

-- |Выполняет операцию, которой нужно передать только имена архивов
multiArchiveOperation fm' action = do
  fm <- val fm'
  if isFM_Archive fm
    then action [fm_arcname fm]
    else do files <- getSelection fm' (const [])
            fullnames <- mapM (fmCanonicalizePath fm') files
            action fullnames

-- |Обновить содержимое панели файл-менеджера актуальными данными
refreshCommand fm' = do
  fm <- val fm'
  curfile <- fmGetCursor fm'
  selected <- getSelection fm' (:[])
  -- Обновим содержимое каталога/архива и восстановим текущий файл и список отмеченных
  closeFMArc fm'
  fmChdir fm' (fm_current fm)
  when (selected>[]) $ do
    fmSetCursor fm' curfile
  fmUnselectAll fm'
  fmSelectFilenames fm' ((`elem` selected).fmname)

-- |Просмотреть файл
runViewCommand = runEditCommand

-- |Редактировать файл
runEditCommand filename = run (iif isWindows "notepad" "gedit") [filename]
  where run cmd params = forkIO (rawSystem cmd params >> return ()) >> return ()
  -- edit filename | isWindows && takeExtension filename == "txt"  =  todo: direct shell open command

-- |Поместим все контролы в симпатичный notebook и возвратим процедуру создания новых страниц в нём
startNotebook dialog = do
  upbox <- dialogGetUpper dialog
  nb <- notebookNew;  boxPackStart upbox nb PackGrow 0
  let newPage name = do hbox <- hBoxNew False 0; notebookAppendPage nb hbox =<< i18n name
                        vbox <- vBoxNew False 0; boxPackStart hbox vbox PackGrow 5
                        return vbox
  return (nb,newPage)

-- |Берёт обычный vbox и возвращает vbox внутри скроллера
createScroller vbox = do
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  boxPackStart vbox scrolledWindow PackGrow 1
  vbox <- vBoxNew False 0
  scrolledWindowAddWithViewport scrolledWindow vbox
  scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAutomatic
  Just viewport <- binGetChild scrolledWindow
  viewportSetShadowType (castToViewport viewport) ShadowNone
  return vbox

-- |Создаёт рамку с заданным контролом внутри
createFrame label pack makeControl = do
  frame <- frameNew;  pack frame;  s <- i18n label;  control <- makeControl
  set frame [containerChild := control, frameLabel := s, containerBorderWidth := 5]
  return control

-- |Помещает контрол w в таблицу t на клетку x,y
packTable t w x y  =  tableAttachDefaults t w x (x+1) y (y+1)


-- |Выполнить операцию с использованием временного файла, куда записываются данные contents
withTempFile contents = withTemporary (`filePutBinary` contents) fileRemove

-- |Выполнить операцию с использованием временного каталога
withTempDir = withTemporary createDirectoryHierarchy (dirRemoveRecursive forcedFileRemove)

-- |Выполнить операцию с использованием временного файла/каталога
withTemporary preAction postAction action = do
  tempDir <- getTempDir
  createDirectoryHierarchy tempDir
  fix $ \tryNext -> do n <- generateRandomBytes 4 >>== encode16
                       let tempname = tempDir </> ("freearc"++n++".tmp")
                       e <- fileOrDirExist tempname
                       if e then tryNext
                            else do preAction tempname
                                    ensureCtrlBreak "remove temporary files" (ignoreErrors$ postAction tempname) $ do
                                      action tempname

-- |Найти все временные файлы, созданные FreeArc
findTempFiles = do
  tempDir <- getTempDir
  files <- dir_list tempDir
  return (files .$filter (isFreeArcTemporary.baseName) .$map diskName)

-- |Check that it's "temporary" FreeArc instance - either current directory or any argument is inside tempdir
isTemporaryInstance args = do
  curdir  <- getCurrentDirectory
  tempDir <- getTempDir
  return$ any (`isInsideDir` tempDir) (curdir:args)

-- |Check that `filename` is inside temporary directory
isTempFile filename = do
  tempDir <- getTempDir
  return (filename `isInsideDir` tempDir)

-- |Check that 'file' is inside `dir`
file `isInsideDir` dir =
  splitDirectories dir  `isPrefixOf`  splitDirectories file

-- |Шаблон имени всех временных файлов, создаваемых FreeArc
isFreeArcTemporary  =  (~= "freearc*.tmp")
