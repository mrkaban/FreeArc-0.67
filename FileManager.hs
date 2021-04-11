{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager                                                                  ------
----------------------------------------------------------------------------------------------------
module FileManager where

import Prelude hiding (catch)
import Control.Concurrent
import Control.OldException
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Numeric hiding (readInt)
import System.IO.Unsafe
import System.Cmd
import System.Process
#if defined(FREEARC_WIN)
import System.Win32
import Foreign.Ptr
#endif

import Graphics.UI.Gtk   -- for gtk2hs 0.11: hiding (eventKeyName, eventModifier, eventWindowState, eventButton, eventClick)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets
import Compression
import Encryption
import Options
import Cmdline
import UI
import Arhive7zLib
import ArhiveStructure
import ArhiveDirectory
import ArcExtract
import FileManPanel
import FileManUtils
import FileManDialogs
import FileManDialogAdd

-- |Maximum size of command line
aMAX_CMDLINE_LENGTH = 32000 `div` 4

----------------------------------------------------------------------------------------------------
---- Обработка GUI-специфичных вариаций командной строки -------------------------------------------
----------------------------------------------------------------------------------------------------

parseGUIcommands run args exec = do
  let extract fm' exec cmd arcnames = extractDialog fm' exec [] cmd arcnames "" []
      add     fm' exec cmd files    = addDialog     fm' exec [] cmd files NoMode
  loadTranslation
  case args of
    ["--settings-dialog"] -> openSettingsDialog                   -- Диалог настроек
    "--add-dialog":xs     -> openDialog xs exec add               -- Диалог упаковки
    "--extract-dialog":xs -> openDialog xs exec extract           -- Диалог распаковки
    ["--register"]        -> changeRegisterShellExtensions args   -- Регистрация в Explorer
    ["--unregister"]      -> changeRegisterShellExtensions args   -- Удаление регистрации в Explorer
    []                    -> myGUI run args                       -- При вызове программы без аргументов или с одним аргументом (именем каталога/архива)
    [_]                   -> myGUI run args                       --   запускаем полноценный Archive Manager
    _                     -> startGUI >> exec args                --   а иначе - просто отрабатываем команды (де)архивации

-- Диалог настроек
openSettingsDialog = do
  startGUI
  gui $ do
    fm' <- newEmptyFM
    settingsDialog fm'
    mainQuit

-- Открыть диалог (рас)паковки и затем выполнить запрошенную команду
openDialog (cmd:"--":params) exec dialog = do
  startGUI
  cmdChan <- newChan
  gui $ do
    let exec _bgmode _use_winrar postAction cmds  =  writeChan cmdChan (cmds,postAction)
    fm' <- newEmptyFM
    dialog fm' exec cmd params
    writeChan cmdChan ([],doNothing)  -- пустая команда на случай если пользователь нажал Cancel
  --
  (cmds,postAction) <- readChan cmdChan
  cmds &&& do
    exec$ joinWith [";"] cmds
    postAction True

openDialog params exec dialog = do
  startGUI
  gui $ debugMsg "FileManager.hs: attempt to run dialog with incorrect parameters to command"


----------------------------------------------------------------------------------------------------
---- Главное меню программы и тулбар под ним -------------------------------------------------------
----------------------------------------------------------------------------------------------------
--      File: New Archive, Open Archive, New SFX, Change Drive, Select All, Select Group, Deselect Group, Invert Selection
--      Commands (или Actions): Add, Extract, Test, ArcInfo, View, Delete, Rename
--      Tools: Wizard (если таковой будет), Protect, Comment, Convert to EXE, Encrypt, Add Recovery record, Repair
--      Options: Configuration, Save settings, Load settings, View log, Clear log
--      Help: собственно сам Help, Goto Homepage (и/или Check for update), About

uiDef =
  "<ui>"++
  "  <menubar>"++
  "    <menu name=\"File\"     action=\"FileAction\">"++
  "      <menuitem name=\"OpenArchive\"        action=\"OpenArchiveAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Select all\"         action=\"SelectAllAction\" />"++
  "      <menuitem name=\"Select\"             action=\"SelectAction\" />"++
  "      <menuitem name=\"Unselect\"           action=\"UnselectAction\" />"++
  "      <menuitem name=\"Invert selection\"   action=\"InvertSelectionAction\" />"++
  "      <menuitem name=\"Refresh\"            action=\"RefreshAction\" />"++
  "      <separator/>"++
  "      <placeholder name=\"FileMenuAdditions\" />"++
  "      <menuitem name=\"Exit\"               action=\"ExitAction\"/>"++
  "    </menu>"++
  "    <menu name=\"Commands\" action=\"CommandsAction\">"++
  "      <menuitem name=\"Add\"                action=\"AddAction\" />"++
  "      <menuitem name=\"Extract\"            action=\"ExtractAction\" />"++
  "      <menuitem name=\"Test\"               action=\"TestAction\" />"++
  "      <menuitem name=\"Info\"               action=\"InfoAction\" />"++
  "      <menuitem name=\"Delete\"             action=\"DeleteAction\" />"++
  "    </menu>"++
#ifndef HAMSTER
  "    <menu name=\"Tools\"    action=\"ToolsAction\">"++
  "      <menuitem name=\"Lock\"               action=\"LockAction\" />"++
  "      <menuitem name=\"Comment\"            action=\"CommentAction\" />"++
  "      <menuitem name=\"Recompress\"         action=\"RecompressAction\" />"++
  "      <menuitem name=\"Convert to SFX\"     action=\"ConvertToSFXAction\" />"++
#if defined(FREEARC_WIN)
  "      <menuitem name=\"Convert to FreeArc\" action=\"ConvertToFreeArcAction\" />"++
#endif
  "      <separator/>"++
  "      <menuitem name=\"Encrypt\"            action=\"EncryptAction\" />"++
  "      <menuitem name=\"Protect\"            action=\"ProtectAction\" />"++
  "      <menuitem name=\"Repair\"             action=\"RepairAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Modify\"             action=\"ModifyAction\" />"++
  "      <menuitem name=\"Join archives\"      action=\"JoinArchivesAction\" />"++
  "    </menu>"++
#endif
  "    <menu name=\"Options\"  action=\"OptionsAction\">"++
  "      <menuitem name=\"Settings\"           action=\"SettingsAction\" />"++
  "      <menuitem name=\"Change skin\"        action=\"ChangeSkinAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"ViewLog\"            action=\"ViewLogAction\" />"++
  "      <menuitem name=\"ClearLog\"           action=\"ClearLogAction\" />"++
  "    </menu>"++
  "    <menu name=\"Help\"     action=\"HelpAction\">"++
  "      <menuitem name=\"MainHelp\"           action=\"MainHelpAction\" />"++
  "      <separator/>"++
#ifndef HAMSTER
  "      <menuitem name=\"CmdlineHelp\"        action=\"CmdlineHelpAction\" />"++
#endif
  "      <menuitem name=\"OpenHomepage\"       action=\"OpenHomepageAction\" />"++
  "      <menuitem name=\"OpenForum\"          action=\"OpenForumAction\" />"++
  "      <menuitem name=\"CheckForUpdate\"     action=\"CheckForUpdateAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"About\"              action=\"AboutAction\" />"++
  "    </menu>"++
  "  </menubar>"++
  "  <toolbar>"++
  "    <placeholder name=\"FileToolItems\">"++
  "      <toolitem name=\"OpenArchive\"        action=\"OpenArchiveAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Add\"                action=\"AddAction\" />"++
  "      <toolitem name=\"Extract\"            action=\"ExtractAction\" />"++
  "      <toolitem name=\"Test\"               action=\"TestAction\" />"++
  "      <toolitem name=\"Info\"               action=\"InfoAction\" />"++
  "      <toolitem name=\"Delete\"             action=\"DeleteAction\" />"++
  "      <separator/>"++
#ifndef HAMSTER
  "      <toolitem name=\"Lock\"               action=\"LockAction\" />"++
  "      <toolitem name=\"Recompress\"         action=\"RecompressAction\" />"++
  "      <toolitem name=\"Convert to SFX\"     action=\"ConvertToSFXAction\" />"++
  "      <toolitem name=\"Join archives\"      action=\"JoinArchivesAction\" />"++
  "      <separator/>"++
#endif
  "      <toolitem name=\"Refresh\"            action=\"RefreshAction\" />"++
  "    </placeholder>"++
  "  </toolbar>"++
  "</ui>"


----------------------------------------------------------------------------------------------------
---- Визуальная часть файл-менеджера ---------------------------------------------------------------
----------------------------------------------------------------------------------------------------

myGUI run args = do
  fileManagerMode =: True
  runGUI $ do
  cmds <- parseCmdline ["l", "a"]   -- инициализация: display, логфайл...
  for cmds cmd_setup_command        --   ... tempdir
  -- Список ассоциаций клавиша->действие
  onKeyActions <- newList
  let onKey = curry (onKeyActions <<=)
  -- Создадим окно индикатора прогресса и загрузим настройки/локализацию
  (windowProgress, (clearMessageBox,showMessageBox)) <- runIndicators
  -- Main menu
  standardGroup <- actionGroupNew "standard"
  let action name  =  (concat$ map (mapHead toUpper)$ words$ i18no name)++"Action"   -- "9999 the name" -> "TheNameAction"
  let names = split ',' "0050 File,0258 Commands,0259 Tools,0260 Options,0261 Help"
  for names $ \name -> do
    label <- i18n name
    actionGroupAddAction standardGroup  =<<  actionNew (action name) label Nothing Nothing
  -- Menus and toolbars
  let anew name comment icon accel = do
        [i18name,i18comment] <- i18ns [name,comment]
        action <- actionNew (action name) i18comment (Just i18comment) icon
        action `set` [actionShortLabel := i18name]
        actionGroupAddActionWithAccel standardGroup action (Just accel)
        accel `onKey` actionActivate action
        return action
  --
  openAct     <- anew "0262 Open archive"        "0265 Open archive"                              (Just stockOpen)            "<Alt>O"
  selectAllAct<- anew "0263 Select all"          "0290 Select all files"                          (Just stockSelectAll)       "<Ctrl>A"
  selectAct   <- anew "0037 Select"              "0047 Select files"                              (Just stockAdd)             "KP_Add"
  unselectAct <- anew "0038 Unselect"            "0048 Unselect files"                            (Just stockRemove)          "KP_Subtract"
  invertSelAct<- anew "0264 Invert selection"    "0291 Invert selection"                          (Nothing)                   "KP_Multiply"
  refreshAct  <- anew "0039 Refresh"             "0049 Reread archive/directory"                  (Just stockRefresh)         "F5"
  exitAct     <- anew "0036 Exit"                "0046 Quit application"                          (Just stockQuit)            "<Alt>Q"

  addAct      <- anew "0030 Add"                 "0040 Add files to archive(s)"                   (Just stockMediaRecord)     "<Alt>A"
  extractAct  <- anew "0035 Extract"             "0045 Extract files from archive(s)"             (Just stockMediaPlay)       "<Alt>E"
  testAct     <- anew "0034 Test"                "0044 Test files in archive(s)"                  (Just stockSpellCheck)      "<Alt>T"
  arcinfoAct  <- anew "0086 Info"                "0087 Information about archive"                 (Just stockInfo)            "<Alt>I"
  deleteAct   <- anew "0033 Delete"              "0043 Delete files (from archive)"               (Just stockDelete)          "Delete"

  lockAct     <- anew "0266 Lock"                "0267 Lock archive from further changes"         (Just stockDialogAuthentication) "<Alt>L"
  commentAct  <- anew "0268 Comment"             "0269 Edit archive comment"                      (Just stockEdit)            "<Alt>C"
  recompressAct<-anew "0293 Recompress"          "0294 Recompress files in archive"               (Just stockGotoBottom)      "<Alt>R"
  toSfxAct    <- anew "0270 Convert to SFX"      "0271 Convert archive to SFX"                    (Just stockConvert)         "<Alt>S"
  toFaAct     <- anew("0426 Convert to "++aFreeArc) ("0427 Convert foreign archive to "++aFreeArc++" format") (Nothing)       ""
  encryptAct  <- anew "0272 Encrypt"             "0273 Encrypt archive contents"                  (Nothing)                   ""
  addRrAct    <- anew "0274 Protect"             "0275 Add Recovery record to archive"            (Nothing)                   "<Alt>P"
  repairAct   <- anew "0379 Repair"              "0380 Repair damaged archive"                    (Nothing)                   ""
  modifyAct   <- anew "0031 Modify"              "0041 Modify archive(s)"                         (Just stockEdit)            "<Alt>M"
  joinAct     <- anew "0032 Join archives"       "0042 Join archives together"                    (Just stockCopy)            "<Alt>J"

  settingsAct <- anew "0064 Settings"            "0065 Edit program settings"                     (Just stockPreferences)     ""
  changeSkinAct<-anew "0487 Change skin"         "0488 Change program skin"                       (Nothing)                   ""
  viewLogAct  <- anew "0276 View log"            "0277 View logfile"                              (Nothing)                   ""
  clearLogAct <- anew "0278 Clear log"           "0279 Clear logfile"                             (Nothing)                   ""

  helpAct     <- anew "0280 Main help"          ("0281 Help on using "++aFreeArc)                        (Just stockHelp)            "F1"
  helpCmdAct  <- anew "0282 Cmdline help"       ("0283 Help on "++aFreeArc++" command line")             (Just stockHelp)            ""
  homepageAct <- anew "0284 Open Homepage"       "0285 Open program site"                         (Just stockHome)            ""
  openForumAct<- anew "0373 Open forum"          "0374 Open program forum"                        (Nothing)                   ""
  openWikiAct <- anew "0375 Open wiki"           "0376 Open program wiki"                         (Nothing)                   ""
  whatsnewAct <- anew "0286 Check for update"    "0287 Check for new program version"             (Just stockDialogInfo)      ""
  aboutAct    <- anew "0288 About"               "0289 About"                                     (Just stockAbout)           ""

  menufile <- findFile configFilePlaces aMENU_FILE
  uiData   <- if menufile>""  then fileGetBinary menufile  else return uiDef

  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiData
  uiManagerInsertActionGroup ui standardGroup 0

  window <- windowNew
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
  (Just toolBar) <- uiManagerGetWidget ui "/ui/toolbar"

  (listUI, listView, listModel, listSelection, columns, onColumnTitleClicked) <- createFilePanel
  statusLabel  <- labelNew Nothing
  miscSetAlignment statusLabel 0 0.5
  messageCombo <- New.comboBoxNewText
  statusbar    <- statusbarNew
  ctx <- statusbarGetContextId statusbar ""
  statusbarPush statusbar ctx "    "
  widgetSetSizeRequest messageCombo 30 (-1)
  lowBox <- vBoxNew False 0
  boxPackStart lowBox statusLabel  PackNatural 2
  boxPackStart lowBox messageCombo PackGrow    2
  --boxPackStart lowBox statusbar    PackNatural 0

  -- Создадим переменную для хранения текущего состояния файл-менеджера
  fm' <- newFM window listView listModel listSelection statusLabel messageCombo

  -- Configure grid lines in the filelist
  gridLines <- fmGetHistoryBool fm' "HorizontalGridLines" False
  set listView [treeViewEnableGridLines := if gridLines then TreeViewGridLinesHorizontal else TreeViewGridLinesNone]

  -- Отрихтуем тулбар
  let toolbar = castToToolbar toolBar
  toolbarCaptions <- fmGetHistoryBool fm' "ToolbarCaptions" True
  toolbar `set` [toolbarStyle := if toolbarCaptions then ToolbarBoth else ToolbarIcons]
  toolbar `toolbarSetIconSize` IconSizeLargeToolbar
  n <- toolbarGetNItems toolbar
  for [0..n-1] $ \i -> do
    Just button <- toolbarGetNthItem toolbar i
    toolItemSetHomogeneous button False

  -- Полоска навигации
  naviBar  <- hBoxNew False 0
  upButton <- button "0006   Up  "
  curdir   <- fmEntryWithHistory fm' "dir/arcname" (const$ return True) (fmCanonicalizePath fm')
  saveDirButton <- button "0007   Save  "
  boxPackStart naviBar (widget upButton)       PackNatural 0
#if defined(FREEARC_WIN)
  -- Меню выбора диска
  driveButton <- button "C:"
  driveMenu   <- makePopupMenu (fmChdir fm'.(++"\\").head.words) =<< getDrives
  driveButton `onClick` (widgetShowAll driveMenu >> menuPopup driveMenu Nothing)
  boxPackStart naviBar (widget driveButton)    PackNatural 0
#endif
  boxPackStart naviBar (widget curdir)         PackGrow    0
  boxPackStart naviBar (widget saveDirButton)  PackNatural 0

  -- Целиком окно файл-менеджера
  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]
  boxPackStart vBox menuBar   PackNatural 0
  boxPackStart vBox toolBar   PackNatural 0
  boxPackStart vBox naviBar   PackNatural 0
  boxPackStart vBox listUI    PackGrow    0
  boxPackStart vBox lowBox    PackNatural 0

  containerAdd window vBox


  -- Список действий, выполняемых при закрытии окна файл-менеджера
  onExit <- newList

  -- Список ассоциаций клавиша->действие
  listView `onKeyPress` \event -> do
    x <- lookup (eventKey event) `fmap` listVal onKeyActions
    case x of
      Just action -> do action; return True
      Nothing     -> return False


----------------------------------------------------------------------------------------------------
---- Сохранение/восстановление размера и положения главного окна и колонок в нём -------------------
----------------------------------------------------------------------------------------------------

  window `windowSetPosition` WinPosCenter
  --windowSetGeometryHints window (Just window) (Just (1,1)) (Just (32000,32000)) Nothing Nothing Nothing
  --widgetSetSizeRequest window 700 500
  --window `windowSetGravity` GravityStatic
  --window `windowSetPosition` WinPosNone
  --windowSetDefaultSize window 200 100

  -- При старте восстановим сохранённый размер окна
  fmRestoreSizePos fm' window "MainWindow" "-10000 -10000 720 500"

  -- Сохраним размер и положение главного окна после его перемещения
  window `onConfigure` \e -> do
    fmSaveSizePos fm' window "MainWindow"
    return False

  -- Запомним, было ли окно максимизировано
  window `onWindowState` \e -> do
    let isMax x = case x of
                    WindowStateMaximized -> True
                    _                    -> False
    fmSaveMaximized fm' "MainWindow" (any isMax (eventWindowState e))
    return False

  -- При закрытии программы сохраним порядок и ширину колонок, при старте восстановим их
  onExit <<= saveColumnsOrderAndWidths fm' "FileManager" listView columns
  restoreColumnsOrderAndWidths         fm' "FileManager" listView columns


----------------------------------------------------------------------------------------------------
---- Навигационная часть файл-менеджера ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--  for [upButton,saveDirButton] (`buttonSetFocusOnClick` False)

  -- Выводить errors/warnings/messages внизу окна FreeArc
  showErrors' <- ref True
  errorHandlers   ++= [whenM (val showErrors') . condPrintLineLn "w"]
  errorHandlers   ++= [whenM (val showErrors') . postGUIAsync . fmStackMsg fm']
  warningHandlers ++= [whenM (val showErrors') . postGUIAsync . fmStackMsg fm']
  loggingHandlers ++= [postGUIAsync . fmStackMsg fm']

  -- Отключает вывод сообщений об ошибках на время выполнения action
  let hideErrors action  =  bracket (showErrors' <=> False)  (showErrors' =: )  (\_ -> action)

  -- Перехват и обработка ошибок
  errorMsg <- ref ""
  errorHandlers ++= [(errorMsg =:)]
  let withErrorHandler onError = handle$ \e->do operationTerminated =: False
                                                fmErrorMsg fm' =<< val errorMsg
                                                sequence_ onError
  -- При возникновении ошибки выдать её пользователю
  let msgboxOnError = withErrorHandler []
  -- При возникновении ошибки выдать её пользователю и заверщить выполнение программы
  let terminateOnError = withErrorHandler [shutdown "" aEXIT_CODE_FATAL_ERROR]

  -- Переход в родительский каталог
  let goParentDir = do
        fm <- val fm'
        unless (isFM_Archive fm  &&  isURL(fm_arcname fm)  &&  fm_arcdir fm=="") $ do  -- Запретить Up из архива в инете
        fmChdir fm' ".."
        -- Выбираем каталог/архив, из которого мы только что вышли
        fmSetCursor fm' (takeFileName$ fm_current fm)

  -- Переход в корневой каталог
  let goRootDir = fmChdir fm' "/"

  -- Поток команд работы с архивами
  cmdChan <- newChan

  -- Запуск файла с диска/из архива
  let runme filename useFA = do
        fm <- val fm'
        if not(isFM_Archive fm)  then runFile filename (fm_curdir fm) False  else do
        forkIO_ $ do
        withTempDir $ \tempdir -> do
          patterns <- fmGetHistory1 fm' "ExtractAllFor" "" >>== split_by_any " \t,;"
          decryptionOptions <- fmGetDecryptionOptions fm'
          let extract_all = any (filenameLower filename ~=) patterns                   -- True, если надо извлечь все файлы из архива (для запуска exe/htm...)
              cmd         = ["x", "-dp"++tempdir, "-fn"]++decryptionOptions++["--", fm_arcname fm]
                              ++ (not extract_all  &&&  [fm_arcdir fm </> filename])   -- имя извлекаемого файла, если нужен только он сам
          successful <- newEmptyMVar
          writeChan cmdChan ([cmd], putMVar successful)
          whenM (takeMVar successful) $ do  -- запустить файл только если команда извлечения завершена без ошибки
            if useFA then do freearc <- getExeName
                             let full_cmd = unparseCommand [freearc,filename]
                             Files.runCommand full_cmd (tempdir </> fm_arcdir fm) True

                     else do runFile filename (tempdir </> fm_arcdir fm) True
            -- Некоторые просмотрщики запускают асинхронноно другой .exe для открытия файла, на этот случай немного подождём прежде чем стирать его
            sleepSeconds 10
            -- to do: if file was modified - ask user whether to put it back to archive


  let -- Перейти в заданный каталог/архив
      select filename = select' filename False

      -- Перейти в заданный каталог/архив или выполнить команду
      select_or_run filename = do
        patterns <- fmGetHistory1 fm' "RunFor" "" >>== split_by_any " \t,;"    -- имена файлов, которые надо сразу запускать, не пытаясь открыть их как архивы
        if (any (filenameLower filename ~=) patterns)
          then runme filename False
          else select' filename True

      select' filename allow_run = do
        handle (\e -> (operationTerminated =: False) >> (allow_run &&& runme filename False)) $ do           -- при неудаче перехода запустим файл :)
          hideErrors $ do
            fmChdir fm' filename
            -- Если это архив в стиле .tar.gz, то сразу откроем внутренний архив
            quickOpenTarGz <- fmGetHistoryBool fm' "QuickOpenTarGz" True
            fm <- val fm'
            let dir = take 2 (arcDirectory (fm_archive fm))
                filename = storedName(cfFileInfo(dir!!0))
            if quickOpenTarGz  &&  (isFM_Archive fm)  &&  (arcArchiveType(fm_archive fm) `elem` words "bzip2 gzip xz Z lzma lzma86")
                &&  (length dir == 1)  &&  (filenameLower(takeExtension(filename)) `elem` words ".tar .cpio")
              then runme filename True >> goParentDir
              else New.treeViewScrollToPoint (fm_view fm) 0 0   -- иначе поставим курсор на первый файл в каталоге, куда мы перешли


  -- Запись текущего каталога в историю
  let saveCurdirToHistory = do
        fm <- val fm'
        fmAddHistory fm' (isFM_Archive fm.$bool "dir" "arcname") =<< fmCanonicalizePath fm' =<< val curdir


  -- При нажатии Enter на строке в списке открываем выбранный архив/каталог
  listView `New.onRowActivated` \path column -> do
    fm <- val fm'
    file <- fmFileAt fm' path
    select_or_run (fmname file)

  -- Вверх и вниз по иерархии (FAR-style keys)
  "<Ctrl>Page_Up"   `onKey` goParentDir
  "<Ctrl>Page_Down" `onKey` do select =<< fmGetCursor fm'


  -- При single-click на свободном пространстве справа/снизу снимаем отметку со всех файлов,
  -- при double-click там же выбираем все файлы
  listView `onButtonPress` \e -> do
    path <- New.treeViewGetPathAtPos listView (round$ eventX e, round$ eventY e)
    coltitle <- case path of
                  Just (_,column,_) -> New.treeViewColumnGetTitle column >>== fromMaybe ""
                  _                 -> return ""
    -- Пустая строка в coltitle означает клик за пределами списка файлов
    coltitle=="" &&& e.$eventButton==LeftButton &&&
      ((if e.$eventClick==SingleClick  then fmUnselectAll  else fmSelectAll) fm'  >>  return True)

  -- При переходе в другой каталог/архив отобразить его имя в строке ввода
  fm' `fmOnChdir` do
    fm <- val fm'
    curdir =: fm_current fm
    -- Сохраняем в историю имена архивов
    isFM_Archive fm  &&&  fm_arcdir fm==""  &&&  saveCurdirToHistory
    -- Перечитаем историю с диска
    rereadHistory curdir

  -- При переходе в другой каталог/архив - отобразить его имя в заголовке окна
  fm' `fmOnChdir` do
    fm <- val fm'
    let title | isFM_Archive fm  =  takeFileName (fm_arcname fm) </> fm_arcdir fm
              | otherwise        =  fm_dir fm
    set (fm_window fm) [windowTitle := title++" - "++aARC_NAME]

  -- Переходим в род. каталог по кнопке Up или нажатию BackSpace в списке файлов
  upButton  `onClick` goParentDir
  "BackSpace" `onKey` goParentDir
  -- "<Ctrl>BackSlash"  `onKey` goRootDir

  -- Сохранение выбранного архива/каталога в истории
  saveDirButton `onClick` do
    saveCurdirToHistory

  -- Открытие другого каталога или архива (Enter в строке ввода)
  entry curdir `onEntryActivate` do
    saveCurdirToHistory
    select_or_run =<< val curdir

  -- Открытие другого каталога или архива (выбор из истории)
  on (widget curdir) changed $ do
    choice <- New.comboBoxGetActive (widget curdir)
    when (choice /= -1) $ do
      saveCurdirToHistory
      select_or_run =<< val curdir

  -- Выполнить action над файлами, состоящими в соотношении makeRE с именем файла под курсором
  let byFile action makeRE = do
        filename <- fmGetCursor fm'
        action fm' ((match$ makeRE filename).fdBasename)

  -- Клавиши Shift/Ctrl/Alt-Plus/Minus с теми же операциями как в FAR
  "<Shift>KP_Add"      `onKey` fmSelectAll   fm'
  "<Shift>KP_Subtract" `onKey` fmUnselectAll fm'
  "<Ctrl>KP_Add"       `onKey` byFile fmSelectFilenames   (("*" ++).takeExtension)
  "<Ctrl>KP_Subtract"  `onKey` byFile fmUnselectFilenames (("*" ++).takeExtension)
  "<Alt>KP_Add"        `onKey` byFile fmSelectFilenames   ((++".*").dropExtension)
  "<Alt>KP_Subtract"   `onKey` byFile fmUnselectFilenames ((++".*").dropExtension)


  -- При нажатии заголовка столбца в списке файлов - сортировать по этому столбцу
  --   (при повторном нажатии - сортировать в обратном порядке)
  onColumnTitleClicked =: \column -> do
    fmModifySortOrder fm' (showSortOrder columns) (calcNewSortOrder column)
    refreshCommand fm'
    fmSaveSortOrder  fm' =<< fmGetSortOrder fm'  -- запишем в конфиг порядок сортировки

  -- Отсортируем файлы по сохранённому критерию
  fmSetSortOrder fm' (showSortOrder columns) =<< fmRestoreSortOrder fm'


  -- Ctrl-H toggles visibility of hidden files
  "<Ctrl>H" `onKey` do
     fmGetHistoryBool fm' "ShowHiddenFiles" False  >>=  fmReplaceHistoryBool fm' "ShowHiddenFiles" . not
     refreshCommand fm'


----------------------------------------------------------------------------------------------------
---- Движок выполнения консольных команд внутри FM gui ---------------------------------------------
----------------------------------------------------------------------------------------------------

  -- При выполнении операций не выходим по исключениям, а печатаем сообщения о них в логфайл
  let myHandleErrors successful action  =  do operationTerminated =: False
                                              parent_id =:: myThreadId
                                              action `catch` handler
                                              whenM (val operationTerminated) $ do
                                                sleepSeconds 0.1
                                                operationTerminated =: False
        where handler ex = do
                successful =: False
                unlessM (val operationTerminated) $ do
                  errmsg <- case ex of
                     Deadlock    -> i18n"0011 No threads to run: infinite loop or deadlock?"
                     ErrorCall s -> return s
                     other       -> return$ show ex
                  condPrintLineLn "w" errmsg
                  return ()
                condPrintLineLn "w" ""

  -- Тред, выполняющий команды архиватора
  forkIO $ do
    foreverM $ do
      (commands, postAction) <- readChan cmdChan
      when (commands==[["ExitProgram"]])  $ do fileManagerMode =: True; shutdown "" aEXIT_CODE_SUCCESS
      postGUIAsync$ do widgetShowAll windowProgress
      successful <- ref True
      for commands $ \cmd -> do
        myHandleErrors successful (parseCmdline cmd >>= mapM_ run)
      postAction =<< val successful
      whenM (isEmptyChan cmdChan)$ postGUIAsync$ do onEmptyQueue; widgetHide windowProgress; clearMessageBox; warningsBefore =:: val warnings; refreshCommand fm'
      --uiDoneProgram

  -- Пока не использовать WinRAR для тех команд, где ещё не добавлена специфическая обработка
  let use_winrar = False

  -- Depending on execution mode, either queue commands or run external FreeArc instances
  let exec detach use_winrar postAction cmds =
        if detach || use_winrar
          then do archiver <- if use_winrar  then return "winrar.exe"  else getExeName
                  fm <- val fm'
                  forkIO_ $ do
                    for cmds $ \cmd -> do
                      let full_cmd = unparseCommand (archiver:cmd)
                      if length(full_cmd) < aMAX_CMDLINE_LENGTH
                        then Files.runCommand full_cmd (fm_curdir fm) True
                        else withTempFile (unicode2utf8$ unparseCommand cmd) $ \cmdfile -> do
                               Files.runCommand (unparseCommand [archiver,'@':cmdfile]) (fm_curdir fm) True
                    postAction True

          else writeChan cmdChan (cmds,postAction)

  -- Закрытие окна файл-менеджера
  let closeMainWindow = do
        sequence_ =<< listVal onExit
        fileManagerMode =: False
        showMessageBox
        widgetHide window
        writeChan cmdChan ([["ExitProgram"]],doNothing)

  window `onDestroy` closeMainWindow


----------------------------------------------------------------------------------------------------
---- Меню File -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Открыть архив
  openAct `onActionActivate` do
    fm <- val fm'
    let curfile  =  if isFM_Archive fm  then fm_arcname fm  else fm_dir fm </> "."
    chooseFile window FileChooserActionOpen "0305 Open archive" aARCFILE_FILTER (return curfile) $ \filename -> do
      msgboxOnError $
        fmChdir fm' filename

  -- Select/unselect files by user-supplied mask
  let byDialog method msg = do
        whenJustM_ (fmInputString fm' "mask" msg (const$ return True) return) $ \mask -> do
          method fm' ((match mask).fdBasename)
  selectAct   `onActionActivate`  byDialog fmSelectFilenames   "0008 Select files"
  unselectAct `onActionActivate`  byDialog fmUnselectFilenames "0009 Unselect files"

  -- Выделить все файлы
  selectAllAct `onActionActivate` do
    fmSelectAll fm'

  -- Инвертировать выделение
  invertSelAct `onActionActivate` do
    fmInvertSelection fm'

  -- Обновить список файлов актуальными данными
  refreshAct `onActionActivate` do
    refreshCommand fm'

  -- Выход из программы
  exitAct `onActionActivate`
    closeMainWindow


----------------------------------------------------------------------------------------------------
---- Меню Commands ---------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Упаковка данных
  addAct `onActionActivate` do
    compressionOperation fm' addDialog exec [AddDetachButton] "a" NoMode

  -- Распаковка архив(ов)
  extractAct `onActionActivate` do
    archiveOperation fm' $
      extractDialog fm' exec [AddDetachButton] "x"
    rereadHistory curdir

  -- Тестирование архив(ов)
  testAct `onActionActivate` do
    archiveOperation fm' $
      extractDialog fm' exec [AddDetachButton] "t"

  -- Информация об архиве
  arcinfoAct `onActionActivate` do
    msgboxOnError $
      archiveOperation fm' $
        arcinfoDialog fm' exec NoMode

  -- Удаление файлов [из архива]
  deleteAct `onActionActivate` do
    fm <- val fm'
    files <- getSelection fm' (if isFM_Archive fm  then xCmdFiles  else (\dirname -> [dirname++[pathSeparator]]))
    if null files  then fmErrorMsg fm' "0012 There are no files selected!" else do
    msgDir <- i18n "0484 Delete directory %1?"
    msg <- i18n$ case files of [f] | isFM_Archive fm          -> "0160 Delete %1 from archive?"
                                   | isPathSeparator (last f) -> msgDir
                                   | otherwise                -> "0161 Delete %1?"
                               _   | isFM_Archive fm          -> "0019 Delete %2 file(s) from archive?"
                                   | otherwise                -> "0020 Delete %2 file(s)?"
    whenM (askOkCancel window (formatn msg [head files, show3$ length files])) $ do
      files <- getSelection fm' (if isFM_Archive fm  then dCmdFiles  else (\dirname -> [dirname++[pathSeparator]]))
      if isFM_Archive fm
        -- Стереть файлы из архива
        then do closeFMArc fm'
                fmDeleteSelected fm'
                let arcname = fm_arcname fm
                exec False use_winrar doNothing [["d", "--noarcext", "-fn", "--", arcname]++map (fm_arcdir fm </>) files]
        -- Удалить файлы/каталоги на диске
        else do let rm f | isPathSeparator (last f) = whenM (askOkCancel window (format msgDir (dropEnd 1 f))) $ do
                                                         dirRemoveRecursive fileRemove (dropEnd 1 f)
                         | otherwise                = fileRemove f
                mapM_ (ignoreErrors.rm.(fm_dir fm </>)) files
                fmDeleteSelected fm'


----------------------------------------------------------------------------------------------------
---- Меню Tools ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Защитить архив от записи
  lockAct `onActionActivate` do
    multiArchiveOperation fm' $ \archives -> do
      let msg = "0299 Lock archive(s)?"
      whenM (askOkCancel window (formatn msg [head archives, show3$ length archives])) $ do
        closeFMArc fm'
        for archives $ \arcname -> do
          exec False use_winrar doNothing [["ch", "--noarcext", "-k", "--", arcname]]

  -- Изменить комментарий архива
  commentAct `onActionActivate` do
    msgboxOnError $
      archiveOperation fm' $
        arcinfoDialog fm' exec CommentMode

  -- Пережать файлы в архиве
  recompressAct `onActionActivate` do
    compressionOperation fm' addDialog exec [AddDetachButton] "ch" RecompressMode

  -- Преобразовать архив в SFX
  toSfxAct `onActionActivate` do
    compressionOperation fm' addDialog exec [AddDetachButton] "ch" MakeSFXMode

  -- Преобразовать чужой архив в формат FreeArc
  toFaAct `onActionActivate` do
    compressionOperation fm' addDialog exec [AddDetachButton] "cvt" NoMode

  -- Зашифровать архив
  encryptAct `onActionActivate` do
    compressionOperation fm' addDialog exec [AddDetachButton] "ch" EncryptionMode

  -- Добавить RR в архив
  addRrAct `onActionActivate` do
    compressionOperation fm' addDialog exec [AddDetachButton] "ch" ProtectionMode

  -- Восстановить повреждённый архив
  repairAct `onActionActivate` do
    multiArchiveOperation fm' $ \archives -> do
      let msg = "0381 Repair archive(s)? Repaired archive(s) will be placed into files named fixed.*"
      whenM (askOkCancel window (formatn msg [head archives, show3$ length archives])) $ do
        closeFMArc fm'
        for archives $ \arcname -> do
          exec False use_winrar doNothing [["r", "--noarcext", "--", arcname]]

  -- Модификация архивов
  modifyAct `onActionActivate` do
    compressionOperation fm' addDialog exec [AddDetachButton] "ch" NoMode

  -- Объединение архивов
  joinAct `onActionActivate` do
    compressionOperation fm' addDialog exec [AddDetachButton] "j" NoMode


----------------------------------------------------------------------------------------------------
---- Меню Options ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Окно настроек программы
  settingsAct `onActionActivate` do
    settingsDialog fm'

  -- Вызов gtk2_prefs.exe
  changeSkinAct `onActionActivate` do
    exe <- getExeName                                -- Name of FreeArc.exe file
    let dir   =  exe.$takeDirectory                  -- FreeArc.exe directory
    Files.runCommand (dir</>"gtk2_prefs.exe") dir False

  -- Действия с логфайлом
  let withLogfile action = do
        logfileHist <- fmGetHistory fm' "logfile"
        case logfileHist of
          logfile:_ | logfile>""  ->  action logfile
          _                       ->  fmErrorMsg fm' "0303 No log file specified in Settings dialog!"

  -- Просмотреть логфайл
  viewLogAct `onActionActivate` do
    withLogfile runViewCommand

  -- Удалить логфайл
  clearLogAct `onActionActivate` do
    withLogfile $ \logfile -> do
      msg <- i18n"0304 Clear logfile %1?"
      whenM (askOkCancel window (format msg logfile)) $ do
        filePutBinary logfile ""


----------------------------------------------------------------------------------------------------
---- Меню Help -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Код языка (ru,ja...)
  langcode <- i18n "0462 "
  let newsURL = aARC_WEBSITE ++ "/News.aspx"

  -- Открыть URL
#ifdef FREEARC_WIN
  let openWebsite url  =  runFile url "." False
#else
  let openWebsite url  =  do browser <- runProgram "gconftool-2 --get '/desktop/gnome/url-handlers/http/command'"
                             browser <- return$ if "%s" `isInfixOf` browser  then browser  else "firefox \"%s\""
                             System.Process.runCommand (browser.$ replaceAll "%s" url)
                             return ()
#endif

  -- Открыть (локализованную) страницу на сайте программы
  let openFreeArcSite page = do
        forkIO_ $ do
          let url     = aARC_WEBSITE ++ "/" ++ page
              i18_url = aARC_WEBSITE ++ "/" ++ langcode ++ "/" ++ page
          if langcode==""  then openWebsite url  else do
          i18 <- fileExist i18_url
          openWebsite (iif i18 i18_url url)

  -- Открыть файл помощи
  let openHelp helpfile = do
        doc  <- i18n helpfile
        file <- findFile libraryFilePlaces (iif isWindows "..\\Documentation" "Documentation" </> doc)
        case file of
          "" -> return ()
          _  -> openWebsite file

  -- Прочитать ИД, сгенерённый для этого компьютера
  let getUserID = do
#ifndef FREEARC_WIN
        -- Имитация отсутствующего Windows Registry для других ОС
        let registryGetStr root branch key       = return Nothing
            registrySetStr root branch key value = return ()
            hKEY_LOCAL_MACHINE                   = ()
#endif
        -- Сначала ищем его в ини-файле
        userid <- fmGetHistory1 fm' "UserID" ""
        if userid/=""  then return (Just userid)  else do
        -- Если не получилось - читаем ключ предыдущей инсталляции из Windows Registry...
        userid <- do userid <- registryGetStr hKEY_LOCAL_MACHINE ("SOFTWARE\\"++aFreeArc) "UserID"
                     case userid of
                       Just userid -> return userid
                                      -- ... или в крайнем случае - генерим новый
                       Nothing     -> generateRandomBytes 8 >>== encode16
        -- И записываем его повсюду
        registrySetStr hKEY_LOCAL_MACHINE ("SOFTWARE\\"++aFreeArc) "UserID" userid
        fmReplaceHistory fm' "UserID" userid
        -- Возвращаем его только если запись в ини-файл была успешной
        userid1 <- fmGetHistory1 fm' "UserID" ""
        return (if userid==userid1  then Just userid  else Nothing)

  -- Возвращает True раз в сутки
  let daily = do
        last <- fmGetHistory1 fm' "LastCheck" ""
        now  <- getUnixTime
        let day = round$ 24.37*60*60
        if  last>""  &&  (now - readI last < day)  then return False  else do
        fmReplaceHistory fm' "LastCheck" (show now)
        now1 <- fmGetHistory1 fm' "LastCheck" ""
        return (show now==now1)

  -- Size of maximum memory block we can allocate in bytes
  maxBlock <- getMaxBlockToAlloc

  -- Регистрирует использование программы и проверяет новости
  --  (manual=True - ручной вызов из меню, False - ежедневная автопроверка)
  let checkNews manual = do
        postGUIAsync$ fmStackMsg fm' "0295 Checking for updates..."
        forkIO_ $ do
          -- Сообщим об использовании программы
          whenJustM_ getUserID $ \userid -> do
#ifdef FREEARC_WIN
            si <- getSystemInfo; let ramLimit = showMem (si.$siMaximumApplicationAddress.$ptrToWordPtr.$toInteger `roundTo` (4*mb))
#endif
            language <- i18n"0000 English"
            let url = aARC_WEBSITE ++ "/CheckNews.aspx"
#ifndef HAMSTER
                                   ++ "?user=" ++ userid ++ "&version=" ++ urlEncode aARC_VERSION
                                   ++ "&OS%20family=" ++ iif isWindows "Windows" "Unix"
                                   ++ "&RAM=" ++ showMem (toInteger getPhysicalMemory `roundTo` mb)
#ifdef FREEARC_WIN
                                   ++ "&address%20space=" ++ ramLimit
                                   ++ "&windows%20version=" ++ urlEncode getWindowsVersion
#endif
                                   ++ "&largest%20memory%20block=" ++ showMem (maxBlock `roundDown` mb)
                                   ++ "&number%20of%20cores=" ++ show getProcessorsCount
                                   ++ "&language=" ++ urlEncode language
#endif
            -- Сообщаем статистику и проверяем страницу новостей
            handleErrors
              -- Выполняется при недоступности страницы
              (when manual$ postGUIAsync$ do
                  msg <- i18n"0296 Cannot open %1. Do you want to check the page with browser?"
                  whenM (askOkCancel window (format msg newsURL)) $ do
                    openFreeArcSite "News.aspx")
              -- Попытка прочитать страницу
              (fileGetBinary url) $ \news -> do
            -- Страница новостей успешно прочитана
            new_crc <- return$ showHex (crc32 news) ""
            old_crc <- fmGetHistory1 fm' "news_crc" ""
            postGUIAsync$ do
            fmStackMsg fm' ""
            if (new_crc == old_crc) then do
               msg <- i18n"0297 Nothing new at %1"
               manual &&& fmInfoMsg fm' (format msg newsURL)
             else do
               fmReplaceHistory fm' "news_crc" new_crc
               let news_page = news.$ (lines >>> map (split2 '=') >>> lookup "newsURL" >>> fromMaybe "News.aspx")
                   newsURL   = aARC_WEBSITE ++ "/" ++ news_page
               msg <- i18n"0298 Found new information at %1! Open the page with browser?"
               whenM (askOkCancel window (format msg newsURL)) $ do
                 openFreeArcSite news_page

  -- Дважды в час проверять отсутствие новостей
  forkIO_ $ do
    whenM (fmGetHistoryBool fm' "CheckNews" True) $ do
      foreverM $ do
        whenM daily $ do
          checkNews False
        sleepSeconds (30*60)


  -- Предложить удалить старые временные файлы (если мы не работаем как раз с ними - например при открытии вложенного архива)
  unlessM (isTemporaryInstance args) $ forkIO_ $ do
    files <- findTempFiles
    when (not$ null files) $ do
      postGUIAsync$ do
        whenM (askYesNo window "0494 Found temporary FreeArc files. Delete them?\n(Press 'No' if there is already another FreeArc process running in the background)") $ do
          for files $ \fname -> do
            ignoreErrors$ forcedFileRemove fname
            ignoreErrors$ dirRemoveRecursive forcedFileRemove fname


  -- Помощь по использованию GUI
  helpAct `onActionActivate` do
    openHelp "0256 FreeArc-GUI-Eng.htm"

  -- Помощь по использованию командной строки
  helpCmdAct `onActionActivate` do
    openHelp "0257 FreeArc036-eng.htm"

  -- Домашняя страница программы
  homepageAct `onActionActivate` do
    openFreeArcSite ""

  -- Домашняя страница программы
  openForumAct `onActionActivate` do
    openFreeArcSite "redirects/forum.aspx"

  -- Домашняя страница программы
  openWikiAct `onActionActivate` do
    openFreeArcSite "redirects/wiki.aspx"

  -- Проверка обновлений на сайте
  whatsnewAct `onActionActivate` do
    checkNews True

  -- Диалог About
  aboutAct `onActionActivate` do
    license <- i18ns aARC_LICENSE
    bracketCtrlBreak "aboutDialogDestroy" aboutDialogNew widgetDestroy $ \dialog -> do
    contributors <- findFile libraryFilePlaces (iif isWindows "../License" "License" </> "Contributors.txt")
    contributors <- contributors &&& fileGetBinary contributors
    dialog `set` [windowTransientFor   := window
                 ,aboutDialogName      := aARC_NAME
                 ,aboutDialogVersion   := aARC_VERSION_WITH_DATE
                 ,aboutDialogCopyright := "(c) "++aARC_EMAIL
                 ,aboutDialogComments  := unlines license
                 ,aboutDialogWebsite   := aARC_WEBSITE
                 ,aboutDialogAuthors   := lines contributors
                 ]
    dialogRun dialog
    return ()

  -- Включить поддержку URL в диалоге About
  aboutDialogSetUrlHook (\url -> if url==aARC_WEBSITE  then openFreeArcSite ""  else openWebsite url)

  -- Инициализируем состояние файл-менеджера каталогом/архивом, заданным в командной строке (при его отсутствии - текущим каталогом)
  terminateOnError $
    fmChdir fm' (head (args++["."]))

  widgetShowAll window
