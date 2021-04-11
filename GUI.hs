{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Информирование пользователя о ходе выполнения программы.                                 ------
----------------------------------------------------------------------------------------------------
module GUI where

import Prelude    hiding (catch)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.OldException
import Data.Char  hiding (Control)
import Data.IORef
import Data.List
import Data.Maybe
import Foreign
import Foreign.C
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.IO
import System.Time

import Graphics.UI.Gtk   -- for gtk2hs 0.11: hiding (eventKeyName, eventModifier, eventWindowState, eventButton, eventClick)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import Charsets
import FileInfo
import Encryption (generateRandomBytes)
import Options
import UIBase

-- |Файл с описанием меню и тулбара
aMENU_FILE = "freearc.menu"

-- Теги в INI-файле
aINITAG_LANGUAGE = "language"

-- |Каталог локализаций
aLANG_DIR = "arc.languages"

-- |Фильтры для выбора архива
aARCFILE_FILTER = ["0307 "++aFreeArc++" archives (*.arc)", "0308 Archives and SFXes (*.arc;*.exe)"]

-- Интерфейсные тексты
shutdown_msg         = "0479 Shutdown computer when operation completed"
global_queueing_msg  = "0508 Queue operations across multiple FreeArc copies"

-- |Фильтр для выбора любого файла
aANYFILE_FILTER = []

-- |Локализация (установить файл локализации, выбраныый в диалоге конфигурирования, плюс arc.english.txt для непереведённых tooltips)
loadTranslation = do
  langDir  <- findDir libraryFilePlaces aLANG_DIR
  settings <- readIniFile
  setLocale [langDir </> aENGLISH_LANG_FILE
            ,langDir </> (settings.$lookup aINITAG_LANGUAGE `defaultVal` aLANG_FILE)]

-- |Прочитать настройки программы из ini-файла
readIniFile = do
  inifile  <- findFile configFilePlaces aINI_FILE
  inifile  &&&  readConfigFile inifile >>== map (split2 '=')

-- |Left/right alignment
data Alignment = Align___ | I___Align deriving Eq


----------------------------------------------------------------------------------------------------
---- Отображение индикатора прогресса --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Переменная, хранящая номер GUI-треда
guiThread  =  unsafePerformIO$ newIORef$ error "undefined GUI::guiThread"

-- |Инициализация Gtk для интерактивной работы (режим FileManager)
runGUI action = do
  runInBoundThread $ do
  unsafeInitGUIForThreadedRTS
  guiThread =:: getOsThreadId
  runLanguageDialog
  action
  mainGUI

-- |Инициализация Gtk для выполнения cmdline
startGUI = do
  x <- newEmptyMVar
  forkIO$ do
    runGUI$ putMVar x ()
  takeMVar x

-- |Инициализация GUI-части программы (индикатора прогресса) для выполнения cmdline
guiStartProgram = gui $ do
  (windowProgress, msgbActions) <- runIndicators
  widgetShowAll windowProgress

-- |Задержать завершение программы
guiPauseAtEnd = do
  uiMessage =: ("", "")
  updateAllIndicators
  val progressFinished >>= gui
  foreverM $ do
    sleepSeconds 1

-- |Завершить выполнение программы
guiDoneProgram = do
  return ()


{-# NOINLINE runIndicators #-}
-- |Создаёт окно индикатора прогресса и запускает тред для его периодического обновления.
runIndicators = do
  hf' <- openHistoryFile

  -- Собственно окно индикатора прогресса
  window <- windowNew
  vbox   <- vBoxNew False 0
  set window [windowWindowPosition := WinPosCenter,
              containerBorderWidth := 10, containerChild := vbox]
  hfRestoreSizePos hf' window "ProgressWindow" "-10000 -10000 350 200"

  -- Разделим окно по вертикали
  (statsBox, updateStats, clearStats) <- createStats
  curFileLabel <- labelNew Nothing
  curFileBox   <- hBoxNew True 0
  boxPackStart curFileBox curFileLabel PackGrow 0
  widgetSetSizeRequest curFileLabel 30 (-1)
  progressBar  <- progressBarNew
  buttonBox    <- hBoxNew True 10
  (expanderBox, insideExpander)  <- expander ""
  (messageBox, msgbActions) <- makeBoxForMessages
  boxPackStart vbox statsBox     PackNatural 0
  boxPackStart vbox curFileBox   PackNatural 10
  boxPackStart vbox progressBar  PackNatural 0
  boxPackStart vbox (widget expanderBox)  PackNatural 0
  boxPackStart vbox messageBox   PackGrow    0
  boxPackStart vbox buttonBox    PackNatural 0
  miscSetAlignment curFileLabel 0 0    -- выравняем влево имя текущего файла
  progressBarSetText progressBar " "   -- нужен непустой текст чтобы установить правильную высоту progressBar

  expanderBox =:: hfGetHistoryBool hf' "ProgressWindow.Expanded" False
  setOnUpdate expanderBox $   do hfReplaceHistoryBool hf' "ProgressWindow.Expanded" =<< val expanderBox

  onTop <- checkBox "0446 Keep window on top";  boxPackStart insideExpander (widget onTop) PackNatural 1
  setOnUpdate onTop $   do windowSetKeepAbove window =<< val onTop
  onTop =:: hfGetHistoryBool hf' "ProgressWindow.KeepOnTop" False

  --shutdown <- checkBox shutdown_msg;               boxPackStart insideExpander (widget shutdown) PackNatural 1

  -- Заполним кнопками нижнюю часть окна
  backgroundButton <- buttonNewWithMnemonic       =<< i18n"0052   _Background  "
  pauseButton      <- toggleButtonNewWithMnemonic =<< i18n"0053   _Pause  "
  cancelButton     <- buttonNewWithMnemonic       =<< i18n"0081   _Cancel  "
  boxPackStart buttonBox backgroundButton PackNatural 0
  boxPackStart buttonBox pauseButton      PackNatural 0
  boxPackEnd   buttonBox cancelButton     PackNatural 0

  -- Обработчики событий (закрытие окна/нажатие кнопок)
  let askProgramClose = do
        terminationRequested <- do
          -- Allow to close window immediately if program already finished
          finished <- val programFinished
          if finished  then return True  else do
          -- Otherwise - ask user's permission
          active <- val pauseButton
          (if active then id else syncUI) $ do
             pauseEverything$
               askYesNo window "0251 Abort operation?"
        when terminationRequested $ do
          pauseButton =: False
          ignoreErrors$ terminateOperation

  -- Обработка нажатия клавиш
  window `onKeyPress` \event -> do
    case (eventKey event) of
      "Escape" -> do askProgramClose; return True
      _        -> return False

  window `onDelete` \e -> do
    askProgramClose
    return True

  cancelButton `onClicked` do
    askProgramClose

  pauseButton `onToggled` do
    active <- val pauseButton
    if active then do takeMVar mvarSyncUI
                      pause_real_secs
                      taskbar_Pause
                      buttonSetLabel pauseButton =<< i18n"0054   _Resume  "
              else do putMVar mvarSyncUI "mvarSyncUI"
                      resume_real_secs
                      taskbar_Resume
                      buttonSetLabel pauseButton =<< i18n"0053   _Pause  "

  backgroundButton `onClicked` do
    windowIconify window

  finishUpdating <- ref False    -- устанавливается в True в конце работы когда надо перестать обновлять индикатор прогресса
  hwnd <- ref Nothing            -- место для сохранения виндового HWND окна window

  -- Обновляем заголовок окна, статистику и надпись индикатора прогресса раз в 0.5 секунды
  n' <- ref 0   -- а сам индикатор прогресса раз в 0.1 секунды
  indicatorThread 0.1 $ \updateMode indicator indType title b bytes total processed p -> postGUIAsync$ do
    unlessM (val finishUpdating) $ do
    n <- val n'; n' += 1; let once_a_halfsecond  =  (updateMode==ForceUpdate)  ||  (n `mod` 5 == 0)
    -- Win7+ taskbar progress indicator
#ifdef FREEARC_WIN
    whenM (isNothing ==<< val hwnd) $ do
      rnd <- encode16 ==<< generateRandomBytes 16
      set window [windowTitle := rnd]
      withCString rnd $ \c_rnd -> do
      hwnd =:: Just ==<< findWindowHandleByTitle c_rnd
    hwnd' <- val hwnd
    taskbar_SetWindowProgressValue (fromJust hwnd') (i bytes) (i total)   `on_` True
#endif
    -- Заголовок окна
    set window [windowTitle := title]                              `on_` once_a_halfsecond
    -- Статистика
    updateStats indType b total processed                          `on_` once_a_halfsecond
    -- Прогресс-бар и надпись на нём
    progressBarSetFraction progressBar processed                   `on_` True
    progressBarSetText     progressBar p                           `on_` once_a_halfsecond
    widgetGrabFocus cancelButton                                   `on_` (updateMode==ForceUpdate)  -- make Cancel button default after operation was finished

  backgroundThread 0.5 $ \updateMode -> postGUIAsync$ do
    unlessM (val finishUpdating) $ do
    -- Имя текущего файла или стадия выполнения команды (вывод пустой строки недопустим, поскольку это меняет высоту виджета)
    (msg,filename) <- val uiMessage;   imsg <- i18n msg
    labelSetText curFileLabel (if imsg>""   then format imsg filename   else (filename|||" "))

  -- Операция, очищающая все поля с информацией о текущем архиве
  clearProgressWindow =: do
    set window [windowTitle := " "]
    clearStats
    labelSetText curFileLabel " "
    progressBarSetFraction progressBar 0
    progressBarSetText     progressBar " "

  -- Выполняется по завершении всех операций
  progressFinished =: do
    widgetSetSensitivity backgroundButton False
    widgetSetSensitivity pauseButton      False
    buttonSetLabel cancelButton =<< i18n"0470   _Close  "
    finishUpdating =: True

  progressWindow =: window
  progressOnTop  =: onTop

  -- Поехали!
  widgetGrabFocus pauseButton
  return (window, msgbActions)


-- |Создание полей для вывода статистики
createStats = do
  upperTable <- tableNew 1 1 False                -- upper table for stats
  hsep       <- hSeparatorNew                     -- horizontal separator between the tables
  lowerTable <- tableNew 1 1 False                -- lower table for stats

  hbox <- hBoxNew False 0                         -- pack upper table into hbox in order to center table horizontally
  boxPackStart hbox upperTable   PackRepel   0
  vbox <- vBoxNew False 0                         -- then put both tables with separator into vertical box ...
  boxPackStart vbox hbox         PackNatural 0
  boxPackStart vbox hsep         PackNatural 4    -- add some space around the hor. line
  boxPackStart vbox lowerTable   PackNatural 0
  hbox <- hBoxNew False 0                         -- ... packed into hbox in order to center whole vbox horizontally
  boxPackStart hbox vbox         PackRepel   0

  tableSetRowSpacings upperTable 2                -- spacing for the upper table
  tableSetColSpacings upperTable 20
  tableSetColSpacings lowerTable 10               -- spacing for the lower table
  for [0,3,7] $ \x -> do                          -- insert empty fields in the lower table that fill up all unused space and provide centering of cells containing information
    l <- labelNew Nothing
    tableAttachDefaults lowerTable l x (x+1) 0 1

  displays' <- ref []                             -- list of all displayed number fields

  -- Создаёт неизменную метку в таблице
  let newLabel table y x width s alignment = do
                          label <- labelNewWithMnemonic =<< i18n s
                          tableAttach table label (x+1-width) (x+1) y (y+1) [Fill] [] 0 0
                          miscSetAlignment label (if alignment==Align___ then 0 else 1) 0

  -- Создаёт в таблице поле для отображения информации, добавляет его в список displays и возвращает операцию вывода значения в этом поле
  let newDisplay table y x = do
                          display <- labelNew Nothing
                          tableAttach table display x (x+1) y (y+1) [Fill] [] 0 0
                          set display [labelSelectable := True]
                          miscSetAlignment display 1 0
                          displays' ++= [display]
                          return (labelSetMarkup display . bold)


--               Files         Bytes     Compressed        Time
-- Processed         8    16,188,368      6,229,876     0:00:02
-- Total            35   134,844,601   ~ 51,893,133   ~ 0:00:17
-- ------------------------------------------------------------
--             Ratio 86%         Speed 16,624 kB/s

  newLabel upperTable 1 0 1 "0541 Processed"     Align___
  newLabel upperTable 2 0 1 "0542 Total"         Align___
  newLabel upperTable 0 1 2 "0056 Files"         I___Align
  newLabel upperTable 0 2 1 "0058 Bytes"         I___Align
  newLabel upperTable 0 3 1 "0252 Compressed"    I___Align
  newLabel upperTable 0 4 1 "0062 Time"          I___Align
  newLabel lowerTable 0 1 1 "0060 Ratio"         I___Align
  newLabel lowerTable 0 5 1 "0061 Speed"         I___Align

  filesDisplay           <- newDisplay upperTable 1 1
  totalFilesDisplay      <- newDisplay upperTable 2 1
  bytesDisplay           <- newDisplay upperTable 1 2
  totalBytesDisplay      <- newDisplay upperTable 2 2
  compressedDisplay      <- newDisplay upperTable 1 3
  totalCompressedDisplay <- newDisplay upperTable 2 3
  timesDisplay           <- newDisplay upperTable 1 4
  totalTimesDisplay      <- newDisplay upperTable 2 4
  ratioDisplay           <- newDisplay lowerTable 0 2
  speedDisplay           <- newDisplay lowerTable 0 6

  last_cmd' <- ref ""

  -- Процедура, выводящая текущую статистику (indType==INDICATOR_FULL - полноценный индикатор, иначе - только проценты, например операции с RR)
  let updateStats indType b total_b (processed :: Double) = do
        ~UI_State { total_files = total_files
                  , total_bytes = total_bytes
                  , files       = files
                  , cbytes      = cbytes
                  , archive_total_bytes      = archive_total_bytes
                  , archive_total_compressed = archive_total_compressed
                  }  <-  val ref_ui_state
        total_bytes <- return (if indType==INDICATOR_FULL  then total_bytes  else total_b)
        -- Общее время с начала операции и момент когда начался показ текущего индикатора прогресса
        secs <- return_real_secs
        sec0 <- val indicator_start_real_secs

        -- Если операция завершена - показываем точные результаты
        if b==total_bytes
          then do (filesDisplay$           "")
                  (bytesDisplay$           "")
                  (compressedDisplay$      "")
                  (timesDisplay$           "")
                  (totalFilesDisplay$      show3 total_files)                      `on_` indType==INDICATOR_FULL
                  (totalBytesDisplay$      show3 total_bytes)
                  (totalCompressedDisplay$ show3 (cbytes))                         `on_` indType==INDICATOR_FULL
                  (totalTimesDisplay$      showHMS (secs))
                  when (b>0) $ do                      -- Поля скорости/коэф. сжатия бессмысленно показывать пока не накоплена хоть какая-то статистика
                    (ratioDisplay$         compression_ratio cbytes b)             `on_` indType==INDICATOR_FULL
                  when (secs-sec0>0.001) $ do
                    (speedDisplay$         bold$ showSpeed b (secs-sec0))

          else do

        -- Определение Total compressed (точное только при распаковке архива целиком, иначе - оценка)
        cmd <- val ref_command >>== cmd_name
        let total_compressed
              | cmdType cmd == ADD_CMD              =  if b==total_bytes then show3 (cbytes)
                                                                         else "~"++show3 (total_bytes*cbytes `div` b)
              | archive_total_bytes == total_bytes  =       show3 (archive_total_compressed)
              | archive_total_bytes == 0            =       show3 (0)
              | otherwise                           =  "~"++show3 (toInteger archive_total_compressed*total_bytes `div` archive_total_bytes)

        (filesDisplay$           show3 files                                )  `on_` indType==INDICATOR_FULL
        (bytesDisplay$           show3 b                                    )
        (compressedDisplay$      show3 cbytes                               )  `on_` indType==INDICATOR_FULL
        (timesDisplay$           showHMS secs                               )
        (totalFilesDisplay$      show3 total_files                          )  `on_` indType==INDICATOR_FULL
        (totalBytesDisplay$      show3 total_bytes                          )
        when (b>0 && secs-sec0>0.001) $ do   -- Поля скорости/коэф. сжатия бессмысленно показывать пока не накоплена хоть какая-то статистика
        (ratioDisplay$           compression_ratio cbytes b                 )  `on_` indType==INDICATOR_FULL
        (speedDisplay$           showSpeed b (secs-sec0)                    )
        when (processed>0.001) $ do          -- Поля оценки времени/результата сжатия показываются только после сжатия 0.1% всей информации
        (totalCompressedDisplay$ total_compressed                           )  `on_` indType==INDICATOR_FULL
        (totalTimesDisplay$      "~"++showHMS (sec0 + (secs-sec0)/processed))

  -- Процедура, очищающая текущую статистику
  let clearStats  =  val displays' >>= mapM_ (`labelSetMarkup` "     ")
  --
  return (hbox, updateStats, clearStats)


-- |Создадим подокно для сообщений об ошибках
makeBoxForMessages = do
  msgbox <- scrollableTextView "" []
  widgetSetNoShowAll (widget msgbox) True
  saved <- ref ""
  -- Добавить сообщение msg в msgbox (и вывести msgbox на экран)
  let add msg = do widgetSetNoShowAll (widget msgbox) False
                   widgetShowAll      (widget msgbox)
                   msgbox ++= (msg++"\n")
  -- Выводить errors/warnings в этот TextView
  let log msg = postGUIAsync$ do
                  fm <- val fileManagerMode
                  if fm
                    then saved ++= (msg++"\n")
                    else add msg
  -- После закрытия FM перенести все сообщения в этот widget
  let afterFMClose = postGUIAsync$ do
                       msg <- val saved
                       saved =: ""
                       when (msg>"") $ do
                         add msg
  errorHandlers   ++= [log]
  warningHandlers ++= [log]
  return (widget msgbox, (saved =: "", afterFMClose))


{-# NOINLINE progressWindow #-}
progressWindow = unsafePerformIO$ newIORef$ error "Progress windows isn't yet created" :: IORef Window

{-# NOINLINE progressOnTop #-}
progressOnTop = unsafePerformIO$ newIORef$ error "Progress windows isn't yet created" :: IORef (GtkWidget HBox Bool)

{-# NOINLINE clearProgressWindow #-}
-- |Операция, очищающая окно индикатора прогресса
clearProgressWindow = unsafePerformIO$ newIORef doNothing0 :: IORef (IO ())

{-# NOINLINE progressFinished #-}
-- |Операция, переводящая диалог прогресса в состояние ожидания закрытия
progressFinished = unsafePerformIO$ newIORef doNothing0 :: IORef (IO ())


-- |Вызывается в начале обработки архива
guiStartArchive = gui$ val clearProgressWindow >>= id

-- |Отметить начало упаковки или распаковки данных
guiStartProcessing = doNothing0

-- |Начало следующего тома архива
guiStartVolume filename = doNothing0

-- |Вызывается в начале обработки файла
guiStartFile = doNothing0

-- |Текущий объём исходных/сжатых данных
guiUpdateProgressIndicator = doNothing0

-- |Приостановить вывод индикатора прогресса и стереть его следы
uiSuspendProgressIndicator = do
  aProgressIndicatorEnabled =: False

-- |Возобновить вывод индикатора прогресса и вывести его текущее значение
uiResumeProgressIndicator = do
  aProgressIndicatorEnabled =: True

-- |Приостановить индикатор (если он запущен) на время выполнения операции
uiPauseProgressIndicator action =
  bracket (aProgressIndicatorEnabled <=> False)
          (aProgressIndicatorEnabled =: )
          (\x -> action)

-- |Reset console title
resetConsoleTitle = return ()

-- |Приостановить показ окна прогресса поверх всех остальных
pauseOnTop action = do
  window <- val progressWindow
  onTop  <- val progressOnTop
  inside (windowSetKeepAbove window False)
         (windowSetKeepAbove window =<< val onTop)
         action

-- |Pause progress indicator & timing while dialog runs
pauseEverything  =  uiPauseProgressIndicator . pauseTiming . pauseTaskbar . pauseOnTop


----------------------------------------------------------------------------------------------------
---- GUI-специфичные операции с файлом истории -----------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Сохранить размеры и положение окна в истории
hfSaveSizePos hf' window name = do
    (x,y) <- windowGetPosition window
    (w,h) <- widgetGetSize     window
    hfReplaceHistory hf' (name++"Coord") (unwords$ map show [x,y,w,h])

-- |Запомним, было ли окно максимизировано
hfSaveMaximized hf' name = hfReplaceHistoryBool hf' (name++"Maximized")

-- |Восстановить размеры и положение окна из истории
hfRestoreSizePos hf' window name deflt = do
    coord <- hfGetHistory1 hf' (name++"Coord") deflt
    let a  = coord.$split ' '
    when (length(a)==4  &&  all isSignedInt a) $ do  -- проверим что a состоит ровно из 4 чисел
      let [x,y,w,h] = map readSignedInt a
      screen <- screenGetDefault
      scrw <- case screen of
                Nothing -> return 999999
                Just screen -> screenGetWidth screen
      scrh <- case screen of
                Nothing -> return 999999
                Just screen -> screenGetHeight screen
      when (x<scrw*9`div`10 && y<scrh*9`div`10 && w<scrw && h<scrh) $ do
        windowMove   window x y  `on_` x/= -10000
        windowResize window w h  `on_` w/= -10000
    whenM (hfGetHistoryBool hf' (name++"Maximized") False) $ do
      windowMaximize window


----------------------------------------------------------------------------------------------------
---- Выбор языка при первом запуске GUI ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Выбор языка при первом запуске GUI
runLanguageDialog = do
  hf'      <- openHistoryFile
  langFile <- hfGetHistory1 hf' aINITAG_LANGUAGE ""
  when (langFile=="") $ do
    -- Создадим диалог со стандартными кнопками OK/Cancel
    let question = "Please select your language (you may change language later in Settings dialog)"
    bracketCtrlBreak "LanguageDialog" (messageDialogNew Nothing [] MessageOther ButtonsOkCancel question) widgetDestroy $ \dialog -> do
    set dialog [windowTitle          := aFreeArc++": select your language",
                windowWindowPosition := WinPosCenter]

    -- Заполнить список языков именами файлов в каталоге arc.languages и выбрать English по умолчанию
    langDir   <- findDir libraryFilePlaces aLANG_DIR
    langFiles <- langDir &&& (dir_list langDir >>== map baseName >>== sort >>== filter (match "arc.*.txt"))
    langComboBox <- New.comboBoxNewText
    for langFiles (New.comboBoxAppendText langComboBox . mapHead toUpper . replace '_' ' ' . dropEnd 4 . drop 4)
    whenJust (langFiles.$ findIndex ((=="arc.english.txt").map toLower)) (New.comboBoxSetActive langComboBox)

    upbox <- dialogGetUpper dialog
    boxPackStart  upbox langComboBox PackGrow 0
    widgetShowAll upbox

    -- Определить файл локализации, соответствующий выбранному в комбобоксе языку
    let getCurrentLangFile = do
          lang <- New.comboBoxGetActive langComboBox
          case lang of
            -1   -> return ""
            lang -> myCanonicalizePath (langDir </> (langFiles !! lang))

    choice <- dialogRun dialog
    when (choice==ResponseOk) $ do
      langFile <- getCurrentLangFile
      hfReplaceHistory hf' aINITAG_LANGUAGE (takeFileName langFile)
      loadTranslation
      writeShellExtScript hf'


----------------------------------------------------------------------------------------------------
---- Запросы к пользователю ("Перезаписать файл?" и т.п.) ------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE askOverwrite #-}
-- |Запрос о перезаписи файла
askOverwrite filename diskFileSize diskFileTime arcfile ref_answer answer_on_u = do
  (title:file:question) <- i18ns ["0078 Confirm File Replace",
                                  "0165 %1\n%2 bytes\nmodified on %3",
                                  "0162 Destination folder already contains processed file.",
                                  "",
                                  "",
                                  "",
                                  "0163 Would you like to replace the existing file",
                                  "",
                                  "%1",
                                  "",
                                  "",
                                  "0164 with this one?",
                                  "",
                                  "%2"]
  let f1 = formatn file [filename,           show3$ diskFileSize,   formatDateTime$ diskFileTime]
      f2 = formatn file [storedName arcfile, show3$ fiSize arcfile, formatDateTime$ fiTime arcfile]
  ask (format title filename) (formatn (joinWith "\n" question) [f1,f2]) ref_answer answer_on_u

-- |Общий механизм для выдачи запросов к пользователю
ask title question ref_answer answer_on_u =  do
  old_answer <- val ref_answer
  new_answer <- case old_answer of
                  "a" -> return old_answer
                  "u" -> return old_answer
                  "s" -> return old_answer
                  _   -> ask_user title question
  ref_answer =: new_answer
  case new_answer of
    "u" -> return answer_on_u
    _   -> return (new_answer `elem` ["y","a"])

-- |Собственно общение с пользователем происходит здесь
ask_user title question  =  gui $ do
  -- Создадим диалог
  bracketCtrlBreak "ask_user" (messageDialogNew Nothing [] MessageQuestion ButtonsNone question) widgetDestroy $ \dialog -> do
  set dialog [windowTitle          := title,
              windowWindowPosition := WinPosCenter]
{-
  -- Запрос к пользователю
  upbox <- dialogGetUpper dialog
  label <- labelNew$ Just$ question++"?"
  boxPackStart  upbox label PackGrow 0
  widgetShowAll upbox
-}
  -- Кнопки для всех возможных ответов
  hbox <- dialogGetActionArea dialog
  buttonBox <- tableNew 3 3 True
  boxPackStart hbox buttonBox PackGrow 0
  id' <- ref 1
  for (zip [0..] buttons) $ \(y,line) -> do
    for (zip [0..] (split '/' line)) $ \(x,text) -> do
      when (text>"") $ do
      text <- i18n text
      button <- buttonNewWithMnemonic ("  "++text++"  ")
      tableAttachDefaults buttonBox button x (x+1) y (y+1)
      id <- val id'; id' += 1
      dialogAddActionWidget dialog button (ResponseUser id)
  widgetShowAll hbox

  -- Получить ответ в виде буквы: y/n/a/...
  (ResponseUser id) <- pauseEverything$ dialogRun dialog
  let answer = (split '/' valid_answers) !! (id-1)
  when (answer=="q") $ do
    terminateOperation
  return answer


-- Ответы, возвращаемые ask_user, и соответствующие им надписи на кнопках, построчно
valid_answers = "y/n/q/a/s/u"
buttons       = ["0079 _Yes/0080 _No/0081 _Cancel"
                ,"0082 Yes to _All/0083 No to A_ll/0084 _Update all"]


----------------------------------------------------------------------------------------------------
---- Запрос паролей --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Запрос пароля при шифровании/дешифровании. Используется невидимый ввод.
-- При шифровании пароль надо ввести дважды - для защиты от ошибок при вводе
ask_passwords = ( ask_password_dialog "0076 Enter encryption password" 2
                , ask_password_dialog "0077 Enter decryption password" 1
                , doNothing0 :: IO ()  -- вызывается при неправильном пароле
                )

-- |Диалог запроса пароля.
ask_password_dialog title' amount opt_parseData = gui $ do
  -- Создадим диалог со стандартными кнопками OK/Cancel
  bracketCtrlBreak "ask_password_dialog" dialogNew widgetDestroy $ \dialog -> do
  title <- i18n title'
  set dialog [windowTitle          := title,
              windowWindowPosition := WinPosCenter]
  okButton <- addStdButton dialog ResponseOk
  addStdButton dialog ResponseCancel

  -- Создаёт таблицу с полями для ввода одного или двух паролей
  (pwdTable, pwds@[pwd1,pwd2]) <- pwdBox amount
  -- Процедура прверки правильности введённых паролей
  let validate = do [pwd1', pwd2'] <- mapM val pwds
                    return (pwd1'>"" && pwd1'==pwd2')
  for pwds (`onEntryActivate` whenM validate (buttonClicked okButton))
  for pwds $ \pwd -> do
    pwd `onEditableChanged` (validate >>= widgetSetSensitivity okButton)
  okButton `widgetSetSensitivity` False

  -- Добавим пробелы вокруг таблицы и кинем её на форму
  set pwdTable [containerBorderWidth := 10]
  upbox <- dialogGetUpper dialog
  boxPackStart  upbox pwdTable PackGrow 0
  widgetShowAll upbox

  fix $ \reenter -> do
  choice <- pauseEverything$ dialogRun dialog
  if choice==ResponseOk
    then do ok <- validate
            if ok  then val pwd1  else reenter
    else terminateOperation >> return ""


{-# NOINLINE ask_passwords #-}

-- |Создаёт таблицу с полями для ввода одного или двух паролей
pwdBox amount = do
  pwdTable <- tableNew 2 amount False
  tableSetColSpacings pwdTable 0
  let newField y s = do -- Надписи в левом столбце
                        label <- labelNewWithMnemonic =<< i18n s
                        tableAttach pwdTable label 0 1 (y-1) y [Fill] [Expand, Fill] 5 0
                        miscSetAlignment label 0 0.5
                        -- Поля ввода пароля в правом столбце
                        pwd <- entryNew
                        set pwd [entryVisibility := False, entryActivatesDefault := True]
                        tableAttach pwdTable pwd 1 2 (y-1) y [Expand, Shrink, Fill] [Expand, Fill] 5 0
                        return pwd
  pwd1 <- newField 1 "0074 Enter password:"
  pwd2 <- if amount==2  then newField 2 "0075 Reenter password:"  else return pwd1
  return (pwdTable, [pwd1,pwd2])


----------------------------------------------------------------------------------------------------
---- Ввод/вывод комментариев к архиву  -------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE uiPrintArcComment #-}
uiPrintArcComment = doNothing

{-# NOINLINE uiInputArcComment #-}
uiInputArcComment old_comment = gui$ do
  bracketCtrlBreak "uiInputArcComment" dialogNew widgetDestroy $ \dialog -> do
  title <- i18n"0073 Enter archive comment"
  set dialog [windowTitle := title,
              windowDefaultHeight := 200, windowDefaultWidth := 400,
              windowWindowPosition := WinPosCenter]
  addStdButton dialog ResponseOk
  addStdButton dialog ResponseCancel

  commentTextView <- newTextViewWithText old_comment
  upbox <- dialogGetUpper dialog
  boxPackStart upbox commentTextView PackGrow 10
  widgetShowAll upbox

  choice <- pauseEverything$ dialogRun dialog
  if choice==ResponseOk
    then textViewGetText commentTextView
    else terminateOperation >> return ""


----------------------------------------------------------------------------------------------------
---- Библиотека ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Выполнить операцию в GUI-треде
gui action = do
  gui <- val guiThread
  my  <- getOsThreadId
  if my==gui  then action else do
  x <- ref Nothing
  y <- postGUISync (action `catch` (\e -> do x=:Just e; return undefined))
  whenJustM (val x) throwIO
  return y

-- |Глобальная переменная, хранящая тултипы контролов на текущей форме
tooltips :: IORef Tooltips = unsafePerformIO$ ref$ error "undefined GUI::tooltips"
tooltip w s = do s <- i18n s; t <- val tooltips; tooltipsSetTip t w s ""

-- |Создать контрол, дав ему локализованную надпись и тултип
i18t title create = do
  (label, t) <- i18n' title
  control <- create label
  tooltip control t  `on_`  t/=""
  return control

-- |This instance allows to get/set radio button state using standard =:/val interface
instance Variable RadioButton Bool where
  new  = undefined
  val  = toggleButtonGetActive
  (=:) = toggleButtonSetActive

-- |This instance allows to get/set toggle button state using standard =:/val interface
instance Variable ToggleButton Bool where
  new  = undefined
  val  = toggleButtonGetActive
  (=:) = toggleButtonSetActive

-- |This instance allows to get/set checkbox state using standard =:/val interface
instance Variable CheckButton Bool where
  new  = undefined
  val  = toggleButtonGetActive
  (=:) = toggleButtonSetActive

-- |This instance allows to get/set entry state using standard =:/val interface
instance Variable Entry String where
  new  = undefined
  val  = entryGetText
  (=:) = entrySetText

-- |This instance allows to get/set expander state using standard =:/val interface
instance Variable Expander Bool where
  new  = undefined
  val  = expanderGetExpanded
  (=:) = expanderSetExpanded

-- |This instance allows to get/set expander state using standard =:/val interface
instance Variable TextView String where
  new  = undefined
  val  = textViewGetText
  (=:) = textViewSetText

-- |This instance allows to get/set value displayed by widget using standard =:/val interface
instance GtkWidgetClass w gw a => Variable w a where
  new  = undefined
  val  = getValue
  (=:) = setValue

-- |Universal interface to arbitrary GTK widget `w` that controls value of type `a`
class GtkWidgetClass w gw a | w->gw, w->a where
  widget        :: w -> gw                 -- ^The GTK widget by itself
  getTitle      :: w -> IO String          -- ^Read current widget's title
  setTitle      :: w -> String -> IO ()    -- ^Set current widget's title
  getValue      :: w -> IO a               -- ^Read current widget's value
  setValue      :: w -> a -> IO ()         -- ^Set current widget's value
  setOnUpdate   :: w -> (IO ()) -> IO ()   -- ^Called when user changes widget's value
  onClick       :: w -> (IO ()) -> IO ()   -- ^Called when user clicks button
  saveHistory   :: w -> IO ()
  rereadHistory :: w -> IO ()

data GtkWidget gw a = GtkWidget
 {gwWidget        :: gw
 ,gwGetTitle      :: IO String
 ,gwSetTitle      :: String -> IO ()
 ,gwGetValue      :: IO a
 ,gwSetValue      :: a -> IO ()
 ,gwSetOnUpdate   :: (IO ()) -> IO ()
 ,gwOnClick       :: (IO ()) -> IO ()
 ,gwSaveHistory   :: IO ()
 ,gwRereadHistory :: IO ()
 }

instance GtkWidgetClass (GtkWidget gw a) gw a where
  widget        = gwWidget
  getTitle      = gwGetTitle
  setTitle      = gwSetTitle
  getValue      = gwGetValue
  setValue      = gwSetValue
  setOnUpdate   = gwSetOnUpdate
  onClick       = gwOnClick
  saveHistory   = gwSaveHistory
  rereadHistory = gwRereadHistory

-- |Пустой GtkWidget
gtkWidget = GtkWidget { gwWidget        = undefined
                      , gwGetTitle      = undefined
                      , gwSetTitle      = undefined
                      , gwGetValue      = undefined
                      , gwSetValue      = \_ -> return ()
                      , gwSetOnUpdate   = undefined
                      , gwOnClick       = undefined
                      , gwSaveHistory   = undefined
                      , gwRereadHistory = undefined
                      }

-- Использовать жирный Pango Markup для переданного текста
bold text = "<b>"++text++"</b>"


{-# NOINLINE eventKey #-}
-- |Возвращает полное имя клавиши, например <Alt><Ctrl>M
eventKey (Key {eventKeyName = name, eventModifier = modifier}) =
  let mshow Shift   = "<Shift>"
      mshow Control = "<Ctrl>"
      mshow Alt     = "<Alt>"
      mshow _       = "<_>"
  --
  in concat ((sort$ map mshow modifier)++[mapHead toUpper name])


{-# NOINLINE addStdButton #-}
-- |Добавить к диалогу стандартную кнопку со стандартной иконкой
addStdButton dialog responseId = do
  let (emsg,item) = case responseId of
                      ResponseYes            -> ("0079 _Yes",    stockYes         )
                      ResponseNo             -> ("0080 _No",     stockNo          )
                      ResponseOk             -> ("0362 _OK",     stockOk          )
                      ResponseCancel         -> ("0081 _Cancel", stockCancel      )
                      ResponseClose          -> ("0364 _Close",  stockClose       )
                      x | x==aResponseDetach -> ("0432 _Detach", stockMissingImage)
                      _                      -> ("???",          stockMissingImage)
  msg <- i18n emsg
  button <- dialogAddButton dialog msg responseId
#if defined(FREEARC_UNIX)
  hbox <- dialogGetActionArea dialog
  boxReorderChild hbox button 0        -- according to HIG, on Linux dialogs should have reverse button order compared to Windows
#endif
  image  <- imageNewFromStock item IconSizeButton
  buttonSetImage button image
  return button

-- |Кнопка фонового выполнения команды
aResponseDetach = ResponseUser 1


{-# NOINLINE debugMsg #-}
-- |Диалог с отладочным сообщением
debugMsg msg = do
  bracketCtrlBreak "debugMsg" (messageDialogNew (Nothing) [] MessageError ButtonsClose msg) widgetDestroy $ \dialog -> do
  dialogRun dialog
  return ()

-- |Диалог с информационным сообщением
msgBox window dialogType msg  =  askConfirmation [ResponseClose] window msg  >>  return ()

-- |Запросить у пользователя подтверждение операции
askOkCancel = askConfirmation [ResponseOk,  ResponseCancel]
askYesNo    = askConfirmation [ResponseYes, ResponseNo]
{-# NOINLINE askConfirmation #-}
askConfirmation buttons window msg = do
  -- Создадим диалог с единственной кнопкой Close
  bracketCtrlBreak "askConfirmation" dialogNew widgetDestroy $ \dialog -> do
    set dialog [windowTitle        := aARC_NAME,
                windowTransientFor := window,
                containerBorderWidth := 10]
    mapM_ (addStdButton dialog) buttons
    -- Напечатаем в нём сообщение
    label <- labelNew.Just =<< i18n msg
    upbox <- dialogGetUpper dialog
    label `set` [labelWrap := True]
    boxPackStart  upbox label PackGrow 20
    widgetShowAll upbox
    -- И запустим
    dialogRun dialog >>== (==buttons!!0)

{-# NOINLINE inputString #-}
-- |Запросить у пользователя строку
inputString window msg = do
  -- Создадим диалог со стандартными кнопками OK/Cancel
  bracketCtrlBreak "inputString" dialogNew widgetDestroy $ \dialog -> do
    set dialog [windowTitle        := msg,
                windowTransientFor := window]
    addStdButton dialog ResponseOk      >>= \okButton -> do
    addStdButton dialog ResponseCancel

    --label    <- labelNew$ Just msg
    entry <- entryNew
    entry `onEntryActivate` buttonClicked okButton

    upbox <- dialogGetUpper dialog
    --boxPackStart  upbox label    PackGrow 0
    boxPackStart  upbox entry PackGrow 0
    widgetShowAll upbox

    choice <- dialogRun dialog
    case choice of
      ResponseOk -> val entry >>== Just
      _          -> return Nothing


{-# NOINLINE boxed #-}
-- |Создать control и поместить его в hbox
boxed makeControl title = do
  hbox    <- hBoxNew False 0
  control <- makeControl .$i18t title
  boxPackStart  hbox  control  PackNatural 0
  return (hbox, control)


{-# NOINLINE label #-}
-- |Метка
label title = do
  (hbox, _) <- boxed labelNewWithMnemonic title
  return gtkWidget { gwWidget      = hbox
                   }


{-# NOINLINE button #-}
-- |Кнопка
button title  =  do
  (hbox, control) <- boxed buttonNewWithMnemonic title
  return gtkWidget { gwWidget   = hbox
                   , gwOnClick  = \action -> onClicked control action >> return ()
                   , gwSetTitle = buttonSetLabel control
                   , gwGetTitle = buttonGetLabel control
                   }


{-# NOINLINE checkBox #-}
-- |Чекбокс
checkBox title = do
  (hbox, control) <- boxed checkButtonNewWithMnemonic title
  return gtkWidget { gwWidget      = hbox
                   , gwGetValue    = val control
                   , gwSetValue    = (control=:)
                   , gwSetOnUpdate = \action -> onToggled control action >> return ()
                   }


{-# NOINLINE expander #-}
-- |Экспандер
expander title = do
  (hbox, control) <- boxed expanderNewWithMnemonic title
  innerBox <- vBoxNew False 0
  containerAdd control innerBox
  return (gtkWidget { gwWidget      = hbox
                    , gwGetValue    = val control
                    , gwSetValue    = (control=:)
                    , gwSetOnUpdate = \action -> onSizeAllocate control (\size -> action)  >> return ()
                    }
         ,innerBox)


{-# NOINLINE comboBox #-}
-- |Создаёт комбобокс, содержащий заданный набор альтернатив
comboBox title labels = do
  hbox  <- hBoxNew False 0
  label <- labelNewWithMnemonic .$i18t title
  combo <- New.comboBoxNewText
  for labels (\l -> New.comboBoxAppendText combo =<< i18n l)
  boxPackStart  hbox  label  PackNatural 5
  boxPackStart  hbox  combo  PackGrow    5
  widgetSetSizeRequest combo 10 (-1)           -- уменьшим размер кобо-бокса, посольку иначе диалог сжатия становится слишком широк
  return gtkWidget { gwWidget      = hbox
                   , gwGetValue    = New.comboBoxGetActive combo
                   , gwSetValue    = New.comboBoxSetActive combo
                   , gwSetOnUpdate = \action -> on combo changed action  >> return ()
                   }


{-# NOINLINE simpleComboBox #-}
-- |Создаёт комбобокс, содержащий заданный набор альтернатив
simpleComboBox labels = do
  combo <- New.comboBoxNewText
  for labels (New.comboBoxAppendText combo)
  return combo

{-# NOINLINE makePopupMenu #-}
-- |Создаёт popup menu
makePopupMenu action labels = do
  m <- menuNew
  mapM_ (mkitem m) labels
  return m
    where
        mkitem menu label =
            do i <- menuItemNewWithLabel label
               menuShellAppend menu i
               i `onActivateLeaf` (action label)



{-# NOINLINE radioFrame #-}
-- |Создаёт фрейм, содержащий набор радиокнопок и возвращает этот фрейм
--  плюс процедуру для чтения текущей выбранной кнопки
radioFrame title (label1:labels) = do
  -- Создать радио-кнопки, объединив их в одну группу
  radio1 <- radioButtonNewWithMnemonic .$i18t label1
  radios <- mapM (\title -> radioButtonNewWithMnemonicFromWidget radio1 .$i18t title) labels
  let buttons = radio1:radios
  -- Упаковать их вертикально и заставить выполнять событие, запомненное в переменной onChanged
  vbox <- vBoxNew False 0
  onChanged <- ref doNothing0
  for buttons $ \button -> do boxPackStart vbox button PackNatural 0
                              button `onToggled` do
                                whenM (val button) $ do
                                  val onChanged >>= id
  -- Создать рамочку вокруг кнопок
  frame <- i18t title $ \title -> do
             frame <- frameNew
             set frame [frameLabel := title.$ deleteIf (=='_'), containerChild := vbox]
             return frame
  return gtkWidget { gwWidget      = frame
                   , gwGetValue    = foreach buttons val >>== fromJust.elemIndex True
                   , gwSetValue    = \i -> (buttons!!i) =: True
                   , gwSetOnUpdate = (onChanged=:)
                   }


{-# NOINLINE twoColumnTable #-}
-- |Двухколоночная таблица, отображающая заданные метки+данные
twoColumnTable dataset = do
  (table, setLabels) <- emptyTwoColumnTable$ map fst dataset
  zipWithM_ ($) setLabels (map snd dataset)
  return table

{-# NOINLINE emptyTwoColumnTable #-}
-- |Двухколоночная таблица: принимает список меток для левой колонки
-- и возвращает список операций setLabels для помещения данных во вторую колонку
emptyTwoColumnTable dataset = do
  table <- tableNew (length dataset) 2 False
  -- Создадим поля для вывода текущей статистики и нарисуем метки к ним
  setLabels <- foreach (zip [0..] dataset) $ \(y,s) -> do
      -- Первая колонка
      label <- labelNewWithMnemonic =<< i18n s;  let x=0
      tableAttach table label (x+0) (x+1) y (y+1) [Expand, Fill] [Expand, Fill] 0 0
      miscSetAlignment label 0 0     --set label [labelWidthChars := 25]
      -- Вторая колонка
      label <- labelNew Nothing
      tableAttach table label (x+1) (x+2) y (y+1) [Expand, Fill] [Expand, Fill] 10 0
      set label [labelSelectable := True]
      miscSetAlignment label 1 0
      -- Возвратим операцию, устанавливающую текст второй метки (предназначенной для вывода данных)
      return$ \text -> labelSetMarkup label$ bold$ text
  return (table, setLabels)

{-# NOINLINE scrollableTextView #-}
-- |Прокручиваемый TextView
scrollableTextView s attributes = do
  control <- newTextViewWithText s
  set control attributes
  -- Scrolled window where the TextView will be placed
  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
  containerAdd scrwin control
  return gtkWidget { gwWidget   = scrwin
                   , gwGetValue = val control
                   , gwSetValue = (control=:)
                   }

-- |Создаёт новый объект TextView с заданным текстом
newTextViewWithText s = do
  textView <- textViewNew
  textViewSetText textView s
  return textView

-- |Задаёт текст, отображаемый в TextView
textViewSetText textView s = do
  buffer <- textViewGetBuffer textView
  textBufferSetText buffer s

-- |Считывает текст, отображаемый в TextView
textViewGetText textView = do
  buffer <- textViewGetBuffer      textView
  start  <- textBufferGetStartIter buffer
  end    <- textBufferGetEndIter   buffer
  textBufferGetText buffer start end False


----------------------------------------------------------------------------------------------------
---- Выбор файла -----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

#if defined(FREEARC_WIN)

{-# NOINLINE chooseFile #-}
-- |Выбор файла через диалог
chooseFile parentWindow dialogType dialogTitle filters getFilename setFilename = do
  title <- i18n dialogTitle
  filename <- getFilename
  -- Строка фильтров состоит из пар (название,шаблоны), разделённых NULL char, плюс дополнительный NULL char в конце
  filterStr <- prepareFilters filters >>== map (join2 "\0") >>== joinWith "\0" >>== (++"\0")
  withCFilePath title            $ \c_prompt   -> do
  withCFilePath filename         $ \c_filename -> do
  withCFilePath filterStr        $ \c_filters  -> do
  allocaBytes (long_path_size*4) $ \c_outpath  -> do
    result <- case dialogType of
                FileChooserActionSelectFolder  ->  c_BrowseForFolder c_prompt c_filename c_outpath
                _                              ->  c_BrowseForFile   c_prompt c_filters c_filename c_outpath
    when (result/=0) $ do
       setFilename =<< peekCFilePath c_outpath

foreign import ccall safe "Environment.h BrowseForFolder"  c_BrowseForFolder :: CFilePath -> CFilePath -> CFilePath -> IO CInt
foreign import ccall safe "Environment.h BrowseForFile"    c_BrowseForFile   :: CFilePath -> CFilePath -> CFilePath -> CFilePath -> IO CInt


guiFormatDateTime t = unsafePerformIO $ do
  allocaBytes 1000 $ \buf -> do
  c_GuiFormatDateTime t buf 1000 nullPtr nullPtr
  peekCString buf

foreign import ccall safe "Environment.h GuiFormatDateTime"
  c_GuiFormatDateTime :: CTime -> CString -> CInt -> CString -> CString -> IO ()

#else

{-# NOINLINE chooseFile #-}
-- |Выбор файла через диалог
chooseFile parentWindow dialogType dialogTitle filters getFilename setFilename = do
  title <- i18n dialogTitle
  filename <- getFilename
  [select,cancel] <- i18ns ["0363 _Select", "0081 _Cancel"]
  bracketCtrlBreak "chooseFile" (fileChooserDialogNew (Just title) (Just$ castToWindow parentWindow) dialogType [(select,ResponseOk), (cancel,ResponseCancel)]) widgetDestroy $ \chooserDialog -> do
    fileChooserSetFilename chooserDialog (unicode2utf8 filename)
    case dialogType of
      FileChooserActionSave -> fileChooserSetCurrentName chooserDialog (takeFileName filename)
      _                     -> fileChooserSetFilename    chooserDialog (unicode2utf8 filename++"/non-existing-file") >> return ()
    prepareFilters filters >>= addFilters chooserDialog
    choice <- dialogRun chooserDialog
    when (choice==ResponseOk) $ do
      whenJustM_ (fileChooserGetFilename chooserDialog) $ \filename -> do
        setFilename (utf8_to_unicode filename)

{-# NOINLINE addFilters #-}
-- |Установить фильтры для выбора файла
addFilters chooserDialog filters = do
  for filters $ \(text, patterns) -> do
    filt <- fileFilterNew
    fileFilterSetName filt text
    for (patterns.$ split ';')  (fileFilterAddPattern filt)
    fileChooserAddFilter chooserDialog filt

guiFormatDateTime = formatDateTime

#endif


-- |Подготовить фильтры к использованию в диалоге
prepareFilters filters = do
  foreach (filters &&& filters++["0309 All files (*)"]) $ \element -> do
    text <- i18n element
    let patterns = text .$words .$last .$drop 1 .$dropEnd 1
    return (text, patterns)
