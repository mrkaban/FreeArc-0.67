----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: file manager panel                                              ------
----------------------------------------------------------------------------------------------------
module FileManPanel where

import Prelude hiding (catch)
import Control.Concurrent
import Control.OldException
import Control.Monad
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Time

import System.Glib.GObject
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets
import Options
import Cmdline
import UIBase
import UI
import Arhive7zLib
import ArhiveDirectory
import FileManUtils


----------------------------------------------------------------------------------------------------
---- Операции файл-менеджера -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Создать фиктивную панель файл-менеджера (для прямого вызова диалогов из ком. строки)
newEmptyFM = do
  history <- openHistoryFile
  curdir <- getCurrentDirectory
  fm' <- mvar FM_State { fm_window_      = Nothing
                       , fm_view         = error "undefined FM_State::fm_view"
                       , fm_model        = error "undefined FM_State::fm_model"
                       , fm_selection    = error "undefined FM_State::fm_selection"
                       , fm_statusLabel  = error "undefined FM_State::fm_statusLabel"
                       , fm_messageCombo = error "undefined FM_State::fm_messageCombo"
                       , fm_filelist     = error "undefined FM_State::fm_filelist"
                       , fm_history      = history
                       , fm_onChdir      = []
                       , fm_sort_order   = ""
                       , fm_passwords    = []
                       , subfm           = FM_Directory {subfm_dir=curdir}}
  return fm'

-- |Создать переменную для хранения состояния файл-менеджера
newFM window view model selection statusLabel messageCombo = do
  fm' <- newEmptyFM
  messageCounter <- ref 0  -- number of last message + 1 in combobox
  fm' .= (\fm -> fm { fm_window_      = Just window
                    , fm_view         = view
                    , fm_model        = model
                    , fm_selection    = selection
                    , fm_statusLabel  = statusLabel
                    , fm_messageCombo = (messageCombo, messageCounter)})
  fm'       `fmOnChdir`              fmStatusBarTotals fm'
  selection `New.onSelectionChanged` fmStatusBarTotals fm'
  return fm'

-- |Открыть архив и возвратить его как объект состояния файл-менеджера
newFMArc fm' arcname arcdir = do
  decryptionOptions <- fmGetDecryptionOptions fm'
  [command]  <- parseCmdline$ ["l", arcname]++decryptionOptions
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- подготовить пароли в команде к использованию
  archive <- archiveReadInfo command "" "" (const True) doNothing2 arcname
  fmSaveDecryptionPasswords fm' command
  let filetree = buildTree$ map (fiToFileData.cfFileInfo)$ arcDirectory archive
  arcClose archive
  return$ FM_Archive archive arcname arcdir filetree

-- |Закрыть файл архива чтобы 1) другие операции смогли модифицировать его,
--    2) его содержимое было перечитано заново при следующем использовании
closeFMArc fm' = do
  fm <- val fm'
  --arcClose (fm_archive fm)
  when (isFM_Archive fm) $ do
    fm' .= \fm -> fm {subfm = (subfm fm) {subfm_archive = phantomArc}}

-- Перейти в архив/каталог filename
fmChdir fm' filename' = do
  fm <- val fm'
  filename <- fmCanonicalizePath fm' filename'
  res <- splitArcPath fm' filename
  msg <- i18n"0071 %1: no such file or directory!"
  if res==Not_Exists  then fmErrorMsg fm' (format msg filename)  else do
  (files, sub) <- case res of
    -- Список файлов в каталоге на диске
    DiskPath dir -> do filelist <- dir_list dir
                       return (map fiToFileData filelist, FM_Directory dir)
    -- Список файлов в архиве
    ArcPath arcname arcdir -> do
                       arc <- if isFM_Archive fm && arcname==fm_arcname fm && not (isArcPhantom (fm_archive fm))
                                then return ((fm.$subfm) {subfm_arcdir=arcdir})
                                else newFMArc fm' arcname arcdir
                       -- Если arcdir - имя существующего файла внутри архива, то перейти в него нельзя :)
                       let filedata = ftFind arcdir (subfm_filetree arc)
                           is_dir   = isNothing filedata  ||  fdIsDir (fromJust filedata)
                       return$ if is_dir  then (arc.$subfm_filetree.$ftFilesIn arcdir fdArtificialDir, arc)  else undefined
  -- Запишем текущий каталог/архив в fm и выведем на экран новый список файлов
  fm' .= \fm -> fm {subfm = sub}
  fmSetFilelist fm' (files.$ sortOnColumn (fm_sort_order fm))
  -- Обновим статусбар и выполним все остальные запрограммированные действия.
  sequence_ (fm_onChdir fm)
  widgetGrabFocus (fm_view fm)

-- Отобразить изменение имени архива
fmChangeArcname fm' newname = do
  fm' .= fm_changeArcname newname
  fm <- val fm'
  sequence_ (fm_onChdir fm)

-- |Добавить action в список операций, выполняемых при переходе в другой каталог/архив
fmOnChdir fm' action = do
  fm' .= \fm -> fm {fm_onChdir = action : fm_onChdir fm}

-- |Вывести в строку сообщений информацию об общем объёме файлов и сколько из них выбрано
fmStatusBarTotals fm' = do
  fm <- val fm'
  selected <- getSelectionFileInfo fm'
  [sel,total] <- i18ns ["0022 Selected %1 bytes in %2 file(s)", "0023 Total %1 bytes in %2 file(s)"]
  let format msg files  =  formatn msg [show3$ sum$ map fdSize files,  show3$ length files]
  fmStatusBarMsg fm' $ (selected &&& (format sel   selected++"     "))
                                 ++   format total (fm_filelist fm)

-- |Вывести информацию в статус-строку
fmStatusBarMsg fm' msg = do
  fm <- val fm'
  labelSetText (fm_statusLabel fm) msg
  return ()

-- |Добавить сообщение в pop-up список сообщений
fmStackMsg fm' emsg = do
  fm <- val fm'
  let (box,n')  =  fm_messageCombo fm
  n <- val n';  n' += 1
  current_time <- getClockTime
  let timestr = showtime "%H:%M:%S " current_time
  imsg <- i18n emsg
  New.comboBoxAppendText box (imsg &&& (timestr++imsg))
  New.comboBoxSetActive  box n
  return ()

-- |Имя файла, находящегося по заданному пути
fmFilenameAt fm' path  =  fmname `fmap` fmFileAt fm' path

-- |Файл, находящийся по заданному пути
fmFileAt fm' path = do
  fm <- val fm'
  let fullList = fm_filelist fm
  return$ fullList!!head path

-- |Возвратить файл под курсором
fmGetCursor fm' = do
  fm <- val fm'
  let fullList  = fm_filelist  fm
  (cursor,_) <- New.treeViewGetCursor (fm_view fm)
  case cursor of
    [i] -> return (fdBasename$ fullList!!i)
    _   -> return ""

-- |Установить курсор на заданный файл
fmSetCursor fm' filename = do
  fm <- val fm'
  whenJustM_ (fmFindCursor fm' filename)
             (\cursor -> New.treeViewSetCursor (fm_view fm) cursor Nothing)

-- |Возвратить курсор для файла с заданным именем
fmFindCursor fm' filename = do
  fm <- val fm'
  let fullList  =  fm_filelist  fm
  return (fmap (:[])$  findIndex ((filename==).fmname) fullList)

-- |Вывести на экран новый список файлов
fmSetFilelist fm' orig_files = do
  showHiddenFiles <- fmGetHistoryBool fm' "ShowHiddenFiles" False
  let file_is_hidden fd | isWindows =  (fdAttr fd .&. (aFI_ATTR_HIDDEN .|. aFI_ATTR_SYSTEM) /= 0)
                        | otherwise =  anyf [beginWith ".", endWith "~"] (fdBasename fd)
      files = orig_files.$ (not showHiddenFiles &&& filter (not.file_is_hidden))
  fm <- val fm'
  fm' =: fm {fm_filelist = files}
  changeList (fm_model fm) (fm_selection fm) files

-- |Вывести сообщение об ошибке
fmErrorMsg fm' msg = do
  fm <- val fm'
  msgBox (fm_window fm) MessageError msg

-- |Вывести информационное сообщение
fmInfoMsg fm' msg = do
  fm <- val fm'
  msgBox (fm_window fm) MessageInfo msg

-- |Опции расшифровки, добавляемые в командную строку
fmGetDecryptionOptions fm' = do
  passwords <- fmGetDecryptionPasswords fm'
  keyfile   <- fmGetHistory1 fm' "keyfile" ""
  return$ (map ("-op"++) passwords)++
          (keyfile  &&&  ["--OldKeyfile="++clear keyfile])


----------------------------------------------------------------------------------------------------
---- Выделение файлов ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Отметить/разотметить файлы, удовлетворяющие заданному предикату
fmSelectFilenames   = fmSelUnselFilenames New.treeSelectionSelectRange
fmUnselectFilenames = fmSelUnselFilenames New.treeSelectionUnselectRange
fmSelUnselFilenames selectOrUnselect fm' filter_p = do
  fm <- val fm'
  let fullList  = fm_filelist  fm
  let selection = fm_selection fm
  for (makeRanges$ findIndices filter_p fullList)
      (\(x,y) -> selectOrUnselect selection [x] [y])

-- |Отметить/разотметить все файлы
fmSelectAll   fm' = New.treeSelectionSelectAll   . fm_selection =<< val fm'
fmUnselectAll fm' = New.treeSelectionUnselectAll . fm_selection =<< val fm'

-- |Инвертировать выделение
fmInvertSelection fm' = do
  fm <- val fm'
  let files     = length$ fm_filelist fm
  let selection = fm_selection fm
  for [0..files-1] $ \i -> do
    selected <- New.treeSelectionPathIsSelected selection [i]
    (if selected  then New.treeSelectionUnselectPath  else New.treeSelectionSelectPath) selection [i]

-- |Список имён избранных файлов + имён каталогов в отображении mapDirName
getSelection fm' mapDirName = do
  let mapFilenames fd | fdIsDir fd = mapDirName$ fmname fd
                      | otherwise  = [fmname fd]
  getSelectionFileInfo fm' >>== concatMap mapFilenames

-- |Список FileInfo избранных файлов
getSelectionFileInfo fm' = do
  fm <- val fm'
  let fullList = fm_filelist fm
  getSelectionRows fm' >>== map (fullList!!)

-- |Список номеров избранных файлов
getSelectionRows fm' = do
  fm <- val fm'
  let selection = fm_selection fm
  New.treeSelectionGetSelectedRows selection >>== map head

-- |Удалить из модели выбранные файлы
fmDeleteSelected fm' = do
  rows <- getSelectionRows fm'
  fm <- val fm'
  fmSetFilelist fm' (fm_filelist fm `deleteElems` rows)     -- O(n^2)!


----------------------------------------------------------------------------------------------------
---- Сортировка списка файлов ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Возвратить текущий порядок сортировки
fmGetSortOrder fm'  =  fm_sort_order `fmap` val fm'
-- |Установить порядок сортировки
fmSetSortOrder fm' showSortOrder  =  fmModifySortOrder fm' showSortOrder . const
-- |Модифицировать порядок сортировки в fm' и показать индикатор сортировки над соответствующим столбцом
fmModifySortOrder fm' showSortOrder f_order = do
  fm <- val fm'
  let sort_order = f_order (fm_sort_order fm)
  fm' =: fm {fm_sort_order = sort_order}
  -- Модифицируем индикатор сортировки
  let (column, order)  =  break1 isUpper sort_order
  showSortOrder column (if order == "Asc"  then SortAscending  else SortDescending)

-- |Сохранить порядок сортировки в историю
fmSaveSortOrder    fm'  =  fmReplaceHistory fm' "SortOrder"
-- |Восстановить порядок сортировки из истории
fmRestoreSortOrder fm'  =  fmGetHistory1    fm' "SortOrder" "NameAsc"

-- | (ClickedColumnName, OldSortOrder) -> NewSortOrder
calcNewSortOrder "Name"     "NameAsc"      = "NameDesc"
calcNewSortOrder "Name"     _              = "NameAsc"
calcNewSortOrder "Size"     "SizeDesc"     = "SizeAsc"
calcNewSortOrder "Size"     _              = "SizeDesc"
calcNewSortOrder "Modified" "ModifiedDesc" = "ModifiedAsc"
calcNewSortOrder "Modified" _              = "ModifiedDesc"
calcNewSortOrder "Type"     "TypeAsc"      = "TypeDesc"
calcNewSortOrder "Type"     _              = "TypeAsc"

-- |Выбор функции сортировки по имени колонки
sortOnColumn "NameAsc"       =  sortOn (\fd -> (not$ fdIsDir fd, strLower$ fmname fd))
sortOnColumn "NameDesc"      =  sortOn (\fd -> (     fdIsDir fd, strLower$ fmname fd))  >>> reverse
--
sortOnColumn "SizeAsc"       =  sortOn (\fd -> if fdIsDir fd  then -1             else  fdSize fd)
sortOnColumn "SizeDesc"      =  sortOn (\fd -> if fdIsDir fd  then aFILESIZE_MIN  else -fdSize fd)
--
sortOnColumn "ModifiedAsc"   =  sortOn (\fd -> (not$ fdIsDir fd,  fdTime fd))
sortOnColumn "ModifiedDesc"  =  sortOn (\fd -> (not$ fdIsDir fd, -fdTime fd))
--
sortOnColumn "TypeAsc"       =  sortOn (\fd -> (not$ fdIsDir fd, strLower$ fdType fd))
sortOnColumn "TypeDesc"      =  sortOn (\fd -> (     fdIsDir fd, strLower$ fdType fd))  >>> reverse
--
sortOnColumn _               =  id


----------------------------------------------------------------------------------------------------
---- Операции с файлом истории ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

fmAddHistory         fm' tags text             =  do fm <- val fm';  hfAddHistory         (fm_history fm) tags text
fmReplaceHistory     fm' tags text             =  do fm <- val fm';  hfReplaceHistory     (fm_history fm) tags text
fmModifyHistory      fm' tags text deleteCond  =  do fm <- val fm';  hfModifyHistory      (fm_history fm) tags text deleteCond
fmGetHistory         fm' tags                  =  do fm <- val fm';  hfGetHistory         (fm_history fm) tags
fmGetHistory1        fm' tags deflt            =  do fm <- val fm';  hfGetHistory1        (fm_history fm) tags deflt
fmGetHistoryInt      fm' tag  deflt            =  do fm <- val fm';  hfGetHistoryInt      (fm_history fm) tag  deflt
fmGetHistoryBool     fm' tag  deflt            =  do fm <- val fm';  hfGetHistoryBool     (fm_history fm) tag  deflt
fmReplaceHistoryBool fm' tag  x                =  do fm <- val fm';  hfReplaceHistoryBool (fm_history fm) tag  x
fmReplaceHistoryInt  fm' tag  x                =  do fm <- val fm';  hfReplaceHistoryInt  (fm_history fm) tag  x
fmSaveSizePos        fm' dialog name           =  do fm <- val fm';  hfSaveSizePos        (fm_history fm) dialog name
fmSaveMaximized      fm' dialog name           =  do fm <- val fm';  hfSaveMaximized      (fm_history fm) dialog name
fmRestoreSizePos     fm' window name deflt     =  do fm <- val fm';  hfRestoreSizePos     (fm_history fm) window name deflt
fmCacheConfigFile    fm' action                =  do fm <- val fm';  hfCacheConfigFile    (fm_history fm) action
fmUpdateConfigFile   fm'                       =  do fm <- val fm';  hfUpdateConfigFile   (fm_history fm)


----------------------------------------------------------------------------------------------------
---- GUI controls, взаимодействующие с FM ----------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Комбинированный виджет, представляющий строку ввода с сохраняемой историей
data EntryWithHistory = EntryWithHistory
  { ehGtkWidget   :: GtkWidget ComboBoxEntry String
  , entry         :: Entry
  , changeTag     :: String -> IO ()
  }

instance GtkWidgetClass EntryWithHistory ComboBoxEntry String where
  widget        = widget        . ehGtkWidget
  getTitle      = getTitle      . ehGtkWidget
  setTitle      = setTitle      . ehGtkWidget
  getValue      = getValue      . ehGtkWidget
  setValue      = setValue      . ehGtkWidget
  setOnUpdate   = setOnUpdate   . ehGtkWidget
  onClick       = onClick       . ehGtkWidget
  saveHistory   = saveHistory   . ehGtkWidget
  rereadHistory = rereadHistory . ehGtkWidget


{-# NOINLINE fmEntryWithHistory #-}
-- |Создать комбо-бокс с историей под тегом tag (пустой тэг означает что мы его зададим позже, через changeTag);
-- перед запоминанием в истории пропускать введённый текст через операцию process
fmEntryWithHistory fm' tag filter_p process = do
  -- Create GUI controls
  comboBox <- New.comboBoxEntryNewText
  Just entry <- binGetChild comboBox >>== fmap castToEntry
  set entry [entryActivatesDefault := True]
  -- Define callbacks
  history'        <- mvar []
  tag'            <- mvar tag
  disableOnUpdate <- mvar False
  let readHistory = do
        tag <- val tag'
        history' .<- \oldHistory -> do
          replicateM_ (1+length oldHistory) (New.comboBoxRemoveText comboBox 0)
          history <- fmGetHistory fm' tag >>= Utils.filterM filter_p
          for history (New.comboBoxAppendText comboBox)
          return history
  let getText = do
        val entry >>= process
  let setText text = do
        entry =: text
  let saveHistory = do
        tag     <- val tag'
        when (tag>"") $ do
        text    <- getText
        history <- val history'
        last    <- fmGetHistory fm' (tag++"Last")
        let fixedOrder  =  (last>[])   -- True - keep order of dropdown "menu" elements fixed
        when fixedOrder $ do
          fmReplaceHistory fm' (tag++"Last") text
        unless (fixedOrder && (text `elem` history)) $ do
          New.comboBoxPrependText comboBox text
          fmAddHistory fm' tag text
  let changeTag save_old tag = do
        bracket_ (disableOnUpdate =: True) (disableOnUpdate =: False) $ do
        when save_old $ do
          saveHistory
        --
        tag' =: tag
        readHistory
        -- Установить текст в поле ввода
        last <- fmGetHistory fm' (tag++"Last")
        case last of
          last:_ -> entry =: last
          []     -> do history <- val history'
                       when (history > []) $ do
                         New.comboBoxSetActive comboBox 0
  --
  tag &&& changeTag False tag
  return EntryWithHistory
           {                           entry           = entry
           ,                           changeTag       = changeTag True
           , ehGtkWidget = gtkWidget { gwWidget        = comboBox
                                     , gwGetValue      = getText
                                     , gwSetValue      = setText
                                     , gwSetOnUpdate   = \action -> on comboBox changed (unlessM (val disableOnUpdate) action) >> return ()
                                     , gwSaveHistory   = saveHistory
                                     , gwRereadHistory = readHistory
                                     }
           }


{-# NOINLINE fmLabeledEntryWithHistory #-}
-- |Ввод строки с историей под тэгом tag и меткой слева
fmLabeledEntryWithHistory fm' tag title = do
  hbox  <- hBoxNew False 0
  title <- label title
  inputStr <- fmEntryWithHistory fm' tag (const$ return True) (return)
  boxPackStart  hbox  (widget title)     PackNatural 0
  boxPackStart  hbox  (widget inputStr)  PackGrow    5
  return (hbox, inputStr)

-- |Ввод строки с историей под тэгом tag и чекбоксом слева
fmCheckedEntryWithHistory fm' tag title  =  fmCheckedEntryWithHistory2 fm' tag False title

{-# NOINLINE fmCheckedEntryWithHistory2 #-}
-- |Ввод строки с историей под тэгом tag и чекбоксом с историей слева
fmCheckedEntryWithHistory2 fm' tag deflt title = do
  hbox  <- hBoxNew False 0
  checkBox <- checkBox title
  let checkBoxTag = tag++".Enabled"
  let rereadHistory = do
        checkBox =:: fmGetHistoryBool fm' checkBoxTag deflt
  let saveHistory = do
        fmReplaceHistoryBool fm' checkBoxTag =<< val checkBox
  rereadHistory
  let checkBoxWithHistory  =  checkBox { gwSaveHistory   = saveHistory
                                       , gwRereadHistory = rereadHistory
                                       }
  inputStr <- fmEntryWithHistory fm' tag (const$ return True) (return)
  boxPackStart  hbox  (widget checkBox)  PackNatural 0
  boxPackStart  hbox  (widget inputStr)  PackGrow    5
  setOnUpdate inputStr (checkBox =: True)
  --checkBox `onToggled` do
  --  on <- val checkBox
  --  (if on then widgetShow else widgetHide) (widget inputStr)
  return (hbox, checkBoxWithHistory, inputStr)

-- |Ввод строки с историей под тэгом tag и доп. выбором через переданный диалог
{-# NOINLINE fmCheckedEntryWithHistoryAndChooser #-}
fmCheckedEntryWithHistoryAndChooser fm' tag makeControl filter_p process chooserDialog = do
  hbox     <- hBoxNew False 0
  control  <- makeControl
  inputStr <- fmEntryWithHistory fm' tag filter_p process
  chooserButton <- button "9999 ..."
  chooserButton `onClick` do
    chooserDialog (val inputStr) (inputStr =:)
  boxPackStart  hbox  (widget control)        PackNatural 0
  boxPackStart  hbox  (widget inputStr)       PackGrow    5
  boxPackStart  hbox  (widget chooserButton)  PackNatural 0
  setOnUpdate inputStr (control =: True)
  return (hbox, control, inputStr)

{-# NOINLINE fmFileBox #-}
-- |Ввод имени файла/каталога с историей под тэгом tag и поиском по диску через вызываемый диалог
fmFileBox fm' dialog tag dialogType makeControl dialogTitle filters filter_p process = do
  fmCheckedEntryWithHistoryAndChooser fm' tag makeControl filter_p process (chooseFile dialog dialogType dialogTitle filters)

{-# NOINLINE fmInputString #-}
-- |Запросить у пользователя строку (с историей ввода)
fmInputString fm' tag title filter_p process = do
  fm <- val fm'
  -- Создадим диалог со стандартными кнопками OK/Cancel
  fmDialog fm' title [] $ \(dialog,okButton) -> do
    x <- fmEntryWithHistory fm' tag filter_p process

    upbox <- dialogGetUpper dialog
    --boxPackStart  upbox label    PackGrow 0
    boxPackStart  upbox (widget x) PackGrow 0
    widgetShowAll upbox

    choice <- dialogRun dialog
    case choice of
      ResponseOk -> do saveHistory x; val x >>== Just
      _          -> return Nothing

{-# NOINLINE fmCheckButtonWithHistory #-}
-- |Создать чекбокс с историей под тегом tag
fmCheckButtonWithHistory fm' tag deflt title = do
  control <- checkBox title
  let rereadHistory = do
        control =:: fmGetHistoryBool fm' tag deflt
  let saveHistory = do
        fmReplaceHistoryBool fm' tag =<< val control
  rereadHistory
  return$ control
           { gwSaveHistory   = saveHistory
           , gwRereadHistory = rereadHistory
           }

{-# NOINLINE fmExpanderWithHistory #-}
-- |Создать экспандер с историей под тегом tag
fmExpanderWithHistory fm' tag deflt title = do
  (control,innerBox) <- expander title
  let rereadHistory = do
        control =:: fmGetHistoryBool fm' tag deflt
  let saveHistory = do
        fmReplaceHistoryBool fm' tag =<< val control
  rereadHistory
  return$ (control { gwSaveHistory   = saveHistory
                   , gwRereadHistory = rereadHistory
                   }
          ,innerBox)

{-# NOINLINE fmComboBoxWithHistory #-}
-- |Создать комбобокс с историей под тегом tag
fmComboBoxWithHistory fm' tag deflt title variants = do
  control <- comboBox title variants
  let rereadHistory = do
        control =:: fmGetHistoryInt fm' tag deflt
  let saveHistory = do
        fmReplaceHistoryInt fm' tag =<< val control
  rereadHistory
  return$ control
           { gwSaveHistory   = saveHistory
           , gwRereadHistory = rereadHistory
           }

{-# NOINLINE fmDialog #-}
-- |Диалог со стандартными кнопками OK/Cancel
fmDialog fm' title flags action = do
  fm <- val fm'
  title <- i18n title
  bracketCtrlBreak "fmDialog" dialogNew widgetDestroy $ \dialog -> do
    set dialog [windowTitle          := title,
                containerBorderWidth := 0]
    when (isJust$ fm_window_ fm) $ do
      set dialog [windowTransientFor := fm_window fm]
    -- Создать 2 или 3 кнопки
    addStdButton dialog ResponseOk       >>= \okButton -> do
    addStdButton dialog aResponseDetach  `on_`  (AddDetachButton `elem` flags)
    addStdButton dialog ResponseCancel
    --
    dialogSetDefaultResponse dialog ResponseOk
    tooltips =:: tooltipsNew
    action (dialog,okButton)

-- |Доп. флаги для настройки fmDialog
data FMDialogFlags = AddDetachButton  -- ^Add the Detach button to the dialog?
                     deriving Eq

{-# NOINLINE fmDialogRun #-}
-- |Отработать диалог с сохранением его положения и размера в истории
fmDialogRun fm' dialog name = do
  fmRestoreSizePos fm' dialog name ""
  res <- dialogRun dialog
  fmSaveSizePos    fm' dialog name
  fm <- val fm'
  when (isJust$ fm_window_ fm) $ do
    windowPresent (fm_window fm)
  return res


----------------------------------------------------------------------------------------------------
---- Список файлов в архиве ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

createFilePanel = do
  let columnTitles = ["0015 Name", "0016 Size", "0497 Type", "0017 Modified", "0018 DIR"]
      n = map i18no columnTitles
  s <- i18ns columnTitles
  createListView fmname [(n!!0, s!!0, fmname,                                                       []),
                         (n!!1, s!!1, (\fd -> if (fdIsDir fd) then (last s) else (show3$ fdSize fd)), [cellXAlign := 1]),
                         (n!!2, s!!2, fdType,                                                       []),
                         (n!!3, s!!3, (guiFormatDateTime.fdTime),                                   [])]

createListView searchField columns = do
  -- Scrolled window where this list will be put
  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
  -- Create a new ListView
  view  <- New.treeViewNew
  set view [ {-New.treeViewSearchColumn := 0, -} New.treeViewRulesHint := True, treeViewRubberBanding := True]
  New.treeViewSetHeadersVisible view True
  -- Создаём и устанавливаем модель
  model <- New.listStoreNew []
  set view [New.treeViewModel := model]
  -- Создаём колонки для её отображения.
  onColumnTitleClicked <- ref doNothing
  let addColumnActions = columns.$map (\(a,b,c,d) -> addColumn view model onColumnTitleClicked a b c d)
  columns <- sequence addColumnActions
  addColumn view model onColumnTitleClicked "" "" (const "") []
  -- Включаем поиск по первой колонке
  New.treeViewSetEnableSearch view True
  New.treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    (i:_) <- New.treeModelGetPath model iter
    row <- New.listStoreGetValue model i
    return (strLower(searchField row) ~= strLower(str)++"*")
  -- Enable multiple selection
  selection <- New.treeViewGetSelection view
  set selection [New.treeSelectionMode := SelectionMultiple]
  -- Pack list into scrolled window and return window
  containerAdd scrwin view
  return (scrwin, view, model, selection, columns, onColumnTitleClicked)

-- |Задать новый список отображаемых файлов
changeList model selection filelist = do
  New.treeSelectionUnselectAll selection
  -- Удалить старые данные из модели и заполнить её новыми
  New.listStoreClear model
  for filelist (New.listStoreAppend model)

-- |Добавить во view колонку, отображающую field, с заголовком title
addColumn view model onColumnTitleClicked colname title field attrs = do
  col1 <- New.treeViewColumnNew
  New.treeViewColumnSetTitle col1 title
  renderer1 <- New.cellRendererTextNew
  New.cellLayoutPackStart col1 renderer1 False
  -- Попытки сделать поле имени автоматически увеличивающимся при увеличении окна программы
  -- (bool New.cellLayoutPackStart New.cellLayoutPackEnd expand) col1 renderer1 expand
  -- set col1 [New.treeViewColumnSizing := TreeViewColumnAutosize] `on_` expand
  -- set col1 [New.treeViewColumnSizing := TreeViewColumnFixed] `on_` not expand
  -- cellLayoutSetAttributes  [New.cellEditable := True, New.cellEllipsize := EllipsizeEnd]
  when (colname/="") $ do
    set col1 [ New.treeViewColumnResizable   := True
             , New.treeViewColumnSizing      := TreeViewColumnFixed
             , New.treeViewColumnClickable   := True
             , New.treeViewColumnReorderable := True
             , nameAttr                      := Just colname]
  -- При нажатии на заголовок столбца вызвать колбэк
  col1 `New.onColClicked` do
    val onColumnTitleClicked >>= ($colname)
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [New.cellText := field row] ++ attrs
  New.treeViewAppendColumn view col1
  return (colname,col1)

-- |Показать индикатор сортировки над столбцом colname в направлении order
showSortOrder columns colname order = do
  for (map snd columns) (`New.treeViewColumnSetSortIndicator` False)
  let Just col1  =  colname `lookup` columns
  New.treeViewColumnSetSortIndicator col1 True
  New.treeViewColumnSetSortOrder     col1 order

-- |Cохранение порядка и ширины колонок в конфиг-файл
saveColumnsOrderAndWidths fm' listname listView columns = do
  colnames  <-  New.treeViewGetColumns listView  >>=  mapM (`get` nameAttr)
  fmReplaceHistory fm' (listname++".ColumnOrder") (unwords$ catMaybes colnames)
  for columns $ \(name,col1) -> do
    w <- New.treeViewColumnGetWidth col1
    fmReplaceHistory fm' (listname++".ColumnWidth."++name) (show w)

-- |Восстановление сохранённых порядка и ширины колонок
restoreColumnsOrderAndWidths fm' listname listView columns = do
  order <- words `fmap` fmGetHistory1 fm' (listname++".ColumnOrder") ""
  for (reverse order) $ \colname -> do
    whenJust (lookup colname columns) $
      New.treeViewMoveColumnFirst listView
  for columns $ \(name,col1) -> do
    w <- readInt  `fmap`  fmGetHistory1 fm' (listname++".ColumnWidth."++name) "150"
    New.treeViewColumnSetFixedWidth col1 w

-- |Атрибут, хранящий имя столбца
nameAttr :: Attr TreeViewColumn (Maybe String)
nameAttr = unsafePerformIO objectCreateAttribute
