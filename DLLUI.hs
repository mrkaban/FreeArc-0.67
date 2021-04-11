{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- �������������� ������������ � ���� ���������� ��������� (CUI - Console User Interface).  ------
----------------------------------------------------------------------------------------------------
module DLLUI where

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent
import Control.OldException
import Data.Char
import Data.IORef
import Foreign
import Foreign.C
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.IO
import System.Time
#ifdef FREEARC_WIN
import System.Win32.Types
#endif
#ifdef FREEARC_UNIX
import System.Posix.IO
import System.Posix.Terminal
#endif

import TABI
import Utils
import Charsets
import Errors
import Files
import FileInfo
import Options
import UIBase


-- |������ ���������� ���������
guiStartProgram = do
  errorHandlers   =: [\msg -> gui_callback "error"   [Pair "message" (W msg)]]
  warningHandlers =: [\msg -> gui_callback "warning" [Pair "message" (W msg)]]
  return ()

-- |���������� � ������ ��������� ������
guiStartArchive = doNothing0

-- |�������� ������ �������� ��� ���������� ������
guiStartProcessing = do
  ui_state <- val ref_ui_state
  gui_callback "total"    [ Pair "files"       (total_files ui_state)
                          , Pair "original"    (total_bytes ui_state)]

-- |������ ���������� ���� ������
guiStartVolume filename = do
  gui_callback "volume"   [ Pair "filename"    (W filename)]

-- |���������� � ������ ��������� �����
guiStartFile = do
  (msg,filename)  <- val uiMessage
  gui_callback "file"     [ Pair "message"     (W msg)
                          , Pair "filename"    (W filename)]

-- |������� ����� ��������/������ ������
guiUpdateProgressIndicator = do
  ui_state <- val ref_ui_state
  gui_callback "progress" [ Pair "original"    (bytes  ui_state)
                          , Pair "compressed"  (cbytes ui_state)]

-- |������������� ����� ���������� ��������� � ������� ��� �����
uiSuspendProgressIndicator = doNothing0

-- |����������� ����� ���������� ��������� � ������� ��� ������� ��������
uiResumeProgressIndicator = doNothing0

-- |������� ����� � ���������� ���������
guiPauseAtEnd = doNothing0

-- |��������� ���������� ���������
guiDoneProgram = doNothing0


----------------------------------------------------------------------------------------------------
---- ������� � ������������ ("������������ ����?" � �.�.) ------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������ � ���������� �����
askOverwrite diskname _ _ arcfile  =  ask (diskname,arcfile)
{-# NOINLINE askOverwrite #-}

-- |����� �������� ��� ������ �������� � ������������
ask question ref_answer answer_on_u =  do
  old_answer <- val ref_answer
  new_answer <- case old_answer of
                  "a" -> return old_answer
                  "u" -> return old_answer
                  "s" -> return old_answer
                  _   -> ask_user question
  ref_answer =: new_answer
  case new_answer of
    "u" -> return answer_on_u
    _   -> return (new_answer `elem` ["y","a"])

-- |���������� ������� � ������������� ���������� �����
ask_user (diskname,fi) = do
  answer <- gui_callback_with_result "can_be_extracted?"   [ Pair "diskname"       (W diskname)
                                                           , Pair "filename"       (W$ storedName fi)
                                                           , Pair "original"       (fiSize fi)
                                                           , Pair "compressed"     (0::Int)
                                                           , Pair "time"           (fromEnum$ fiTime fi)
                                                           , Pair "attr"           (fiAttr fi)
                                                           , Pair "is_folder?"     (fiIsDir fi)
                                                           , Pair "crc"            (0::Int)
                                                           , Pair "is_encrypted?"  (False)
                                                           ]
  return [chr answer]


----------------------------------------------------------------------------------------------------
---- ������ ������� --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

ask_passwords = (ask_encryption_password, ask_decryption_password, bad_decryption_password)

-- |�������� ��������� � ���, ��� �������� ������ �� �������� ��� ������������
bad_decryption_password = doNothing0

-- |������ ������ ��� ������. ������������ ��������� ����
-- � ������ ����������� ������ ��� ���������� ������ ��� ��� �����
ask_encryption_password opt_parseData = do
  return ""

-- |������ ������ ��� ����������. ������������ ��������� ����
ask_decryption_password opt_parseData = do
  let size=1000::Int
  allocaBytes (size*2) $ \ptr -> do
  gui_callback "ask_password"   [ Pair "password_buf"  ptr
                                , Pair "password_size" size]
  peekCWString ptr

uiPrintArcComment arcComment = doNothing0

-- |������ � stdin ����������� � ������
uiInputArcComment old_comment = do
  return ""

resetConsoleTitle = doNothing0



gui_callback_with_result request params = do
  callback <- val var_gui_callback
  TABI.call callback (Pair "request" request : params)

gui_callback request params  =  gui_callback_with_result request params >> return ()

var_gui_callback = unsafePerformIO$ newIORef (error "undefined DLLUI::var_callback")      :: IORef TABI.C_FUNCTION

