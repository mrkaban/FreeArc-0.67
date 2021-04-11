{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- �������������� ������������ � ���� ���������� ��������� (CUI - Console User Interface).  ------
----------------------------------------------------------------------------------------------------
module CUI where

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

import Utils
import Charsets
import Errors
import Files
import FileInfo
import Options
import UIBase


----------------------------------------------------------------------------------------------------
---- ����������� ���������� ��������� --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |��������� background thread ��� ������ ���������� ���������
guiStartProgram = do
  -- ��������� ��������� ��������� � ��������� ���� ��� � 0.5 �������
  indicatorThread 0.5 $ \updateMode indicator indType title b bytes total processed p -> do
    setConsoleTitle title
    taskbar_SetProgressValue (i bytes) (i total)
    withConsoleAccess $ do
      myPutStr$ back_percents indicator ++ p
      myFlushStdout

-- |���������� � ������ ��������� ������
guiStartArchive = doNothing0

-- |�������� ������ �������� ��� ���������� ������
guiStartProcessing = doNothing0

-- |������ ���������� ���� ������
guiStartVolume filename = doNothing0

-- |���������� � ������ ��������� �����
guiStartFile = do
  command <- val ref_command
  when (opt_indicator command == "2") $ do
    syncUI $ do
    uiSuspendProgressIndicator
    (msg,filename)  <- val uiMessage
    imsg <- i18n (msg ||| msgDo(cmd_name command))
    myPutStrLn   ""
    myPutStr$    left_justify 72 ("  "++format imsg filename)
    uiResumeProgressIndicator
    hFlush stdout

-- |������� ����� ��������/������ ������
guiUpdateProgressIndicator = doNothing0

-- |������������� ����� ���������� ��������� � ������� ��� �����
uiSuspendProgressIndicator = do
  aProgressIndicatorEnabled =: False
  (indicator, indType, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
  myPutStr$ clear_percents indicator
  myFlushStdout

-- |����������� ����� ���������� ��������� � ������� ��� ������� ��������
uiResumeProgressIndicator = do
  (indicator, indType, arcname, direction, b :: Rational, bytes', total') <- val aProgressIndicatorState
  bytes <- bytes' (round b);  total <- total'
  myPutStr$ percents indicator bytes total
  myFlushStdout
  aProgressIndicatorEnabled =: True

-- |������� ����� � ���������� ���������
guiPauseAtEnd = do
  withoutEcho getHiddenChar
  return ()

-- |��������� ���������� ���������
guiDoneProgram = do
  return ()

-- |Pause progress indicator & timing while dialog runs
pauseEverything  =  pauseTiming . pauseTaskbar

{-# NOINLINE guiStartProgram #-}
{-# NOINLINE guiStartFile #-}
{-# NOINLINE uiSuspendProgressIndicator #-}
{-# NOINLINE uiResumeProgressIndicator #-}
{-# NOINLINE guiDoneProgram #-}


----------------------------------------------------------------------------------------------------
---- ������� � ������������ ("������������ ����?" � �.�.) ------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������ � ���������� �����
askOverwrite filename _ _ _ =  ask ("Overwrite " ++ filename)
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
ask_user question = syncUI $ pauseEverything go  where
  go = do myPutStr$ "\n  "++question++"?\n  "++commented_answers++"? "
          hFlush stdout
          answer  <-  getLine >>== strLower
          when (answer=="q") $ do
              terminateOperation
          if (answer `elem` (split '/' valid_answers))
            then return answer
            else myPutStr askHelp >> go

-- |���������, ��������� �� ����� ��� ������������ ������
askHelp = unlines [ "  Valid answers are:"
                  , "    y - yes"
                  , "    n - no"
                  , "    a - always, answer yes to all remaining queries"
                  , "    s - skip, answer no to all remaining queries"
                  , "    u - update remaining files (yes for each extracted file that is newer than file on disk)"
                  , "    q - quit program"
                  ]

commented_answers = "(Y)es / (N)o / (A)lways / (S)kip all / (U)pdate all / (Q)uit"
valid_answers = "y/n/a/s/u/q"


----------------------------------------------------------------------------------------------------
---- ������ ������� --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

ask_passwords = (ask_encryption_password, ask_decryption_password, bad_decryption_password)

-- |�������� ��������� � ���, ��� �������� ������ �� �������� ��� ������������
bad_decryption_password = myPutStrLn "Incorrect password"

-- |������ ������ ��� ������. ������������ ��������� ����
-- � ������ ����������� ������ ��� ���������� ������ ��� ��� �����
ask_encryption_password opt_parseData = syncUI $ pauseEverything $ do
  withoutEcho $ go where
    go = do myPutStr "\n  Enter encryption password:"
            hFlush stdout
            answer <- getHiddenLine >>== opt_parseData 't'
            myPutStr "  Reenter encryption password:"
            hFlush stdout
            answer2 <- getHiddenLine >>== opt_parseData 't'
            if answer/=answer2
              then do myPutStrLn "  Passwords are different. You need to repeat input"
                      go
              else return answer

-- |������ ������ ��� ����������. ������������ ��������� ����
ask_decryption_password opt_parseData = syncUI $ pauseEverything $ do
  withoutEcho $ do
  myPutStr "\n  Enter decryption password:"
  hFlush stdout
  getHiddenLine >>== opt_parseData 't'

-- |������ ������, �� ��������� � �� ������
getHiddenLine = go ""
  where go s = do c <- getHiddenChar
                  case c of
                    '\r' -> do myPutStrLn ""; return s
                    '\n' -> do myPutStrLn ""; return s
                    c    -> go (s++[c])


#ifdef FREEARC_WIN

-- |��������� ������� � ����� ���������� �����
withoutEcho = id
-- |������ ������ ��� ���
getHiddenChar = liftM (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
   c_getch :: IO CInt

#else

getHiddenChar = getChar

withoutEcho action = do
  let setAttr attr = setTerminalAttributes stdInput attr Immediately
      disableEcho = do origAttr <- getTerminalAttributes stdInput
                       setAttr$ origAttr.$ flip withMode ProcessInput
                                        .$ flip withoutMode EnableEcho
                                        .$ flip withMode KeyboardInterrupts
                                        .$ flip withoutMode IgnoreBreak
                                        .$ flip withMode InterruptOnBreak
                       return origAttr
  --
  bracketCtrlBreak "restoreEcho" disableEcho setAttr (\_ -> action)

#endif

{-# NOINLINE ask_passwords #-}


----------------------------------------------------------------------------------------------------
---- ����/����� ������������ � ������  -------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������� �� ����� ����������� � ������
uiPrintArcComment arcComment = do
  when (arcComment>"") $ do
    myPutStrLn arcComment

-- |������ � stdin ����������� � ������
uiInputArcComment old_comment = syncUI $ pauseEverything $ do
  myPutStrLn "Enter archive comment, ending with \".\" on separate line:"
  hFlush stdout
  let go xs = do line <- myGetLine
                 if line/="."
                   then go (line:xs)
                   else return$ joinWith "\n" $ reverse xs
  --
  go []

{-# NOINLINE uiPrintArcComment #-}
{-# NOINLINE uiInputArcComment #-}

----------------------------------------------------------------------------------------------------
----- External functions ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

#ifdef FREEARC_WIN
type TString = Ptr TCHAR
#else
withTString  = withCString
type TString = CString
#endif


-- |Set console title
setConsoleTitle title = do
  withTString title c_SetConsoleTitle

-- |Set console title (external)
foreign import ccall safe "Environment.h EnvSetConsoleTitle"
  c_SetConsoleTitle :: TString -> IO ()

-- |Reset console title
foreign import ccall safe "Environment.h EnvResetConsoleTitle"
  resetConsoleTitle :: IO ()


-- |Synchronize console access
withConsoleAccess  =  bracket_ c_SynchronizeConio_Enter c_SynchronizeConio_Leave

-- |Enter & leave console access mutex
foreign import ccall safe "Compression/External/C_External.h SynchronizeConio_Enter"
  c_SynchronizeConio_Enter :: IO ()
foreign import ccall safe "Compression/External/C_External.h SynchronizeConio_Leave"
  c_SynchronizeConio_Leave :: IO ()

