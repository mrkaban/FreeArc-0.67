----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: utility functions                                               ------
----------------------------------------------------------------------------------------------------
module FileManUtils where

import Prelude hiding (catch)
import Control.Concurrent
import Control.OldException
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C
import Foreign.Marshal
import System.IO.Unsafe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Options
import UIBase
import UI
import Arhive7zLib


-- |������ ���������� � �����������
encryptionPassword  =  unsafePerformIO$ newIORef$ ""
decryptionPassword  =  unsafePerformIO$ newIORef$ ""


----------------------------------------------------------------------------------------------------
---- ������� ��������� ����-��������� --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������� ��������� ����-���������: ������ ��������� ������, ����� ������ ������ � ������ ����������
data FM_State = FM_State { fm_window_      :: Maybe Window
                         , fm_view         :: TreeView
                         , fm_model        :: New.ListStore FileData
                         , fm_selection    :: TreeSelection
                         , fm_statusLabel  :: Label
                         , fm_messageCombo :: (New.ComboBox, IORef Int)
                         , fm_filelist     :: [FileData]
                         , fm_history      :: HistoryFile
                         , fm_onChdir      :: [IO()]
                         , fm_sort_order   :: String
                         , fm_passwords    :: [String]                       -- ������, �������� ������������� �� ����� ������� ������
                         , subfm           :: SubFM_State
                         }

-- |������� ��������� ����-���������: ���������� �� ������������ ������ ��� �������� �����
data SubFM_State = FM_Archive   { subfm_archive  :: ArchiveInfo
                                , subfm_arcname  :: FilePath
                                , subfm_arcdir   :: FilePath
                                , subfm_filetree :: FileTree FileData
                                }
                 | FM_Directory { subfm_dir      :: FilePath
                                }

-- |True, ���� FM ������ ���������� �����
isFM_Archive (FM_State {subfm=FM_Archive{}}) = True
isFM_Archive _                               = False

fm_archive = subfm_archive.subfm
fm_arcname = subfm_arcname.subfm
fm_arcdir  = subfm_arcdir .subfm
fm_dir     = subfm_dir    .subfm

-- |���� ����-���������
fm_window FM_State{fm_window_ = Just window} = window

-- |������� �����+������� � �� ��� ������� �� �����
fm_current fm | isFM_Archive fm = fm_arcname fm </> fm_arcdir fm
              | otherwise       = fm_dir     fm

-- |������� �������, ������������ � FM, ��� �������, � ������� ��������� ������� �����
fm_curdir fm | isFM_Archive fm = fm_arcname fm .$takeDirectory
             | otherwise       = fm_dir     fm

-- |�������� ��� ������, ��������� � FM
fm_changeArcname arcname fm@(FM_State {subfm=subfm@FM_Archive{}}) =
                         fm {subfm = subfm {subfm_arcname=arcname}}

-- |��������� ������ ������� �����������, �������������� ��� ���������� �������
fmSaveDecryptionPasswords fm' command = do
  passwords <- get_opt_decryption_passwords command    -- �������� ������ �������, �������� �������������
  fm' .= \fm -> fm {fm_passwords = take 10 passwords}  -- ... �� ������� ���� ������ 10 �� ���

-- |�������� ������ ������� �����������
fmGetDecryptionPasswords fm' = do
  xpwd'     <- val decryptionPassword        -- ������ ����������, �������� � Settings
  passwords <- fm_passwords `fmap` val fm'   -- ������, �������� ������������� � ������� ������
  return ((insertAt 1 (xpwd' &&& [xpwd']) (delete xpwd' passwords)))   -- �� ������ ����� ������ ���� ��������� ������, �������� ������� - ��������� 7z.dll ������������ ������ ���� ������


----------------------------------------------------------------------------------------------------
---- �������� ��� ������� ���������/������ ---------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������� ��������������� � ����-���������: ������� ������ ������ ��� �� �����
data PathInfo path  =  ArcPath path path | DiskPath path | Not_Exists  deriving (Eq,Show)

isArcPath ArcPath{} = True
isArcPath _         = False

-- |������ ������� ��������������� � FM � ��������� PathInfo
splitArcPath fm' fullname = do
  fm <- val fm'
  -- ������� fullname � ������ ��������� � fm ������ (arcname)
  -- ���� arcname - ������� fullname, �� �������� fullname �� ��� ������ arcname � ������� ������ ����
  let arcname = fm_arcname fm
  if isFM_Archive fm  &&  (arcname `isParentDirOf` fullname)
    then return$ ArcPath arcname (fullname `dropParentDir` arcname)
    else do
  -- �������� ������������� �������� � ����� ������ (��� "", ����� �������� ������������)
  d <- not(isURL fullname) &&& dirExist fullname
  if d || fullname=="" then return$ DiskPath fullname
    else do
  -- �������� ������������� ����� � ����� ������
  f <- fileExist fullname
  if f then return$ ArcPath fullname ""
    else do
  -- �������� ��� ��������, ������� �� fullname ��������� ���������� �����
  res <- splitArcPath fm' (takeDirectory fullname)
  -- ���� ��������� - ������� ������ ������, �� ������� ���������� ���������� � ����� ��������
  -- ����� �� ������������ fullname �������� �� �������������� � ������� ����
  case res of
    ArcPath  dir name | isURL(takeDirectory fullname) == isURL fullname  -- ��������� ��� �� �� �������� URL �� ����� ������ :D
                      -> return$ ArcPath dir (name </> takeFileName fullname)
    _                 -> return$ Not_Exists


-- |��������� ����, ���������� ������������ �������� ��������� �������� � FM, � ����������
fmCanonicalizeDiskPath fm' relname = do
  let name  =  unquote (trimRight relname)
  if (name=="")  then return ""  else do
  fm <- val fm'
  myCanonicalizePath$ fm_curdir fm </> name

-- |��������� ����, ���������� ������������ �������� ��������� � FM, � ����������
fmCanonicalizePath fm' relname = do
  fm <- val fm'
  case () of
   _ | isURL relname                              ->  return relname
     | isAbsolute relname                         ->  myCanonicalizePath relname
     | isURL (fm_current fm) || isFM_Archive fm   ->  return$ urlNormalize (fm_current fm) relname    -- ������������ ���� Normalize ��� ��������� ������ ������� � �� URL
     | otherwise                                  ->  myCanonicalizePath (fm_current fm </> relname)

-- |������������� ����, ���������� ������������ ������� URL
urlNormalize url relname =  dropTrailingPathSeparator$ concat$ reverse$ remove$ reverse$ splitPath (url++[pathSeparator]) ++ splitPath relname
  where remove (".":xs)    = remove xs
        remove ("./":xs)    = remove xs
        remove (".\\":xs)    = remove xs
        remove ("..":x:xs) = remove xs
        remove ("../":x:xs) = remove xs
        remove ("..\\":x:xs) = remove xs
        remove (x:xs)      = x : remove xs
        remove []          = []


----------------------------------------------------------------------------------------------------
---- FileData � FileTree ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |���������, �������� ��� ����������� ��� ���������� � �����
data FileData = FileData
  { fdPackedDirectory      :: !MyPackedString  -- ��� ��������
  , fdPackedBasename       :: !MyPackedString  -- ��� ����� ��� ��������, �� � �����������
  , fdSize  :: {-# UNPACK #-} !FileSize        -- ������ ����� (0 ��� ���������)
  , fdTime  :: {-# UNPACK #-} !FileTime        -- ����/����� �������� �����
  , fdAttr  :: {-# UNPACK #-} !FileAttr        -- ��������� �������� �����
  , fdIsDir :: {-# UNPACK #-} !Bool            -- ��� �������?
  }

fiToFileData fi = FileData { fdPackedDirectory = fpPackedDirectory (fiStoredName fi)
                           , fdPackedBasename  = fpPackedBasename  (fiStoredName fi)
                           , fdSize            = fiSize  fi
                           , fdTime            = fiTime  fi
                           , fdAttr            = fiAttr  fi
                           , fdIsDir           = fiIsDir fi }

fdDirectory  =  myUnpackStr.fdPackedDirectory
fdBasename   =  myUnpackStr.fdPackedBasename
fdExt        =  takeExtension.fdBasename

-- |��� �����. ������ ���� fdFullname ��� ��������� ������ "�������� ������" �������/�������� ������
fmname = fdBasename

-- |����������� ����: ������ ��� �����, ������� ������� � ����������
fdFullname fd  =  fdDirectory fd </> fdBasename fd

-- |���������� ������������� ������� � ������� ������ name
fdArtificialDir name = FileData { fdPackedDirectory = myPackStr ""
                                , fdPackedBasename  = name
                                , fdSize            = 0
                                , fdTime            = aMINIMUM_POSSIBLE_FILETIME
                                , fdAttr            = 0
                                , fdIsDir           = True }



-- |��� �����. ������ �� ����, ���� � �� ��� - ������������� � Windows
fdType fd | fdIsDir fd = ""
          | otherwise  = unsafePerformIO$ do
                           let ext = fdExt fd ||| "file"
                           types <- val filetypeList
                           case ext `lookup` types of
                             Just typ -> return typ
                             Nothing  -> do typ <- guiGetFileType ext
                                            filetypeList =: (ext,typ):types
                                            return typ

-- |���������� ����������, �������� ��� ���������� ������������ ���������� -> ��� �����
filetypeList = unsafePerformIO$ mvar [] :: MVar [(String,String)]

-- |�������� ��� ����� �� ��� ����������
guiGetFileType ext = do
  withCWString ext  $ \c_ext -> do
  allocaBytes 1000 $ \buf -> do
  c_GuiGetFileType c_ext buf
  peekCWString buf

foreign import ccall safe "Environment.h GuiGetFileType"
  c_GuiGetFileType :: CWString -> CWString -> IO ()




-- |������ ������. �������� ������ ������ �� ���� ������ ���� ������������� ����������
--                        files   dirname subtree
data FileTree a = FileTree [a]  [(MyPackedString, FileTree a)]

-- |���������� ���������� ��������� � ������
ftDirs  (FileTree files subdirs) = length (removeDups (subdirs.$map fst  ++  files.$filter fdIsDir .$map fdPackedBasename))
                                 + sum (map (ftDirs.snd) subdirs)

-- |���������� ���������� ������ � ������
ftFiles (FileTree files subdirs) = length (filter (not.fdIsDir) files)  +  sum (map (ftFiles.snd) subdirs)

-- |���������� ������ ������ � �������� ��������,
-- ��������� ����������� artificial ��� ��������� ������-������ �� ��� ��������� ���������
ftFilesIn dir artificial = f (map myPackStr$ splitDirectories dir)
 where
  f (path0:path_rest) (FileTree _     subdirs) = lookup path0 subdirs.$ maybe [] (f path_rest)
  f []                (FileTree files subdirs) = (files++map (artificial.fst) subdirs)
                                                  .$ keepOnlyFirstOn fdPackedBasename

-- |����� ���������� � ����� �� ��� ������� ���� � ������
ftFind fullpath tree = let (dir,name) = splitFileName fullpath
                           files = ftFilesIn (dropTrailingPathSeparator dir) fdArtificialDir tree
                       in  listToMaybe$ filter ((name==).fdBasename) files

-- |���������� ������ ������ � ������
buildTree x = x
  .$splitt 0                                     -- ��������� �� ������ �� ���������, ������� � 0-�� ������
splitt n x = x
  .$sort_and_groupOn (dirPart n)                 -- ���������/���������� �� ����� �������� ���������� ������
  .$partition ((==myPackStr"").dirPart n.head)   -- �������� ������ � �������, ������������ ��������������� � ���� ��������
  .$(\(root,other) -> FileTree (concat root)     -- ��������� ������ ������������ ���������� �� (n+1)-� ������
                               (map2s (dirPart n.head, splitt (n+1)) other))

-- ��� n-� ����� ��������
dirPart n = myPackStr.(!!n).(++[""]).splitDirectories.fdDirectory
