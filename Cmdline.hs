{-# OPTIONS_GHC -cpp #-}
---------------------------------------------------------------------------------------------------
---- ����������� ��������� ������ � ����� ������/����� �� ����������.                          ----
---------------------------------------------------------------------------------------------------
module Cmdline where

import Prelude hiding (catch)
import Control.OldException
import Control.Monad
import Control.Concurrent
import Data.Array
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C
import Foreign.C.Types
import System.Environment
import System.IO.Unsafe
import System.Time

import qualified CompressionLib
import Utils
import Files
import Charsets
import Errors
import FileInfo
import Compression
import Arhive7zLib
import Options
#if defined(FREEARC_WIN)
import System.Win32.File    (fILE_ATTRIBUTE_ARCHIVE)
#endif


-- |��������� ��������� ������ � ���������� ������ �������� � ��� ������ � ���� �������� Command.
-- ������ ������� �������� ��� �������, ������������ �������, ������ ������������ ������ � �����.
-- ������� ����������� " ; ", �������� "a archive -r ; t archive ; x archive"
parseCmdline cmdline  =  (`mapMaybeM` split ";" cmdline) $ \args -> do
  -- ���������� display_option � �������� �� ���������, ��������� ������� ������ ����� �� �������������.
  display_option' =: aDISPLAY_DEFAULT
  let options = takeWhile (/="--") $ filter (match "-*") args
  -- ���� ��������� ������ �� �������� ������ ����� ����� - ���������� help/������������ � �����
  if args==options then do
      putStr $ if options `contains` "--print-config"
                 then unlines ("":";You can insert these lines into ARC.INI":aCOMPRESSION_METHODS:builtinMethodSubsts)
                 else aHELP
      return Nothing
    else do

  -- Additional filters for params of some options when required to resolve ambiguity
  let option_checks  =  [("type", (\arctype -> (arctype `elem` ["--",aFreeArcExt]) || (arctype/="" && szCheckType arctype)))]

  -- ��������� ����� �� ���������� ����� FREEARC ��� �������� � ����� -env
  (o0, _) <- parseOptions options option_checks [] []
  let no_configs = findReqList o0 "config" `contains` "-"
  env_options <- case (findReqArg o0 "env" "--") of
                    "--" | no_configs -> return ""  -- ����� -cfg- � ��������� ������ ��������� ������������� � arc.ini, � %FREEARC
                         | otherwise  -> getEnv aCONFIG_ENV_VAR  `catch`  (\e -> return "")
                    "-"               -> return ""
                    env               -> getEnv env


  -----------------------------------------------------------------------------------------------------------
  -- ������ ������-����� ------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------

  -- ��������� ������-����� arc*.ini ��� ��������� ������ -cfg
  (o1, _)  <- parseOptions (words env_options++options) option_checks [] []   -- ����� -cfg ����� ���� ������ � ��������� ������ ��� � ���������� �����
  cfgfiles <- case (findReqArg o1 "config" "--") of
                "-"  -> return []
                "--" -> findFiles configFilePlaces aCONFIG_FILES
                cfg  -> findFiles configFilePlaces cfg
  -- ���������� ����� --charset/-sc, ����� ���������� ��������� ��� ������ ������-������
  let (_, parseFile1, _, _, _)  =  parse_charset_option (findReqList o1 "charset")
  -- ��������� ���������� ������-������, � ������ �� ��� ������ ������ � �����������
  configs  <-  foreach cfgfiles $ \cfgfile -> do
                 config <- parseFile1 'i' cfgfile >>== map trim >>== deleteIfs [null, match ";*"]
                 -- �������� ������ �� ��� ����� - ����� �� ��������� �� ������ ������ � ������ ������������
                 return$ case config of
                           x:xs | not (isSectionHeading x) -> (x, xs)
                           _                               -> ("", config)
  -- ������� ���������� �� ���� ������-������
  let config_1st_line  = unwords (map fst configs)
      config_remainder = concatMap snd configs

  -- ��� ����������� ���������� ���������� ������-������ � ����� ������, ����� ������� ����� ���� �������� �������� configSection.
  -- � �������, configSection "[Compression methods]" - ������ ����� � ������� "[Compression methods]"
  let (externalSections, otherSections)  =  partition (match aEXTERNAL_COMPRESSOR.head) $ makeGroups isSectionHeading config_remainder
      joinedSections  =  otherSections .$ map makeSection .$ groupFst .$ mapSnds concat  -- ����� ���������� ����� �� ������ ������-������
      configSection name  =  lookup (cleanupSectionName name) joinedSections `defaultVal` []
      makeSection (x:xs)  =  (cleanupSectionName x, xs)
      -- ������������ ����� ������/���. ���������, ��������� ��������� �� ������ "[Compression methods]"
      decode_compression_method cpus  =  decode_method cpus (configSection aCOMPRESSION_METHODS)
      decode_method_sequence    cpus  =  snd . head . decode_compression_method cpus

  -- � ��� ����������� ��������� �������� �� ������ ������� � �������� ������,
  -- ������� ������, ����� ����� ������� ����������� �������� ��������� ����,
  -- ������� ����� ���� � �� �� �����������,
  -- � ����� ����������� ����������� (� ���� ������ ���� ����� ��� ������).
  -- ������:
  --   a create j = -m4x -ms
  --   a = --display
  -- � ���� ������ (configElement section "a") ��������� "-m4x -ms --display"
  let sectionElement name = unwords . map snd
                              . filter (strLowerEq name . fst)
                              . concatMap (\line -> let (a,b)  =  split2 '=' line
                                                    in  map (\w->(w,trim b)) (words$ trim a))
      configElement section element  =  configSection section .$ sectionElement element

  -- ��� �������: "a", "create" � ��� �����. ����� �� ��������� ��� ���� �������, �������� � ������-�����
  let cmd = head1$ filter (not.match "-*") args
      default_cmd_options = configElement aDEFAULT_OPTIONS cmd

  -----------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------

  -- ���������, ��������� � GUI Settings dialog
  gui_options <- not no_configs &&& readGuiOptions

  -- ������� � ������ ��������� ������ ����� �� ��������� ��� ���� ������, ����� �� freearc.ini (� GUI-������),
  -- ����� �� ��������� ��� ���� ������� � ���������� ���������� �����
  let additional_args  =  words config_1st_line ++ gui_options ++ words default_cmd_options ++ words env_options

  -- ������� ��������� ������, ������� ����� ����� � ������ "��������� ����������"
  (o, freeArgs)  <-  parseOptions (additional_args++args) option_checks [] []
  -- �������� �� ������, ���� "��������� ����������" ������ ���� - ����������� ������� ��� ��� ������
  case freeArgs of
    []     ->  registerError$ CMDLINE_NO_COMMAND args
    [cmd]  ->  registerError$ CMDLINE_NO_ARCSPEC args
    otherwise -> return ()
  let (cmd:pure_arcspec:pure_filespecs) = freeArgs

                               -- ���������:  �������� ����� � �������� �� ���������
  let grouping              =  findReqArg   o "solid" aDEFAULT_DATA_GROUPING .$ parseSolidOption
      group_dir             =  fst3 grouping
      group_data            =  snd3 grouping
      defaultDirCompressor  =  thd3 grouping ||| aDEFAULT_DIR_COMPRESSION
      orig_dir_compressor   =  findReqArg   o "dirmethod"  defaultDirCompressor .$ decode_compression_method 1
      compression_options   =  findReqList  o "method"
      orig_sort_order       =  findMaybeArg o "sort"
      yes                   =  findNoArg    o "yes"
      autogenerate_arcname  =  findOptArg   o "autogenerate"  "--" ||| "%Y%m%d%H%M%S"
      indicator             =  findOptArg   o "indicator"     "1"  ||| "0"   -- �� ��������� -i1; -i ������������ -i0
      recovery              =  findOptArg   o "recovery"      (if take 2 cmd=="rr"  then drop 2 cmd  else "--")   -- ������� "rr..." ������������ ������� "ch -rr..."
                                                              .$  changeTo [("0.1%","0*4kb"), ("0.01%","0*64kb")]
      orig_workdir          =  findOptArg   o "workdir"       ""  .$  changeTo [("--","")]
      create_in_workdir     =  findNoArg    o "create-in-workdir"
      pretest               =  findOptArg   o "pretest"       "1" .$  changeTo [("-","0"), ("+","2"), ("","2")]
      broken_archive        =  findReqArg   o "BrokenArchive" "-"  ||| "0"
      language              =  findReqArg   o "language"      "--"
      pause_before_exit     =  findOptArg   o "pause-before-exit" "--"   .$changeTo [("--",iif isGUI (iif (cmd=="t" || test_opt) "on" "on-warnings") "off"), ("","on"), ("yes","on"), ("no","off"), ("always","on"), ("never","off")]
      shutdown              =  findNoArg    o "shutdown"
      noarcext              =  findNoArg    o "noarcext"
      crconly               =  findNoArg    o "crconly"
      nodata                =  findNoArg    o "nodata"
      url_proxy             =  findOptArg   o "proxy"         "--"
      url_bypass            =  findOptArg   o "bypass"        ""
      exclude_path          =  findOptArg   o "ExcludePath"   "--"
      global_queueing_opt   =  findNoArg    o "queue"
      test_opt              =  findNoArg    o "test"

      -- In absence of -tTYPE option, type of new archives is defined by the arcspec extension: "a a.zip" is the same as "a a.zip -tzip"
      default_arctype   =  szFindFormatForArchiveName pure_arcspec ||| aFreeArcExt
      archive_type      =  findReqArg o "type" "--"  .$  changeTo [("--", default_arctype)]
      archive_extension =  if archive_type==aFreeArcInternalExt   then aDEFAULT_ARC_EXTENSION   else szDefaultExtension archive_type

      add_exclude_path  =  exclude_path .$ changeTo [("--", "9"), ("", "0")] .$ readInt
      dir_exclude_path  =  if                cmd=="e"          then 0
                             else if cmdType cmd==EXTRACT_CMD  then add_exclude_path
                             else                                   3

  -- ������ ��������, ������� ���� ��������� ��������������� ����� ������� ���������� �������
  setup_command <- newList
  let setup action = do action; setup_command <<= action
  setup (url_setup_proxy      .$ withCString (replace ',' ' ' url_proxy))
  setup (url_setup_bypass_list.$ withCString (replace ',' ' ' url_bypass))

  -- ��������� ���� �����������
  setup (setLocale [language])

  -- ������� ��������� ����� -o/-op
  let (op, o_rest) = partition is_op_option (findReqList o "overwrite")
      op_opt       = map  (tryToSkip "p") op
      overwrite    = last ("p":o_rest)
      is_op_option ('p':_:_) = True
      is_op_option _         = False

  -- ���������, ��� ����� ��������� ���� �� ���������� ��������
  testOption "overwrite"         "o"  overwrite         (words "+ - p")
  testOption "indicator"         "i"  indicator         (words "0 1 2")
  testOption "pretest"           "tp" pretest           (words "0 1 2 3")
  testOption "BrokenArchive"     "ba" broken_archive    (words "- 0 1")
  testOption "ExcludePath"       "ep" exclude_path      ([""]++words "1 2 3 --")
  testOption "pause-before-exit" ""   pause_before_exit (words "on off on-warnings on-error")

  -- ���������� ��� SFX-������, ������� ����� �������� � ������ ������
  let sfxname  =  findOptArg o "sfx" (if take 1 cmd=="s"  then drop 1 cmd  else "--")   -- ������� "s..." ������������ ������� "ch -sfx..."
                    |||  default_sfx archive_type  -- ��� ������ ��������� ������������ ������ SFX �� ��������� (� ����������� �� ���� ������, freearc.sfx/7z.sfx �� �������� ���������)
  sfx <- if sfxname `notElem` words "- --" && takeFileName sfxname == sfxname
           then findFile libraryFilePlaces sfxname   -- ������������ ������ � �������� ������ �� ������������ ��������
           else return sfxname
  when (sfx=="") $
    registerError$ GENERAL_ERROR ["0342 SFX module %1 is not found", sfxname]

  -- ������� � �������� ����� ������ ����� ����/�������, ���� ������� ����� -ag
  current_time <- getClockTime
  let add_ag  =  case autogenerate_arcname of
                   "--" -> id
                   _    -> updateBaseName (++ showtime autogenerate_arcname current_time)

  -- ������� � ����� ������ ���������� �� ���������, ���� ��� ������� ���������� � �� ������������ ����� --noarcext
  let arcspec  =  addArcExtension noarcext archive_extension$ add_ag pure_arcspec

  -- ���������� ������ ����� --charset/-sc, ��������� ������� ���������
  -- � ��������� ������/������ ������ � � ������
  let (charsets, parseFile, unParseFile, parseData, unParseData)  =  parse_charset_option (findReqList o "charset")
  setup (setGlobalCharsets charsets)

  -- ������� ���������� ������ ����� --display
  let orig_display = foldl f aDISPLAY_DEFAULT (findReqList o "display")
      -- ������� ��������� ����� --display
      f value ""       =  aDISPLAY_ALL     -- -di ��� ���������� �������� �������� ����� ���� ����������
      f value "--"     =  aDISPLAY_DEFAULT -- -di-- �������� ������������ �������� �� ���������
      f value ('+':x)  =  nub (value++x)   -- -di+x �������� �������� x � ������
      f value ('-':x)  =  nub value \\ x   -- -di-x �������� ������ x �� ������
      f value x        =  nub x            -- ����� ������ ��������� �������� � �������� �����

  -- ��� ������� "lb" ��������� ��������� ����� ���. ���������� �� �����,
  -- ��� ������ ������ �������� �������� ����� ����� ������ � �������������� �������
  let display = case () of
                  _ | cmd=="lb"              ->  ""
                    | cmdType cmd==LIST_CMD  ->  orig_display++"a"
                    | otherwise              ->  orig_display
  -- ���������� display_option, ��������� ��� ��� ����� ������������ ��� ������ warning � ���������� external compressor section
  setup (display_option' =: display)

  -- �������������� �������� ������� ����������� �� ������ [External compressor:...]
  let registerExternalCompressors makeWarnings = do
        CompressionLib.clearExternalCompressorsTable
        for externalSections $ \section -> do
          result <- CompressionLib.addExternalCompressor (unlines section)
          when (result/=1 && makeWarnings) $ do
            registerWarning (BAD_CFG_SECTION "configuration file" section)
  -- �������������� �� ������ ��� �������� ��������� ������ � ���������������� ��� ���������� ��� ���������� ������.
  registerExternalCompressors True
  setup_command <<= (registerExternalCompressors False)


---------------------------------------------------------------------------------------------------
-- ����������� ��������� ������ -------------------------------------------------------------------
  -- ������ ������� ������, �������������� ������ ���� "75%" (�� ������ ���)
  -- ����� ������ ����������� �� ��������, ������� 4 ��, ����� ��������� ��������� ��������� ������� � ���������� �������� ��������� Shadow BIOS options
  let parsePhysMem = parseMemWithPercents (toInteger getPhysicalMemory `roundTo` (4*mb))

  -- ������ ����� -md
  let parseDict dictionary  =  case dictionary of
          [c]       | isAlpha c     ->  Just$ 2^(16 + ord c - ord 'a')   -- ����� ������ ����� ������, -mda..-mdz
          s@(c:_)   | isDigit c     ->  Just$ parsePhysMem s             -- ����� ���������� c �����: -md8, -md8m, -md10%
          otherwise                 ->  Nothing                          -- ����� - ��� �� ����� -md, � ����� -m, ������������ � -md...

  -- ����, ������� �������������� ��������� �����, ������������ �� "-m"
  method <- ref "";    methods <- ref "";  dict <- ref 0;    change_methods' <- newList
  mm'    <- ref "--";  threads <- ref 0 ;  ma'  <- ref "--"
  when (archive_type == aFreeArcInternalExt) $ do                        -- ���� -tarc, �� ���������� ����� -m... ��� arc-�������
    for compression_options $ \option ->
      case option of
        -- Syntax: -mc[$group1,$group2][:]-$group,-algo,+algo,algo1/algo2
        'c':rest  |  anyf [beginWith "-", beginWith "+", beginWith ":", beginWith "$", allf [isInfixOf "/", not.isInfixOf "/$"], endWith "-", endWith "+"] rest

            -> do let parseGroups xs@('$':_) = do
                        let (group_str,operation)  =  break (`elem` ":-+") xs
                            groups                 =  split ',' group_str
                        if any (not.beginWith "$") groups   -- �������� ����� ������ ���������� � '$'
                          then registerError$ CMDLINE_BAD_OPTION_FORMAT ("-m"++option)
                          else return (groups,operation)
                      --
                      parseGroups xs = return ([],xs)
                  --
                  let rarAbbrevs = changeTo [("d","delta"), ("e","exe"), ("l","lzp"), ("r","rep"), ("z","dict"), ("a","$wav"), ("c","$bmp"), ("t","$text")]
                  --
                  let parseOperation operation
                        | beginWith "+" operation || endWith "+" operation  =  return ('+', operation .$tryToSkip "+" .$tryToSkipAtEnd "+", "")
                        | beginWith "-" operation || endWith "-" operation  =  return ('-', operation .$tryToSkip "-" .$tryToSkipAtEnd "-" .$rarAbbrevs, "")
                        | (algo1,'/':algo2)  <-  break (=='/') operation,
                           algo1 > "",  algo2 > ""                          =  return ('/', algo1, algo2)
                        | otherwise                                         =  registerError$ CMDLINE_BAD_OPTION_FORMAT ("-m"++option)
                  --
                  -- Split `rest` into list of groups and operation
                  (groups,operation) <- parseGroups rest
                  -- Parse `operation` string
                  (op,algo1,algo2) <- parseOperation (tryToSkip ":" operation)
                  change_methods' <<= (op, groups.$replace "$default" "", algo1, algo2)

        -- ����� -md ������������� ������ ������� ��� � ������ ������ RAR :)
        'd':rest  | Just md <- parseDict rest ->  dict =: md
        -- ����� -mm �������� ����� �����������-������.
        'm':rest  | mmflag <- rest.$tryToSkip "=",
                    mmflag `elem` ["","--","+","-","max","fast"]  ->  mm' =: mmflag
        -- ����� -ms ����� ������������� �������� ������ ������ ��� ��� ������ ������
        "s"  ->  methods ++= "/$compressed="++join_compressor aCOMPRESSED_METHOD
        "s-" ->  change_methods' <<= ('-',[],"$compressed","")
        -- ����� -ma �������� ����� ��������������� ����� ������
        'a':rest  | maflag <- rest.$tryToSkip "=".$changeTo [("+","--"), ("","--"), ("-","0")],
                    maflag `elem` ["--"]++map show [0..9]  ->  ma' =: maflag
        -- ����� -mt ��������/��������� ��������������� � ������������� ���������� ������
        't':rest  | n <- rest.$tryToSkip "=".$changeTo [("-","1"), ("+","0"), ("","0"), ("--","0")],
                    all isDigit n  ->  threads =: readInt n
        -- ����� -m$type=method ������������� ��������� ������ ��� ��������� ����� ������
        '$':_ -> case (break (`elem` "=:.") option) of
                   (_type, '=':method) -> methods ++= '/':option                      -- -m$type=method: ������������ ����� ����� ���� �������� ������������
                   -- (_type, ':':names)  -> types  ++= split ':' names               -- -m$type:name1:name2: �������� � ������ ������ ����� ���� �������� �����
                   -- (_type, ',':exts)   -> types  ++= map ("*."++) $ split '.' exts -- -m$type.ext1.ext2: �������� ���������� � ������ ����
                   otherwise -> registerError$ CMDLINE_BAD_OPTION_FORMAT ("-m"++option)
        -- ��� ��������� �����, ������������ �� -m0= ��� ������ -m, ������ �������� ����� ������.
        m  ->  method =: m.$tryToSkip "0="
  -- ��������� ������������� �������� ����������
  dictionary      <- val dict                  -- ������ ������� (-md)
  cthreads        <- val threads               -- ���������� compression threads (-mt)
  mainMethod      <- val method                -- �������� ����� ������.
  userMethods     <- val methods               -- �������������� ������ ��� ���������� ����� ������ (-m$/-ms)
  mm              <- val mm'                   -- �����������-������
  change_methods  <- listVal change_methods'   -- ������ ��������, ������� ����� ��������� ��� ����������� ������
  ma              <- val ma'                   -- ����� ��������������� ����� ������

  -- ������� ������, 0..9. �������� ������� ��� �� �����, ���������� ��� ����������� ������ ������.
  let clevel = case mainMethod of
                 xs | xs &&& isDigit (head xs) &&& all isAlpha (tail xs) &&& length(xs)<=3  ->  digitToInt (head xs)
                 xs | xs &&& isDigit (last xs) &&& all isAlpha (init xs) &&& length(xs)<=3  ->  digitToInt (last xs)
                 "mx"                -> 9
                 "max"               -> 9
                 _                   -> 4  -- default compression level
  -- ������� �����������, 0..9
  let ma_opt = case ma of "--" -> clevel
                          _    -> readInt ma

  -- �������� � ���������� �������� ���������� ������, ������� ��� ������ ������������
  let cpus  =  cthreads ||| i getProcessorsCount      -- By default, use number of threads equal to amount of available processors/cores
  setup (CompressionLib.setCompressionThreads cpus)

  -- ��������/��������� ����� ����������� ������
  setup (CompressionLib.setDebugMode (if display `contains_one_of` "$" then 1 else 0))

  -- ����������� �� ������ ��� ��������/����������
  let climit = parseLimit "75%"$ findReqArg o "LimitCompMem"   "--"
      dlimit = parseLimit d_def$ findReqArg o "LimitDecompMem" "--"
      d_def  = if cmdType cmd == ADD_CMD  then "1600mb"  else "75%"
      parseLimit deflt x = case x of
        "--" -> parsePhysMem deflt  -- �� ���������: ���������� ������������� ������ 75% � ����������� ������ ��� ��������, � 1�� ��� ����������
        "-"  -> CompressionLib.aUNLIMITED_MEMORY  -- �� ������������ ������������� ������
        s    -> parsePhysMem s                    -- ���������� ������������� ������ �������� �������

  -- ���������� �����������-�������
  let multimedia mm = case mm of
        "-"    -> (++"/$wav=/$bmp=")    -- ������ ������ $wav � $bmp �� ������ ������� ������.
        "fast" -> (++"/$wav=wavfast/$bmp=bmpfast")
        "max"  -> (++"/$wav=wav/$bmp=bmp")
        "+"    -> multimedia "max"
        ""     -> multimedia "+"
        "--"   -> id


  -- ���������� ��������, �������� ������ -mc:
  let -- ������ ������ mc (��������, "$bmp") �� ������ ������� ������.
      changeMethod ('-',groups,group@('$':_),_)  =  filter ((/=group).fst)

      -- 1. ������ ������, � ������� algo - ��������� �������� ������ (�������� -mc-tta ������� � �������� �����, ������� ������ ������� ������������� ���������� tta)
      -- 2. ������ �������� algo �� ��������� ������� ������.
      changeMethod ('-',groups,algo,_)         =  processTail (deleteIf ((algo==).method_name.last1.snd))      -- �� ������� �������� ������ ������ (������ ������)
                                              >>> applyToGroups groups (deleteIf ((algo==).method_name))

      -- ������� �������� �������� ������.
      changeMethod ('+',groups,algo,_)         =  applyToGroups groups (decode_method_sequence cpus algo++)

      -- ������� �������� ������ �� ������
      changeMethod ('/',groups,algo1,algo2)    =  applyToGroups groups (concatMap (replace algo1 algo2))
                                                      where  replace algo1 algo2 algo  =  if method_name algo==algo1  then (decode_method_sequence cpus algo2)  else [algo]


      -- ��������� �������� op ������ � �������, ������������� � ������ groups (�� ���� ���� ������ ����)
      applyToGroups []     op  =  mapSnds op
      applyToGroups groups op  =  map (\x@(grp,_) -> if grp `elem` groups  then mapSnd op x  else x)


  -- ���� ������ ����� "--nodata", �� ������������ ������ ������.
  -- ���� ������ ����� "--crconly", �� ������������ ��������� CRC ������������ ������.
  -- � ��������� ������ ���������� ��������� �������� � �������������� ��������� ������,
  -- �������� �����������-������ � ������ �������, ������ ����������� ���������,
  -- � ��������� ����������� ������
  let data_compressor = if      nodata   then [("", [aFAKE_COMPRESSION])]
                        else if crconly  then [("", [aCRC_ONLY_COMPRESSION])]
                        else ((mainMethod ||| aDEFAULT_COMPRESSOR) ++ userMethods)
                               .$ multimedia mm
                               .$ decode_compression_method cpus
                               .$ applyAll (map changeMethod change_methods)
                               .$ setDictionary dictionary
                               .$ limitMinDecompressionMem dlimit

  -- ���������� ������ �������� ��������� ������� � �������� ������� � �������� ������� ������
  let dir_compressor = orig_dir_compressor.$ limitMinDecompressionMem dlimit
                                          .$ getMainCompressor
                                          .$ reverse .$ take 1

  -- ����. ������ ����� � ������������ ������� ������������ ��� 0
  let maxBlockSize = getBlockSize data_compressor
  -- ������, ��������� ��� ��������� ������.
  let compressionMem = getCompressionMem data_compressor

  -- ���������, ������� ������ ����� ������������ ��� ����� ������������ ������ ������.
  -- ���� ������ ���� �� ����� ���� ������ --cache, �� ���������� �� 1 �� 16 ��,
  -- �������� ������� ���, ����� ����� ����������� ������ ���������� �� ������������
  -- �������� �� � ����������� ������ (�� ������ ������, ����������� ��� ���������� ������
  -- � ����������� �������). ����������, ��� ������� ����������� ������������� memory-intensive
  -- tasks (� � ���������, ����������� ���������� ������ FreeArc) ��� ������� �� ����� ������.
  -- To do: ����� ���� �� �������� �� ����� *����������* ����������� ��� + ��������� ���� �� � ������ ������� ���������.
  let minCache   =  1*aIO_BUFFER_SIZE   -- ���. �������� ������ ���� ��� ������
      maxCache   =  16*mb               -- ����. ������ ���� - 16 ��
      availCMem  =  i(parsePhysMem "50%" `min` climit) `minusPositive` compressionMem  -- "�������� ������" = min(50% ���,lc) ����� ������, ��������� ��� ������.
      compression_cache    =  clipToMaxInt $
                                case (findReqArg o "cache" "--") of
                                    "--" -> availCMem.$clipTo minCache maxCache
                                    "-"  -> minCache
                                    s    -> parsePhysMem s
      decompression_cache  =  clipToMaxInt $
                                case (findReqArg o "cache" "--") of
                                    "--" -> 4*aIO_BUFFER_SIZE
                                    "-"  -> 0
                                    s    -> parsePhysMem s

  -- ������������� �������� ����� --recompress ��� ������, ���������� �����,
  -- ���� ������� ����� -m../--nodata/--crconly
  let recompress = findNoArg o "recompress"
                   || (is_COPYING_COMMAND cmd  &&  (mainMethod>"" || nodata || crconly))
  -- �� �������������� ������������ �����-����� � ������ ��� --append
  -- � � �������� ����������� ������, ���� --recompress �� ������ ����
  let keep_original = findNoArg o "append"
                      || (is_COPYING_COMMAND cmd  &&  not recompress)


---------------------------------------------------------------------------------------------------
-- ��������� ��� ����������� ������ ������ (find_group) � ���� ����� (find_type) ------------------
  -- ����������, ����� ���� �� ������� ����� (���� arc.groups) ����� ��������������.
  actual_group_file <- case (findReqArg o "groups" "--") of
      "--" -> findFile configFilePlaces aDEFAULT_GROUPS_FILE  -- ������������ ���� ����� �� ��������� (arc.groups �� ��������, ��� ��������� ���������)
      "-"  -> return ""      -- ���� ����� �������� ������    --groups-
      x    -> return x       -- ���� ����� ������ ���� ������ --groups=FILENAME

  -- ��������� ������ ����� �� ����� �����
  group_strings  <-  if actual_group_file > ""
                         then parseFile 'i' actual_group_file      -- ���������� ���� ����� � ������ ��������� �������� � ������������ �����
                                >>== map translatePath             -- ���������� ��� '\' � '/'
                                >>== deleteIfs [match ";*", null]  -- ������� ������ ������������ � ������
                         else return [reANY_FILE]     -- ���� ���� ����� �� ������������, �� ��� ����� ����������� ����� ����� ������
  -- ������ ����������, ����������� ��������� � ������ ������
  let group_predicates  =  map (match_FP fpBasename) group_strings
  -- ������ �� ���������, ���� �������� ��� �����, �� ����������� �� � ����� �� �����.
  -- ����������� ������-������ "$default", ��� � ���������� ���������, ��� ��� ����� ��������� � ����� ������
  let lower_group_strings = (map strLower group_strings) ++ ["$default"]
      default_group = "$default" `elemIndex` lower_group_strings .$ fromJust
  -- ������� "PackedFilePath -> ����� ������ �� arc.groups"
  let find_group    = findGroup group_predicates default_group

  -- ������ ����� ������ ($text, $exe � ��� �����), ��������������� ������ ������ �� arc.groups
  let group_type_names = go "$binary" lower_group_strings  -- ��������� ������ - "$binary"
      go t []     = []           -- ������ �� ������ �����, ������� ����� ������
      go t (x:xs) = case x of    --   �� �������������� �� ����� ����� ������ ("$text", "$rgb" � ��� �����)
                      '$':_ | x/="$default" -> x : go (proper_type x) xs
                      _                     -> t : go t xs
      -- ������ ��� �� ������ ���� x, �������� � compressor_types
      proper_type x    = (find (`elem` compressor_types) (words x)) `defaultVal` ""
      -- ������ ����� ������, ������������ � data_compressor (��������� ������������� ������ ������)
      compressor_types = map fst data_compressor

  -- ������ ������� ������� ������ �� ������ `data_compressor`, ��������������� ������ ������ �� arc.groups
  let group_types =  map typeNum group_type_names
      typeNum t   =  (t `elemIndex` compressor_types) `defaultVal` 0
  -- ������ ����������, ����������� ��� ���� ����������� ������ �� �����, ������������� � `data_compressor`
  let type_predicates  =  const False : map match_type [1..maximum group_types]
      match_type t     =  any_function$ concat$ zipWith (\a b->if a==t then [b] else []) group_types group_predicates
  -- ������� "PackedFilePath -> ����� ����������� � ������ `data_compressor`"
  let find_type  =  findGroup type_predicates 0


-------------------------------------------------------------------------------------
-- ������ ������
  let match_with            =  findNoArg    o "fullnames"          .$bool fpBasename fpFullname
      orig_include_list     =  findReqList  o "include"
      orig_exclude_list     =  findReqList  o "exclude"
      include_dirs          =  findNoArgs   o "dirs" "nodirs"
      clear_archive_bit     =  findNoArg    o "ClearArchiveBit"
      select_archive_bit    =  findNoArg    o "SelectArchiveBit"
      filesize_greater_than =  findReqArg   o "SizeMore"           "--"
      filesize_less_than    =  findReqArg   o "SizeLess"           "--"
      time_before           =  findReqArg   o "TimeBefore"         "--"
      time_after            =  findReqArg   o "TimeAfter"          "--"
      time_newer            =  findReqArg   o "TimeNewer"          "--"
      time_older            =  findReqArg   o "TimeOlder"          "--"

  -- ������� ������ �� ����-����� (@listfile/-n@listfile/-x@listfile) �� ����������
  listed_filespecs <- pure_filespecs   .$ replace_list_files parseFile >>== map translatePath
  include_list     <- orig_include_list.$ replace_list_files parseFile >>== map translatePath
  exclude_list     <- orig_exclude_list.$ replace_list_files parseFile >>== map translatePath

  -- ��������� ������ ���������� (-n) � ����������� (-x) ������. ��� -n ��������� orig_include_list, ��������� ��� ������ ��������� �� ���� ���� �� ������ ��������� ������
  let match_included  =  orig_include_list &&& [match_filespecs match_with include_list]
      match_excluded  =  exclude_list      &&& [match_filespecs match_with exclude_list]

#if defined(FREEARC_WIN)
  -- ����� ������ �� ���������
  let attrib_filter | select_archive_bit = [\attr -> attr.&.fILE_ATTRIBUTE_ARCHIVE /= 0]
                    | otherwise          = []
#else
  let attrib_filter = []
#endif

  -- ����� ������ �� �������
  let size_filter _  "--"   = []
      size_filter op option = [(`op` parseSize 'b' option)]

  -- ����� ������ �� ������� �����������, time � ������� YYYYMMDDHHMMSS
  let time_filter _  "--" = []
      time_filter op time = [(`op` (time.$makeCalendarTime.$toClockTime.$convert_ClockTime_to_CTime))]
      -- ����������� ������� ���� YYYY-MM-DD_HH:MM:SS � CalendarTime � ���������� ���������� ctTZ � ����������� �� � ������� ���� (��� ����� toCalendarTime.toClockTime �������� ������)
      makeCalendarTime str = ct {ctTZ = ctTZ$ unsafePerformIO$ toCalendarTime$ toClockTime ct2}
          where        ct2 = ct {ctTZ = ctTZ$ unsafePerformIO$ toCalendarTime$ toClockTime ct}
                       ct = CalendarTime
                            { ctYear    = readInt (take 4 s)
                            , ctMonth   = readInt (take 2 $ drop 4 s) .$ (\x->max(x-1)0) .$ toEnum
                            , ctDay     = readInt (take 2 $ drop 6 s)
                            , ctHour    = readInt (take 2 $ drop 8 s)
                            , ctMin     = readInt (take 2 $ drop 10 s)
                            , ctSec     = readInt (take 2 $ drop 12 s)
                            , ctPicosec = 0
                            , ctWDay    = error "ctWDay"
                            , ctYDay    = error "ctYDay"
                            , ctTZName  = error "ctTZName"
                            , ctTZ      = 0
                            , ctIsDST   = error "ctIsDST"
                            }
                       s = filter isDigit str ++ repeat '0'

  -- ����� ������ �� "��������", time � ������� [<ndays>d][<nhours>h][<nminutes>m][<nseconds>s]
  let oldness_filter _  "--" = []
      oldness_filter op time = [(`op` (time.$calcDiff.$(`addToClockTime` current_time).$convert_ClockTime_to_CTime))]

      calcDiff  =  foldl updateTD noTimeDiff . recursive (spanBreak isDigit)
      updateTD td x = case (last x) of
                        'd' -> td {tdDay  = -readInt (init x)}
                        'h' -> td {tdHour = -readInt (init x)}
                        'm' -> td {tdMin  = -readInt (init x)}
                        's' -> td {tdSec  = -readInt (init x)}
                        _   -> td {tdDay  = -readInt x}

  -- ������ ������ ������, ���������� ��� �������� ������,
  -- ��������� � ��������� ������, ����� ������ �� filespecs.
  -- ��� ��������� ������������ ��������� �������,
  -- ������ ��� ��� ��-������� ������������ � �������� ������� ����.
  let file_filter = all_functions$
                      concat [                     attrib_filter          .$map (.fiAttr)
                             , map (not.)          match_excluded         .$map (.fiFilteredName)
                             , nst_filters
                             ]
      nst_filters =   concat [                     match_included         .$map (.fiFilteredName)
                             , size_filter    (>)  filesize_greater_than  .$map (.fiSize)
                             , size_filter    (<)  filesize_less_than     .$map (.fiSize)
                             , time_filter    (>=) time_after             .$map (.fiTime)
                             , time_filter    (<)  time_before            .$map (.fiTime)
                             , oldness_filter (>=) time_newer             .$map (.fiTime)
                             , oldness_filter (<)  time_older             .$map (.fiTime)
                             ]

  -- ���� ����� �������������� ������ �� ������� � ������� �� cw/d, �� ������������ ��� �����
  filespecs <- case listed_filespecs of
      [] | cmd `elem` (words "cw d")  ->  registerError$ CMDLINE_NO_FILENAMES args
         | otherwise                  ->  return aDEFAULT_FILESPECS
      _  | cmd.$is_CMD_WITHOUT_ARGS   ->  registerError$ CMDLINE_GENERAL ["0377 command \"%1\" shouldn't have additional arguments", cmd]
         | otherwise                  ->  return listed_filespecs

  -- �������� �������� � ���������? ��� ���������� ������������ ������ ��� ��������/����������
  let x_include_dirs  =  case include_dirs of
           Just x  -> x   -- � ������������ � ������ --dirs/--nodirs
           _       -> -- ��, ���� �������������� ��� �����, ��� �������� -n/-s*/-t* � ������� �� "e"
                      filespecs==aDEFAULT_FILESPECS && null nst_filters && cmd/="e"


-------------------------------------------------------------------------------------
-- ����������
  -- �������� ����������; �������� ���������� � ���������� � ������������� ���� ("aes" -> "aes-256/ctr")
  let ea = findReqArg o "encryption" aDEFAULT_ENCRYPTION_ALGORITHM
  encryptionAlgorithm <- join_compressor ==<< (foreach (split_compressor ea) $ \algorithm -> do
    unless (isEncryption algorithm) $ do
      registerError$ CMDLINE_GENERAL ["0378 bad name or parameters in encryption algorithm %1", algorithm]
    return$ CompressionLib.canonizeCompressionMethod algorithm)

  -- ������ ��� ������ � ��������� ������
  let (dpwd,hpwd) = case (findReqArg o "password"        "--" .$changeTo [("-", "--")]
                         ,findReqArg o "HeadersPassword" "--" .$changeTo [("-", "--")])
                    of
                       (p,    "--")  ->  (p,  "--")    --  -p...
                       ("--", p   )  ->  (p,  p   )    --  -hp..,
                       (p,    ""  )  ->  (p,  p   )    --  -p[PWD] -hp
                       ("",   p   )  ->  (p,  p   )    --  -p -hpPWD
                       (p1,   p2  )  ->  (p1, p2  )    --  -pPWD1 -hpPWD2

  -- ��������� ������ �������, ����������� ��� ����������, ���� ������� -op-/-p-/-hp-
  let dont_ask_passwords  =  last ("":op_opt) == "-" || findReqArg o "OldPassword" "" == "-"  ||  findReqArg o "password" "" == "-"  ||  findReqArg o "HeadersPassword" "" == "-"
  -- ������ �������, ������������ ��� ����������
  mvar_unpack_passwords  <-  newMVar$ deleteIfs [(==""),(=="?"),(=="-"),(=="--")]$ op_opt ++ findReqList o "OldPassword" ++ findReqList o "password" ++ findReqList o "HeadersPassword"
  -- ���������� �������� ������, ������������ ��� ����������
  oldKeyfileContents     <-  mapM fileGetBinary (findReqList o "OldKeyfile" ++ findReqList o "keyfile")
  -- ���������� ��������� ����, ������������� ��� ��������
  keyfileContents        <-  unlessNull fileGetBinary (findReqArg o "keyfile" "")
  -- ��������� ���� ������ � ���������� ��� -p? � ��� -p, ���� ��� ��������� �����
  let askPwd pwd          =  pwd=="?" || (pwd=="" && keyfileContents=="")
  -- ������ ���������� ������� � ������������� ����������, ��� Nothing �� �������� �������
  receipt                <-  newMVar Nothing

  -- �������������� command � ������������� ����������, ��� �������������
  -- ���������� ������ � ������������ � �������� keyfiles
  let cookPasswords command (ask_encryption_password, ask_decryption_password, bad_decryption_password) = do
        modifyMVar receipt $ \x -> do
          f <- x.$maybe makeReceipt return   -- ������� ������ ���������� ������� � ����������, ���� ��� ��� ���
          return (Just f, f command)         -- ��������� ������ � command � ��������� ��� ��� ����������� ����������
       where
        makeReceipt = do
          -- �������� � ������������ ������, ���� �� ����������� ��� ������
          let ask_password | cmdType cmd==ADD_CMD = ask_encryption_password parseData
                           | otherwise            = ask_decryption_password parseData
          asked_password  <-  any askPwd [dpwd,hpwd]  &&&  ask_password
          -- ������� � ������ ������� ���������� �������� ������������� ������ � ������ ������, ���� ��� ����������� ����� ���� ����������� keyfile
          asked_password      &&&  modifyMVar_ mvar_unpack_passwords (return.(asked_password:))
          oldKeyfileContents  &&&  modifyMVar_ mvar_unpack_passwords (return.("":))
          -- �������� � ������ ���������� keyfile � �������� ����������� "--"/"?". 0.75: �������� unicode2utf8 asked_password/pwd
          let cook "--"             = ""                                -- ���������� ���������
              cook pwd | askPwd pwd = asked_password++keyfileContents   -- ������, ������� � ���������� + ���������� keyfile
                       | otherwise  = pwd++keyfileContents              -- ������ �� ��������� ������ + ���������� keyfile
          return$ \command ->
                   command { opt_data_password    = cook dpwd
                           , opt_headers_password = cook hpwd
                           , opt_decryption_info  = (dont_ask_passwords, mvar_unpack_passwords, oldKeyfileContents, ask_decryption_password parseData, bad_decryption_password)}


-------------------------------------------------------------------------------------
-- ������ �� ������
  -- �������� ���������� ������
  let update_type = case cmd of
        "f"                       -> 'f'  -- ������� f: �������� ����� ����� ������� ��������, ����� ������ �� ���������
        "u"                       -> 'u'  -- ������� u: �������� ����� ����� ������� �������� � �������� ����� �����
        _ | findNoArg o "freshen" -> 'f'  -- �����  -f: ��. ����
          | findNoArg o "update"  -> 'u'  -- �����  -u: ��. ����
          | findNoArg o "sync"    -> 's'  -- ����� --sync: �������� ����� � ������ � ������������ � ������� �� �����
          | otherwise             -> 'a'  -- �����: �������� ����� � ������ �� ������ � ����� � �������� ����� �����

  -- ������� ����� �� ���������, ���� ������������ ����� "-k" ��� ������� "k"
  let lock_archive  =  findNoArg o "lock" || cmd=="k"

  -- ������� ������������ �����, ���� ������������ ����� "-d[f]" ��� ������� "m[f]"
  delete_files  <-  case (findNoArg o "delete"   || cmd=="m"
                         ,findNoArg o "delfiles" || cmd=="mf")
                      of
                         (False, False) -> return NO_DELETE
                         (False, True ) -> return DEL_FILES
                         (True , False) -> return DEL_FILES_AND_DIRS
                         (True , True ) -> registerError$ CMDLINE_INCOMPATIBLE_OPTIONS "m/-d" "mf/-df"

  -- �������� ������������� ������������� �����
  when (clear_archive_bit && delete_files/=NO_DELETE) $
      registerError$ CMDLINE_INCOMPATIBLE_OPTIONS "m[f]/-d[f]" "-ac"

  -- ������� ��� ��������� ������ - ����� ���� ������ ���� ��� ����� ���������� �����
  -- "" �������� ������������� �����. ��� �� �������� ��������� ������
  workdir <- case orig_workdir of
               '%':envvar -> getEnv envvar
               dir        -> return dir

  setup_command <<= (setTempDir workdir)

  -- ���������� ������� ���������� ������ � ������
  let sort_order  =  case (orig_sort_order, group_data) of
        (Just "-", _)  -> ""                    -- ���� ������� ���������� ����� ��� "-", �� ��������� ����������
        (Just  x,  _)  -> x                     -- ���� ������� ���������� ��� ���� ������, �� ������������ ���
        (_, [GroupNone]) -> ""                  -- ���� �� ������������ solid-������ - ��������� ����������
        _  -> if getMainCompressor data_compressor
                 .$anyf [(==aNO_COMPRESSION), isFakeCompressor, isVeryFastCompressor]
                then ""                         -- ���� -m0/--nodata/--crconly/tor:1..4/lzp:h13..15 - ����� ��������� ����������
                else aDEFAULT_SOLID_SORT_ORDER  -- ����� - ������������ ����������� ������� ���������� ��� solid-�������

  -- ��������, ��� ����� "-rr" ��������� ���� �� ���������� ��������
  let rr_ok = recovery `elem` ["","-","--"]
              || snd(parseNumber recovery 'b') `elem` ['b','%','p']
              || ';' `elem` recovery
              || '*' `elem` recovery
  unless rr_ok $ do
    registerError$ INVALID_OPTION_VALUE "recovery" "rr" ["MEM", "N", "N%", "MEM;SS", "N%;SS", "N*SS", "-", ""]

  -- ��������� ������� � ������������ � ���������� ������
  ref_overwrite  <-  newIORef$ case (yes,   overwrite) of
                                    (_,     "+")  ->  "a"
                                    (_,     "-")  ->  "s"
                                    (True,  _  )  ->  "a"
                                    (False, "p")  ->  " "


  -- ������ ��������, ������� ���� ��������� ��������������� ����� ������� ���������� �������
  setup_command'  <-  listVal setup_command >>== sequence_


------------------------------------------------------------------------------------------------
-- ������ �� ��� � ���������, �������������� ����������� ������� � ����������� ����� ���������
  return$ Just$ Command {
      cmd_args                 = args
    , cmd_additional_args      = additional_args
    , cmd_name                 = cmd
    , cmd_arcspec              = arcspec
    , cmd_arclist              = error "Using uninitialized cmd_arclist"
    , cmd_arcname              = error "Using uninitialized cmd_arcname"
    , cmd_archive_filter       = error "Using uninitialized cmd_archive_filter"
    , cmd_filespecs            = filespecs
    , cmd_added_arcnames       = return []
    , cmd_diskfiles            = return []
    , cmd_subcommand           = False
    , cmd_setup_command        = setup_command'

    , opt_scan_subdirs         = findNoArg    o "recursive"
    , opt_add_dir              = findNoArg    o "adddir"
    , opt_add_exclude_path     = add_exclude_path
    , opt_dir_exclude_path     = dir_exclude_path
    , opt_arc_basedir          = findReqArg   o "arcpath"   "" .$ translatePath .$ dropTrailingPathSeparator
    , opt_disk_basedir         = findReqArg   o "diskpath"  "" .$ translatePath .$ dropTrailingPathSeparator
    , opt_no_nst_filters       = null nst_filters
    , opt_file_filter          = file_filter
    , opt_group_dir            = group_dir
    , opt_group_data           = group_data
    , opt_data_compressor      = data_compressor
    , opt_dir_compressor       = dir_compressor
    , opt_autodetect           = ma_opt
    , opt_include_dirs         = include_dirs
    , opt_indicator            = indicator
    , opt_display              = display
    , opt_overwrite            = ref_overwrite
    , opt_keep_time            = findNoArg    o "keeptime"
    , opt_time_to_last         = findNoArg    o "timetolast"
    , opt_global_queueing      = global_queueing_opt
    , opt_test                 = test_opt
    , opt_pretest              = readInt pretest
    , opt_keep_broken          = findNoArg    o "keepbroken"
    , opt_match_with           = match_with
    , opt_append               = findNoArg    o "append"
    , opt_recompress           = recompress
    , opt_keep_original        = keep_original
    , opt_noarcext             = noarcext
    , opt_nodir                = findNoArg    o "nodir"
    , opt_nodates              = findNoArg    o "nodates"
    , opt_compression_cache    = compression_cache
    , opt_decompression_cache  = decompression_cache
    , opt_update_type          = update_type
    , opt_x_include_dirs       = x_include_dirs
    , opt_sort_order           = sort_order
    , opt_reorder              = False
    , opt_find_group           = find_group . fiFilteredName
    , opt_groups_count         = length group_strings
    , opt_find_type            = find_type  . fiFilteredName
    , opt_types_count          = maximum group_types + 1
    , opt_group2type           = (listArray0 group_types!)
    , opt_arccmt_file          = findOptArg   o "arccmt"            (if cmd=="c"  then ""  else "--")   -- ������� "c" ������������ ������� "ch -z"
    , opt_arccmt_str           = findReqArg   o "archive-comment"   ""
    , opt_lock_archive         = lock_archive
    , opt_sfx                  = sfx
    , opt_logfile              = findReqArg   o "logfile"           ""
    , opt_delete_files         = delete_files
    , opt_create_in_workdir    = create_in_workdir
    , opt_clear_archive_bit    = clear_archive_bit
    , opt_language             = language
    , opt_recovery             = recovery
    , opt_broken_archive       = broken_archive
    , opt_original             = findOptArg   o "original"          "--"
    , opt_save_bad_ranges      = findReqArg   o "save-bad-ranges"   ""
    , opt_pause_before_exit    = pause_before_exit
    , opt_shutdown             = shutdown
    , opt_limit_compression_memory   = climit
    , opt_limit_decompression_memory = dlimit
    , opt_volumes              = findReqList   o "volume"  .$lastElems 1  .$map (parseSize 'm')
    , opt_archive_type         = archive_type
    , opt_archive_extension    = archive_extension
    , opt_7z_compression       = compression_options

    , opt_encryption_algorithm = encryptionAlgorithm
    , opt_cook_passwords       = cookPasswords
    , opt_data_password        = error "opt_data_password used before cookPasswords!"
    , opt_headers_password     = error "opt_headers_password used before cookPasswords!"
    , opt_decryption_info      = error "opt_decryption_info used before cookPasswords!"

    , opt_parseFile            = parseFile
    , opt_unParseFile          = unParseFile
    , opt_parseData            = parseData
    , opt_unParseData          = unParseData
    }


{-# NOINLINE testOption #-}
-- |���������, ��� ����� ��������� ���� �� ����������� ��������
testOption fullname shortname option valid_values = do
  unless (option `elem` valid_values) $ do
    registerError$ INVALID_OPTION_VALUE fullname shortname valid_values

{-# NOINLINE addArcExtension #-}
-- |���� ��� ������ �� �������� ���������� � �� ������������ ����� --noarcext,
-- �� �������� � ���� ���������� �� ���������
addArcExtension noarcext default_ext filespec =
  case (hasExtension filespec, noarcext) of
    (False, False)  ->  filespec ++ default_ext
    _               ->  filespec

{-# NOINLINE replace_list_files #-}
-- |�������� ������ �� ����-����� ("@listfile") �� ����������
replace_list_files parseFile  =  concatMapM $ \filespec ->
  case (startFrom "@" filespec) of
    Just listfile  ->  parseFile 'l' listfile >>== map removeComment >>== deleteIf null
    _              ->  return [filespec]
  where
    -- Remove "//" comment and preceding spaces from the string
    removeComment str | pos:_ <- strPositions str "//"  = str .$take pos .$trimRight
                      | otherwise                       = str


-- |���� ���. ������ ������������ � ���� ������ ��������� @cmdfile, �� ���� ��������� � �� ���������� �����
processCmdfile args =
  case args of
    ['@':cmdfile] -> fileGetBinary cmdfile >>== utf8_to_unicode >>== splitArgs
    _             -> return args

 where -- ��������� ������ � ����������� �� ��������� ��������� (�������� �������� � unparseCommand)
       splitArgs = parseArg . dropWhile isSpace
       parseArg ""          =  []
       -- ����������� �������� ��������� ���. ������: "param" -> param, "dir\\" -> dir\ --
       parseArg ('"':rest)  =  let (arg,_:rest1) = break (=='"') rest
                                 in (replaceAtEnd "\\\\" "\\" arg):splitArgs rest1
       parseArg rest        =  let (arg,rest1) = break isSpace rest
                                 in arg:splitArgs rest1


-- |������ ���������� ����� "-s"
parseSolidOption opt =
  case (split ';' opt) of
    []        ->  ([aDEFAULT_DIR_GROUPING], [GroupAll], "")   -- "-s" �������� ����� �����-���� ��� ���� ������ � ����� �������� ������
    ["-"]     ->  ([aDEFAULT_DIR_GROUPING], [GroupNone], "")  -- "-s-" ��������� �����-������, ��� ��������� ������������ ����������� �����������
    ["7z"]    ->  ([GroupAll],  [GroupAll], "")               -- "-s=7z"  ������ ����� ������ ������� � ���� �����-���� ��� ���� ������ � ������
    ["cab"]   ->  ([GroupAll],  [GroupAll],  "0")   --  -dm0  -- "-s=cab" ������ ����� �������� ������� � ���� �����-���� ��� ���� ������ � ������
    ["zip"]   ->  ([GroupAll],  [GroupNone], "0")   --  -dm0  -- "-s=zip" ������ ��������� �����-���� ��� ������� ����� � ������, � ����� �������� �������
    ["arj"]   ->  ([GroupNone], [GroupNone], "0")   --  -dm0  -- "-s=arj" ������ ��������� �����-���� � ������� ��� ������� ����� � ������
    [dat]     ->  ([aDEFAULT_DIR_GROUPING], parse dat, "")    -- "-sXXX" ����� ����������� ������ ��� �����-������, �������� ������������ ����������
    [dir,dat] ->  (parse dir, parse dat, "")                  -- "-sXXX;YYY" ����� ����������� � ��� ���������, � ��� �����-������
  where
    -- ��������� �������� ����������� ������:
    --   "-s/-se/-s10m/-s100f" - ������������ ���/�� ����������/�� 10 ��/�� 100 ������, ��������������.
    -- `parse1` ������������ ���� �������� �����������,
    -- � `parse` - �� ������������������, �������� -se100f10m
    parse = map parse1 . recursive (split "")
      where split text ""                          =  (reverse text,      "")
            split ""   ('e':xs)                    =  ("e",               xs)
            split text ('e':xs)                    =  (reverse text,      'e':xs)
            split text (x:xs)   | not (isDigit x)  =  (reverse (x:text),  xs)
                                | otherwise        =  split (x:text) xs

    parse1 s = case s of
                ""  -> GroupAll
                "e" -> GroupByExt
                _   -> case (parseNumber s 'f') of
                         (num, 'b') -> GroupBySize (i num)
                         (1,   'f') -> GroupNone
                         (num, 'f') -> GroupByNumber (i num)
                         _          -> error$ "bad solid grouping option '"++s++"'"

