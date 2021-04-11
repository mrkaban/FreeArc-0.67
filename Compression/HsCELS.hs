{-# OPTIONS_GHC -cpp -fno-monomorphism-restriction #-}
----------------------------------------------------------------------------------------------------
---- �������� � ���������� ������.                                                              ----
---- ��������� � ����������� �� �� �����������, ������������ ��� �������� ������.               ----
----------------------------------------------------------------------------------------------------
module HsCELS where

import Control.Concurrent
import Control.OldException
import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe

import qualified TABI
infixr 7  ==>
a ==> b = TABI.Pair a b

----------------------------------------------------------------------------------------------------
----- Error codes ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

aFREEARC_OK                            =  0
aFREEARC_ERRCODE_GENERAL               = -1
aFREEARC_ERRCODE_INVALID_COMPRESSOR    = -2
aFREEARC_ERRCODE_ONLY_DECOMPRESS       = -3
aFREEARC_ERRCODE_OUTBLOCK_TOO_SMALL    = -4
aFREEARC_ERRCODE_NOT_ENOUGH_MEMORY     = -5
aFREEARC_ERRCODE_READ                  = -6
aFREEARC_ERRCODE_BAD_COMPRESSED_DATA   = -7
aFREEARC_ERRCODE_NOT_IMPLEMENTED       = -8
aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED = -9
aFREEARC_ERRCODE_OPERATION_TERMINATED  = -10
aFREEARC_ERRCODE_WRITE                 = -11

compressionErrorMessage x
  | x==aFREEARC_OK                            = "All OK"
  | x==aFREEARC_ERRCODE_GENERAL               = "0365 general (de)compression error in %1"
  | x==aFREEARC_ERRCODE_INVALID_COMPRESSOR    = "0366 invalid compression method or parameters in %1"
  | x==aFREEARC_ERRCODE_ONLY_DECOMPRESS       = "program build with FREEARC_DECOMPRESS_ONLY, so don't try to use compress"
  | x==aFREEARC_ERRCODE_OUTBLOCK_TOO_SMALL    = "output block size in (de)compressMem is not enough for all output data in %1"
  | x==aFREEARC_ERRCODE_NOT_ENOUGH_MEMORY     = "0498 can't allocate memory required for (de)compression in %1, use -lc/-ld to limit memory usage"
  | x==aFREEARC_ERRCODE_READ                  = "0430 read error (bad media?) in compression algorithm %1"
  | x==aFREEARC_ERRCODE_BAD_COMPRESSED_DATA   = "0369 bad compressed data in %1"
  | x==aFREEARC_ERRCODE_NOT_IMPLEMENTED       = "requested feature isn't supported in %1"
  | x==aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED = "required part of data was already decompressed"
  | x==aFREEARC_ERRCODE_OPERATION_TERMINATED  = "operation terminated by user"
  | x==aFREEARC_ERRCODE_WRITE                 = "0431 write error (disk full?) in compression algorithm %1"
  | otherwise                                 = "unknown (de)compression error "++show x++" in %1"


----------------------------------------------------------------------------------------------------
----- Compression library services -----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Useful definitions for kilobytes, megabytes and so on
b  = 1
kb = 1024*b
mb = 1024*kb
gb = 1024*mb
tb = 1024*gb

-- |Compress/decompress using callback
compress   = runWithCallback "compress"
-- |Decompress using callback
decompress = runWithCallback "decompress"

-- |Compress memory block
[compressMem, decompressMem :: Method -> Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int] =
  (flip map) (words "compressMem decompressMem")
             (\service method inbuf insize outbuf outsize  ->  run service method ["inbuf" ==> inbuf, "insize" ==> insize, "outbuf" ==> outbuf, "outsize" ==> outsize])

-- |Return canonical representation of compression method
canonizeCompressionMethod :: Method -> Method
canonizeCompressionMethod = mapMethod "canonize" []

-- |Set memory used to compress/decompress, dictionary or block size of method given
[setCompressionMem, setDecompressionMem, setDictionary, setBlockSize :: MemSize -> Method -> Method] =
  (flip map) (words "SetCompressionMem SetDecompressionMem SetDictionary SetBlockSize")
             (\service mem -> mapMethod service ["mem" ==> mem])

-- |Put upper limit to memory used to compress/decompress, dictionary or block size of method given
[limitCompressionMem, limitDecompressionMem, limitDictionary, limitBlockSize :: MemSize -> Method -> Method] =
  (flip map) (words "LimitCompressionMem LimitDecompressionMem LimitDictionary LimitBlockSize")
             (\service mem -> mapMethod service ["mem" ==> mem])

-- |Returns memory used to compress/decompress, dictionary or block size of method given
[getCompressionMem, getDecompressionMem, getDictionary, getBlockSize :: Method -> MemSize] =
  (flip map) (words "GetCompressionMem GetDecompressionMem GetDictionary GetBlockSize")
             (\service -> mapMethod service [])

-- |Check boolean property of compression method, returning FALSE if it's not implemented
compressionIs :: Property -> Method -> Bool
compressionIs property method  =  (askMethod property [] method) > 0

-- |Get value of compression method property or raise error if this property isn't supported
compressionGet :: Property -> Method -> Int
compressionGet property method  =  case (askMethod property [] method) of
                                     x | x<0 -> error$ "Error "++show x++" when querying "++method++"."++property
                                     x       -> x

-- |Adds new external compresion method definition
addExternalCompressor definition =
  withCString definition $ \c_definition -> do
    c_AddExternalCompressor c_definition


----------------------------------------------------------------------------------------------------
----- Internal auxiliary functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Run operation with callback
runWithCallback service method callback  =  run service method ["callback" ==> (callback :: TABI.FUNCTION)]

-- |Run operation
run service method params  =  TABI.call server ("service" ==> service : "method" ==> method : params)

-- |Map method to value
mapMethod service params method  =  unsafePerformIO$ TABI.callret server ("service" ==> service : "method" ==> method : params)

-- |Query method property
askMethod service params method  =  unsafePerformIO$ TABI.call server ("service" ==> service : "method" ==> method : params)


----------------------------------------------------------------------------------------------------
----- Imports from C++ compression library ---------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Server function implemented in C++
foreign import ccall safe "Compression.h CELS_Call"
  server :: TABI.C_FUNCTION

-- |Get/set number of threads used for (de)compression
foreign import ccall unsafe  "Compression.h  GetCompressionThreads"
  getCompressionThreads :: IO Int
foreign import ccall unsafe  "Compression.h  SetCompressionThreads"
  setCompressionThreads :: Int -> IO ()

-- |Clear external compressors table
foreign import ccall unsafe  "Compression.h  ClearExternalCompressorsTable"
  clearExternalCompressorsTable :: IO ()

-- |Adds new external compresion method to the table
foreign import ccall unsafe  "External/C_External.h  AddExternalCompressor"
  c_AddExternalCompressor :: CString -> IO Int

foreign import ccall unsafe  "Compression.h compressionLib_cleanup"
  compressionLib_cleanup :: IO ()

-- |Boolean flag set when we need fastest buffer-to-buffer compression
foreign import ccall "Compression.h &" compress_all_at_once :: Ptr CInt


----------------------------------------------------------------------------------------------------
----- Types and wrappers ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Compression method representation
type Method = String

-- |Compression method property
type Property = String

-- |Memory sizes
type MemSize = CUInt

-- |Unlimited memory usage
aUNLIMITED_MEMORY = maxBound::MemSize

-- |Universal integral types conversion routine
ii x = fromIntegral x

