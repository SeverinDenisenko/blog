module Files
  ( dumpFileContents,
    SystemException,
  )
where

import Control.Exception (throw)
import Control.Exception.Base (Exception)
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (unpack)
import System.Directory

data SystemException = SystemException deriving (Show)

instance Exception SystemException

dumpFileContents :: String -> IO String
dumpFileContents name = do
  exist <- doesFileExist name
  if exist
    then do
      string <- ByteString.readFile name
      return (unpack string)
    else
      throw SystemException
