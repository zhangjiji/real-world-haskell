module BetterPredicate where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import System.IO (openFile, hClose, hFileSize, IOMode(..))
import Control.Exception (bracket, handle)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\_ -> return Nothing) $ bracket (openFile path ReadMode) hClose $ \x -> do
  size <- hFileSize x
  return (Just size)
