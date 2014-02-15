module ControlledVisit (Info, getUsefulContents, getInfo)
       where

import System.Directory (Permissions(..), getModificationTime, getPermissions, getDirectoryContents)
import Data.Time.Clock (UTCTime)
import Control.Monad (mapM, forM, liftM)
import System.FilePath ((</>))
import System.IO (openFile, hClose, hFileSize, IOMode(..))
import Control.Exception (bracket, handle, SomeException)

data Info = Info {
  infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  time <- maybeIO (getModificationTime path)
  return (Info path perms size time)

maybeIO :: IO a -> IO (Maybe a)
-- maybeIO act = handle (\(_::SomeException) -> return Nothing) (Just `liftM` act) -XScopedTypeVariables version
maybeIO act = handle handler $ (Just `liftM` act)
  where handler :: SomeException -> IO (Maybe a)
        handler _ = return Nothing

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

preOrder = id
postOrder (x:xs) = xs ++ [x]

-- P228 ex3, Take the predicates and combinators from "Gluing Predicates Together" on page 224 and make them work with our new Info type.

infoEq :: (Eq a) => (Info -> a) -> a -> Info -> Bool
infoEq accessF k x = accessF x == k

-- P228 ex4, Write a wrapper for traverse that lets you control traversal using one predicate and filter results using another.
