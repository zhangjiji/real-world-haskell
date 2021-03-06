module BetterPredicate where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
-- import System.Time (ClockTime(..)) -- old time. deprecated
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)
import System.IO (openFile, hClose, hFileSize, IOMode(..))
import Control.Exception (bracket, handle, SomeException)

import RecursiveContents (getRecursiveContents)

betterFind :: InfoP Bool -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
                    where check name = do
                            perms <- getPermissions name
                            size <- getFileSize name
                            modified <- getModificationTime name
                            return (p name perms size modified)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler $ bracket (openFile path ReadMode) hClose $ \x -> do
  size <- hFileSize x
  return (Just size)
  where handler :: SomeException -> IO (Maybe Integer)
        handler _ = return Nothing

type InfoP a = FilePath
               -> Permissions
               -> Maybe Integer
               -> UTCTime
               -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP g f k x y z w = f x y z w `g` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 c f g x y z w = f x y z w `c` g x y z w

andP = liftP2 (&&)
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension ==? ".hs") &&? (sizeP >? 131072)

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP
infix 4 ==?


(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP
infix 3 &&?

(||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(||?) = orP
infix 2 ||?

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP
infix 4 >?

(<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(<?) = lesserP
infix 4 <?

