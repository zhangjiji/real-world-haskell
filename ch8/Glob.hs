module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: FilePath -> [FilePath]
namesMatching path = [path]
