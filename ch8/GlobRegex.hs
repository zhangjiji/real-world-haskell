module GlobRegex (
  globToRegex,
--  matchGlob
  ) where

import Text.Regex.Posix((=~))
import Data.Char (toLower)

globToRegex :: String -> String
globToRegex cs = '^': globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' :  globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated ["
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass [] = error "unterminated"

metchesGlob :: FilePath -> String -> Bool
name `metchesGlob` pat = name =~ globToRegex pat

iMetchesGlob :: FilePath -> String -> Bool
name `iMetchesGlob` pat = (map toLower name) `metchesGlob` (map toLower pat)
