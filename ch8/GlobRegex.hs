module GlobRegex (
  globToRegex,
  matchesGlob
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

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

iMatchesGlob :: FilePath -> String -> Bool
name `iMatchesGlob` pat = (map toLower name) `matchesGlob` (map toLower pat)
