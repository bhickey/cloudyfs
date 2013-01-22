module System.CloudyFS.FileSpec where

import Data.Maybe

import System.FilePath.Posix

import Text.Regex.Base (defaultCompOpt, defaultExecOpt)
import Text.Regex.Base.RegexLike (matchTest)
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.String

data File =
    RegularFile
  | DirectoryFile
  deriving (Show)

type FileMatcher = FilePath -> Maybe ([FilePath], File)

makeRegex :: String -> Regex
makeRegex s = case compile defaultCompOpt defaultExecOpt s of
  Right r -> r
  _ -> error "Regex compilation failure."

makeFileMatcher :: File -> String -> FileMatcher
makeFileMatcher f r fp = 
  if (matchTest (makeRegex r) fp)
    then Just (splitPath fp, f)
    else Nothing

weatherStation :: FileMatcher
weatherStation = makeFileMatcher RegularFile "^/us/[A-Z]{4}$"

zipCode :: FileMatcher
zipCode = makeFileMatcher RegularFile "^/us/[0-9]{5}$"

countryUS :: FileMatcher
countryUS = makeFileMatcher DirectoryFile "^/us/?$"

fsRoot :: FileMatcher
fsRoot = makeFileMatcher DirectoryFile "^/$"

fileSpecifications = [fsRoot, zipCode, countryUS, weatherStation]

matchSpec :: FilePath -> [([FilePath], File)]
matchSpec path =
  mapMaybe (\x -> x path) fileSpecifications 
