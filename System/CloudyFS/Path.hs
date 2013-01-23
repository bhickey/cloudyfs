module System.CloudyFS.Path where

import Data.Maybe
import System.FilePath.Posix
import Text.Regex.Base (defaultCompOpt, defaultExecOpt)
import Text.Regex.Base.RegexLike (matchOnce)
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.String

type FilePart = String

data File =
    RegularFile FileAction
  | DirectoryFile

type FileAction = IO Int
type FileMatcher = FilePath -> Maybe ([FilePath], File)

makeRegex :: String -> Regex
makeRegex s = case compile defaultCompOpt defaultExecOpt s of
  Right r -> r
  _ -> error "Regex compilation failure."

normalisePath :: FilePath -> [FilePart]
normalisePath fp = map removeTrailingSlash (splitPath fp)
  where removeTrailingSlash s =
          if last s == '/'
            then reverse $ tail $ reverse s
            else s

makeFileMatcher :: File -> String -> FileMatcher
makeFileMatcher f r fp = 
  case matchOnce (makeRegex r) fp of
    Just arr -> Just (normalisePath fp, f)
    _ -> Nothing

weatherStation :: FileMatcher
weatherStation = makeFileMatcher (RegularFile (return 2)) "^/us/[A-Z]{4}$"

zipCode :: FileMatcher
zipCode = makeFileMatcher (RegularFile (return 2)) "^/us/[0-9]{5}$"

countryUS :: FileMatcher
countryUS = makeFileMatcher DirectoryFile "^/us/?$"

fsRoot :: FileMatcher
fsRoot = makeFileMatcher DirectoryFile "^/$"

fileSpecifications :: [FileMatcher]
fileSpecifications = [fsRoot, zipCode, countryUS, weatherStation]

matchSpec :: FilePath -> [([FilePath], File)]
matchSpec path =
  mapMaybe (\x -> x path) fileSpecifications 
