module System.CloudyFS.Path where

import System.CloudyFS.Weather

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

type FileAction = [FilePart] -> IO (Maybe Weather)
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
    Just _ -> Just (normalisePath fp, f)
    _ -> Nothing

makeWeather :: [FilePart] -> IO (Maybe Weather)
makeWeather fp = fetchWeather (fp !! 2)

weatherStation :: FileMatcher
weatherStation = makeFileMatcher (RegularFile makeWeather) "^/us/[A-Z]{4}$"

countryUS :: FileMatcher
countryUS = makeFileMatcher DirectoryFile "^/us/?$"

fsRoot :: FileMatcher
fsRoot = makeFileMatcher DirectoryFile "^/$"

fileSpecifications :: [FileMatcher]
fileSpecifications = [fsRoot, countryUS, weatherStation]

matchSpec :: FilePath -> [([FilePath], File)]
matchSpec path =
  mapMaybe (\x -> x path) fileSpecifications 
