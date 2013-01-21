module System.CloudyFS.FileSpec where

import System.FilePath.Posix

import Text.Regex.Base (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.String

data File =
    RegularFile
  | DirectoryFile

type FileMatcher = FilePath -> Maybe ([FilePath], File)

makeRegex :: String -> Regex
makeRegex s = case compile defaultCompOpt defaultExecOpt s of
  Right r -> r
  _ -> error "Regex compilation failure."

makeFileMatcher :: File -> String -> FileMatcher
makeFileMatcher f r fp =
  case execute (makeRegex r) fp of
    Right _ -> Just (splitPath fp, f)
    Left _ -> Nothing

zipCode :: FileMatcher
zipCode = makeFileMatcher RegularFile "/us/[0-9]{5}/+"

countryUS :: FileMatcher
countryUS = makeFileMatcher DirectoryFile "/us/+"
