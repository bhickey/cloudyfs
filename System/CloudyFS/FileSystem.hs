module System.CloudyFS.FileSystem where

import System.FilePath.Posix

import Data.Map (Map)
import qualified Data.Map as M

import Data.IORef

type Database = IORef (FileSystem Int)

data RegularFilePath = RegularFilePath [DirPart] FilePart deriving (Show)
data RegularDirPath = RegularDirPath [DirPart] deriving (Show)
data FilePart = FilePart String deriving (Eq, Ord, Show)
data DirPart = DirPart String deriving (Eq, Ord, Show)

data FileSystem a = FileSystem 
  { 
    subdirs :: Map DirPart (FileSystem a),
    contents :: Map FilePart a
  }
  deriving (Eq, Show)

makePath :: FilePath -> Either RegularFilePath RegularDirPath
makePath fp = 
  let path = splitPath fp
      rpath = reverse path in
    if (last . head $ rpath) == '/'
    then Right $ RegularDirPath $ map DirPart path
    else Left $ RegularFilePath
      (map DirPart (reverse $ tail rpath))
      (FilePart (head rpath))

emptyFS = FileSystem M.empty M.empty

insertFile :: 
  a -> 
  RegularFilePath ->
  FileSystem a ->
  FileSystem a
insertFile datum (RegularFilePath [] fp) (FileSystem sd c) =
  (FileSystem sd (M.insert fp datum c))
insertFile datum (RegularFilePath (h:t) fp) (FileSystem sd c) =
  let subdir = M.findWithDefault emptyFS h sd
      updatedDir = insertFile datum (RegularFilePath t fp) subdir in
    (FileSystem (M.insert h updatedDir sd) c)

insertDir ::
  RegularDirPath ->
  FileSystem a ->
  FileSystem a
insertDir (RegularDirPath []) fs = fs
insertDir (RegularDirPath (h:t)) (FileSystem sd c) =
  let subdir = M.findWithDefault emptyFS h sd
      updatedDir = insertDir (RegularDirPath t) subdir in
    (FileSystem (M.insert h updatedDir sd) c)

type Weather = ()
type Station = String
