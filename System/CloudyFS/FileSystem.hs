module System.CloudyFS.FileSystem where

import qualified Data.Map as M
import System.CloudyFS.Path

data FileSystem a =
    SystemFile a
  | SystemDirectory (M.Map FilePart (FileSystem a))
  deriving (Show)

type FileName = FilePart
type DirName = FilePart

emptyFS :: FileSystem a
emptyFS = SystemDirectory M.empty

newFile :: a -> FileSystem a
newFile dat = SystemFile dat

mkdir :: FileSystem a -> [FilePart] -> Maybe (FileSystem a)
mkdir f [] = Just f
mkdir (SystemFile _) _ = Nothing
mkdir (SystemDirectory contents) (h:t) =
  let d = M.findWithDefault emptyFS h contents in
    case (mkdir d t) of
      Nothing -> Nothing
      Just subdir -> Just $ SystemDirectory (M.insert h subdir contents)

lsdir :: FileSystem a -> [FilePart] -> Maybe (M.Map FilePart (FileSystem a))
lsdir (SystemFile _) _ = Nothing
lsdir (SystemDirectory contents) (h:t) =
  case M.lookup h contents of
    Nothing -> Nothing
    Just dir -> lsdir dir t
lsdir (SystemDirectory contents) [] = Just contents

mkfile :: FileSystem a -> [FilePart] -> a -> Maybe (FileSystem a)
mkfile (SystemFile _) _ _ = Nothing
mkfile (SystemDirectory _) [] _ = Nothing
mkfile (SystemDirectory contents) [nm] dat =
  case M.lookup nm contents of
    Just (SystemDirectory _) -> Nothing
    _ -> Just $ SystemDirectory (M.insert nm (newFile dat) contents)
mkfile (SystemDirectory contents) (h:t) dat =
  let subdir = M.findWithDefault emptyFS h contents in
    case mkfile subdir t dat of
      Nothing -> Nothing
      Just fs -> Just $ SystemDirectory (M.insert h fs contents)
    
