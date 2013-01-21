module System.CloudyFS.FileSystem where

import System.CloudyFS.Expiring
import Data.DateTime (DateTime, fromSeconds)

import System.FilePath.Posix

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M

type Database = IORef (FileSystem Int)

type IntFS = FileSystem Int

data RegularFilePath = RegularFilePath [DirPart] FilePart deriving (Show)
data RegularDirPath = RegularDirPath [DirPart] deriving (Show)
data FilePart = FilePart String deriving (Eq, Ord, Show)
data DirPart = DirPart String deriving (Eq, Ord, Show)

existsAsDir :: FilePart -> FileSystem a -> Bool
existsAsDir (FilePart s) (FileSystem sd c) =
  (M.member (DirPart (s ++ "/")) sd)

existsAsFile :: DirPart -> FileSystem a -> Bool
existsAsFile (DirPart s) (FileSystem sd c) =
  (M.member (FilePart (drop ((length s) - 1) s)) c)

data FileSystem a = FileSystem 
  { 
    subdirs :: Map DirPart (FileSystem a),
    contents :: Map FilePart a
  }
  deriving (Eq, Show)

instance (Expiring a) => Expiring (FileSystem a) where
  expiresAt (FileSystem sd c) = 
    let contentExpiry = getExpiry c
        subdirExpiry = getExpiry sd in
      max contentExpiry subdirExpiry
    where
      getExpiry mp = 
        case M.elems mp of
          [] -> fromSeconds 0
          e -> foldl1 max (map expiresAt e)

expire :: (Expiring a) => DateTime -> FileSystem a -> Maybe (FileSystem a)
expire t (FileSystem sd c) =
  let sd' = M.mapMaybe (expire t) sd
      c'  = M.mapMaybe (ifValid t) c in
    if M.null sd' && M.null c'
    then Nothing
    else Just $ FileSystem sd' c'

makePath :: FilePath -> Either RegularFilePath RegularDirPath
makePath fp = 
  let path = splitPath $ normalise fp
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
  Maybe (FileSystem a)
insertFile datum (RegularFilePath [] fp) fs@(FileSystem sd c) =
  if existsAsDir fp fs
  then Nothing
  else Just (FileSystem sd (M.insert fp datum c))
insertFile datum (RegularFilePath (h:t) fp) fs@(FileSystem sd c) =
  if existsAsFile h fs
  then Nothing
  else let subdir = M.findWithDefault emptyFS h sd in
         case insertFile datum (RegularFilePath t fp) subdir of
           Just d' -> Just (FileSystem (M.insert h d' sd) c)
           Nothing -> Nothing

insertDir ::
  RegularDirPath ->
  FileSystem a ->
  Maybe (FileSystem a)
insertDir (RegularDirPath []) fs = Just fs
insertDir (RegularDirPath (h:t)) fs@(FileSystem sd c) =
  if existsAsFile h fs
  then Nothing
  else let subdir = M.findWithDefault emptyFS h sd in
         case insertDir (RegularDirPath t) subdir of
           Just d' -> Just (FileSystem (M.insert h d' sd) c)
           Nothing -> Nothing

type Weather = ()
type Station = String
