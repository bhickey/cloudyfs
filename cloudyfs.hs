module Main where

import Data.DateTime (DateTime, getCurrentTime)
import Data.Time.LocalTime
import Data.Convertible (convert)

import Data.Maybe
import Data.IORef

import qualified Data.Map as M

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse hiding (RegularFile, Directory)
import qualified System.Fuse as Fuse

import System.CloudyFS.Expiring
import System.CloudyFS.FileSystem
import System.CloudyFS.Path
import System.CloudyFS.Weather

type FileType = Weather
type HT = FileType
type State = IORef (FileSystem FileType)

mkFileSystem :: IO State
mkFileSystem = newIORef emptyFS

main :: IO ()
main = do
  fs <- mkFileSystem
  fuseMain (cloudyFSOps fs) defaultExceptionHandler

cloudyFSOps :: State -> FuseOperations HT
cloudyFSOps fs = defaultFuseOps { fuseGetFileStat = cloudyGetFileStat fs
                            , fuseOpen = cloudyOpen fs
                            , fuseRead = cloudyRead
                            , fuseOpenDirectory = cloudyOpenDirectory fs
                            , fuseReadDirectory = cloudyReadDirectory fs
                            , fuseGetFileSystemStats = cloudyGetFileSystemStats
                            }

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat
  { statEntryType = Fuse.Directory
  , statFileMode = foldr1 unionFileModes
                     [ ownerReadMode
                     , ownerExecuteMode
                     , groupReadMode
                     , groupExecuteMode
                     , otherReadMode
                     , otherExecuteMode
                     ]
  , statLinkCount = 2
  , statFileOwner = fuseCtxUserID ctx
  , statFileGroup = fuseCtxGroupID ctx
  , statSpecialDeviceID = 0
  , statFileSize = 4096
  , statBlocks = 1
  , statAccessTime = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

fileStat :: Weather -> FuseContext -> FileStat
fileStat w ctx = FileStat 
  { statEntryType = Fuse.RegularFile
  , statFileMode = foldr1 unionFileModes
                     [ ownerReadMode
                     , groupReadMode
                     , otherReadMode
                     ]
  , statLinkCount = 1
  , statFileOwner = fuseCtxUserID ctx
  , statFileGroup = fuseCtxGroupID ctx
  , statSpecialDeviceID = 0
  , statFileSize = fromIntegral $ B.length $ asByteString w
  , statBlocks = 1
  , statAccessTime = 0
  , statModificationTime = epochTime w 
  , statStatusChangeTime = 0
  }

stat :: FuseContext -> FileSystem FileType -> FileStat
stat c (SystemDirectory _) = dirStat c
stat c (SystemFile w) = fileStat w c

cloudyGetFileStat :: State -> FilePath -> IO (Either Errno FileStat)
cloudyGetFileStat stateRef p = do
  t <- getCurrentTime
  ctx <- getFuseContext
  state <- readIORef stateRef
  case getFile t state path of
    Just fs -> return $ Right $ stat ctx fs
    Nothing ->
      case mapMaybe (\ x -> x p) fileSpecifications of
        (_, RegularFile action):[] -> do
          result <- action path
          case result of
            Nothing -> return err
            Just r ->
              case mkfile state path r of
                Just f -> do
                  writeIORef stateRef f
                  cloudyGetFileStat stateRef p
                Nothing -> return err
        (_, DirectoryFile):[] -> do
          case mkdir state path of
            Just f -> do
              writeIORef stateRef f
              cloudyGetFileStat stateRef p
            Nothing -> return err
        _ -> return err
   where path = normalisePath p
         err = Left eNFILE

getDirContents ::
  DateTime ->
  FileSystem FileType ->
  [FilePart] ->
  FuseContext ->
  [(FilePath, FileStat)]
getDirContents t fs path ctx =
  case lsdir fs path of
    Nothing -> []
    Just m -> M.foldWithKey accumulator [] m
  where
    accumulator k f l =
      if isValid t f
        then (k, stat ctx f):l 
        else l

cloudyOpenDirectory :: State -> FilePath -> IO Errno
cloudyOpenDirectory stateRef path = do
  case mapMaybe (\ x -> x path) fileSpecifications of
    (fp, DirectoryFile):[] -> do
      st <- readIORef stateRef
      case mkdir st fp of
        Just f -> do
          writeIORef stateRef f
          return $ eOK
        Nothing -> return $ eOK
    _ -> return $ eEXIST
 
cloudyReadDirectory :: State -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
cloudyReadDirectory stateRef path = do
    t <- getCurrentTime
    ctx <- getFuseContext
    case mapMaybe (\ x -> x path) fileSpecifications of
      (fp, DirectoryFile):_ -> do
        state <- readIORef stateRef
        return $ Right $ [(".", dirStat ctx)
                        ,("..", dirStat ctx)
                        ] ++ (getDirContents t state fp ctx)
      _ -> return $ Left eEXIST

cloudyOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
cloudyOpen stateRef path ReadOnly flags = do
   t <- getCurrentTime
   state <- readIORef stateRef
   case getFile t state (normalisePath path) of
     Nothing -> fetchAndPut state
     Just (SystemFile a) -> 
       if isValid t a
         then return $ Right a
         else fetchAndPut state
     _ -> return $ Left eNFILE
  where
    fetchAndPut state =
      case mapMaybe (\ x -> x path) fileSpecifications of
        (fp, RegularFile act):[]  -> do
          w <- act fp
          case w of
            Nothing -> return $ Left eNFILE
            Just r -> 
              case mkfile state fp r of
                Nothing -> return $ Left eACCES
                Just f -> do
                  writeIORef stateRef f
                  cloudyOpen stateRef path ReadOnly flags
        _ -> return $ Left eNFILE

cloudyOpen _ _ _ _ = return $ Left eACCES

cloudyRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
cloudyRead _ ht byteCount offset =
  return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) $ asByteString ht

cloudyGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
cloudyGetFileSystemStats _ =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
