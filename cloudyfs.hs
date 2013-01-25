module Main where

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

import System.CloudyFS.Path
import System.CloudyFS.FileSystem

type HT = FileType

type FileType = B.ByteString

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

fileStat :: Int -> FuseContext -> FileStat
fileStat sz ctx = FileStat 
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
  , statFileSize = fromIntegral (sz + 1)
  , statBlocks = 1
  , statAccessTime = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

stat :: FuseContext -> FileSystem B.ByteString -> FileStat
stat c (SystemDirectory _) = dirStat c
stat c (SystemFile b) = fileStat (B.length b) c

cloudyGetFileStat :: State -> FilePath -> IO (Either Errno FileStat)
cloudyGetFileStat stateRef p = do
  ctx <- getFuseContext
  state <- readIORef stateRef
  case getFile state path of
    Just fs -> return $ Right $ stat ctx fs
    Nothing ->
      case mapMaybe (\ x -> x p) fileSpecifications of
        (_, RegularFile action):[] -> do
          result <- action
          case mkfile state path result of
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
         err = Left eEXIST

getDirContents ::
  FileSystem FileType ->
  [FilePart] ->
  FuseContext ->
  [(FilePath, FileStat)]
getDirContents fs path ctx =
  case lsdir fs path of
    Nothing -> []
    Just m -> M.foldWithKey accumulator [] m
  where accumulator k fs l = (k, stat ctx fs):l 

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
    ctx <- getFuseContext
    case mapMaybe (\ x -> x path) fileSpecifications of
      (fp, DirectoryFile):_ -> do
        state <- readIORef stateRef
        return $ Right $ [(".", dirStat ctx)
                        ,("..", dirStat ctx)
                        ] ++ (getDirContents state fp ctx)
      _ -> return $ Left eEXIST

cloudyOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
cloudyOpen stateRef path ReadOnly flags = do
   state <- readIORef stateRef
   case getFile state (normalisePath path) of
     Nothing ->
       case mapMaybe (\ x -> x path) fileSpecifications of
         (fp, RegularFile _):[]  -> do
           case mkfile state fp (B.pack path) of
             Nothing -> return $ Left eACCES
             Just f -> do
               writeIORef stateRef f
               cloudyOpen stateRef path ReadOnly flags
         _ -> return $ Left eEXIST
     Just (SystemFile a) -> return $ Right a
     _ -> return $ Left eEXIST
cloudyOpen _ _ _ _ = return $ Left eACCES

cloudyRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
cloudyRead path ht byteCount offset =
  return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) ht

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
