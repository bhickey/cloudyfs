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

type HT = ()

type State = IORef (FileSystem Int)

mkFileSystem :: IO State
mkFileSystem = newIORef emptyFS

main :: IO ()
main = do
  fs <- mkFileSystem
  fuseMain (cloudyFSOps fs) defaultExceptionHandler

cloudyFSOps :: State -> FuseOperations HT
cloudyFSOps fs = defaultFuseOps { fuseGetFileStat = cloudyGetFileStat
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

fileStat :: String -> FuseContext -> FileStat
fileStat fileName ctx = FileStat 
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
  , statFileSize = fromIntegral $ length fileName
  , statBlocks = 1
  , statAccessTime = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

cloudyGetFileStat :: FilePath -> IO (Either Errno FileStat)
cloudyGetFileStat path = do
  ctx <- getFuseContext
  case mapMaybe (\ x -> x path) fileSpecifications of
    [] -> return $ Left $ eBUSY
    (_, RegularFile _):[] -> do
      return $ Right $ fileStat path ctx
    (_, DirectoryFile):[] -> do
      return $ Right $ dirStat ctx
    _ -> return $ Left $ eEXIST
  
getDirContents ::
  FileSystem a ->
  [FilePart] ->
  FuseContext ->
  [(FilePath, FileStat)]
getDirContents fs path ctx =
  case lsdir fs path of
    Nothing -> []
    Just m -> M.foldWithKey accumulator [] m
  where accumulator k (SystemFile _) l = (k, fileStat k ctx):l 
        accumulator k (SystemDirectory _) l = (k, dirStat ctx):l 

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
cloudyOpen stateRef path ReadOnly _ =
   case mapMaybe (\ x -> x path) fileSpecifications of
     (fp, RegularFile _):[]  -> do
       state <- readIORef stateRef
       case mkfile state fp 1 of
         Nothing -> do
           appendFile "/home/brendan/cloudy.log" ("Failed to write file " ++ path ++ "\n")
           return (Left eACCES)
         Just f -> do
           appendFile "/home/brendan/cloudy.log" ("Wrote file " ++ path ++ "\n")
           writeIORef stateRef f
           return $ Right ()
     _ -> return (Left eACCES)
cloudyOpen _ _ _ _ = return $ Left eACCES

cloudyRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
cloudyRead path _ _ _ =
  return $ Right (B.pack $ path)

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
