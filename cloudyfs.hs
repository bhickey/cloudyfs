module Main where

import Data.Maybe
import Data.IORef

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse hiding (RegularFile, Directory)
import qualified System.Fuse as Fuse

import System.CloudyFS.FileSpec

type HT = ()

mkDatabase :: IO (IORef ())
mkDatabase = newIORef ()

main :: IO ()
main = do
  fuseMain cloudyFSOps defaultExceptionHandler

cloudyFSOps :: FuseOperations HT
cloudyFSOps = defaultFuseOps { fuseGetFileStat = cloudyGetFileStat
                            , fuseOpen = cloudyOpen
                            , fuseRead = cloudyRead
                            , fuseOpenDirectory = cloudyOpenDirectory
                            , fuseReadDirectory = cloudyReadDirectory
                            , fuseGetFileSystemStats = cloudyGetFileSystemStats
                            }

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType = Fuse.Directory
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
fileStat fileName ctx = FileStat { statEntryType = Fuse.RegularFile
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
  

cloudyOpenDirectory :: FilePath -> IO Errno
cloudyOpenDirectory path = do
  case mapMaybe (\ x -> x path) fileSpecifications of
    (_, DirectoryFile):[] -> do
      return $ eOK
    _ -> return $ eEXIST
 

cloudyReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
cloudyReadDirectory path = do
    ctx <- getFuseContext
    case mapMaybe (\ x -> x path) fileSpecifications of
      (_, DirectoryFile):_ -> do
        return $ Right [(".", dirStat ctx)
                        ,("..", dirStat ctx)
                        ]
      _ -> return $ Left eEXIST

cloudyOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
cloudyOpen path ReadOnly _ =
   case mapMaybe (\ x -> x path) fileSpecifications of
     [] -> return (Left eACCES)
     _ -> return (Right ())
cloudyOpen _ _ _ = return $ Left eACCES

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
