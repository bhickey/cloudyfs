module Main where

import Data.IORef
import qualified Data.Set as S

import System.CloudyFS.FileSystem

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse

type HT = ()

logFile = "~/cloudyfs.log"

mkDatabase :: IO Database
mkDatabase = newIORef (emptyFS)

main :: IO ()
main = do
  db <- mkDatabase
  fuseMain (cloudyFSOps db) defaultExceptionHandler

cloudyFSOps :: Database -> FuseOperations HT
cloudyFSOps db = defaultFuseOps { fuseGetFileStat = cloudyGetFileStat
                            , fuseOpen = cloudyOpen db
                            , fuseRead = cloudyRead db
                            , fuseOpenDirectory = cloudyOpenDirectory
                            , fuseReadDirectory = cloudyReadDirectory db
                            , fuseGetFileSystemStats = cloudyGetFileSystemStats db
                            }

dirStat ctx = FileStat { statEntryType = Directory
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

fileStat fileName ctx = FileStat { statEntryType = RegularFile
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
    appendFile logFile ("file stat " ++ path ++ "\n")
    ctx <- getFuseContext
    case makePath (path ++ "/") of
      Left _ -> return $ Right $ fileStat path ctx
      Right _ -> return $ Right $ dirStat ctx

cloudyOpenDirectory path = do
  appendFile logFile ("open dir " ++ path ++ "\n")
  case makePath (path ++ "/") of
    Left _ -> return eEXIST
    Right p -> return eOK

cloudyReadDirectory :: Database -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
cloudyReadDirectory database path = do
    appendFile logFile ("read dir " ++ path ++ "\n")
    ctx <- getFuseContext
    db <- readIORef database
    case insertDir (getDirPath (path ++ "/")) db of
      Nothing -> return (Left eACCES)
      Just fs -> do
        writeIORef database fs
        return $ Right [(".", dirStat ctx)
                       ,("..", dirStat ctx)
                       ,("baz/", dirStat ctx)
                       ]
      where getDirPath path = case makePath path of Right d -> d

cloudyOpen :: Database -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
cloudyOpen database path ReadOnly flags = do
  db <- readIORef database
  case insertFile 1 (getFilePath path) db of
    Nothing -> return (Left eACCES)
    Just fs -> writeIORef database fs >> return (Right ())
    where getFilePath path = case makePath path of Left fp -> fp
cloudyOpen _ _ _ _ = return (Left eACCES)

cloudyRead :: Database -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
cloudyRead _ path _ byteCount offset = return $ Left eEXIST

cloudyGetFileSystemStats :: Database -> String -> IO (Either Errno FileSystemStats)
cloudyGetFileSystemStats _ str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
