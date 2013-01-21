module System.CloudyFS.Path where

import Data.List (groupBy)
import System.FilePath.Posix

--canonicalise :: FilePath -> [FilePath]
canonicalise fp = reverse $ foldl processPath [] $ filter (\ x -> x /= "/") $  splitPath $ normalise fp
  where processPath [] "../" = []
        processPath (hd:tl) "../" = tl
        processPath lst n = n:lst
