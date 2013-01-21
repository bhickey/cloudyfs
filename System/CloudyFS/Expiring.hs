module System.CloudyFS.Expiring where

import Data.DateTime

class Expiring a where
  expiresAt :: a -> DateTime
  ifValid :: a -> IO (Maybe a)
  ifValid x = do
    v <- isValid x
    return $ if v then Just x else Nothing
  isValid :: a -> IO Bool
  isValid x = do
    t <- getCurrentTime
    return $ t > expiresAt x
