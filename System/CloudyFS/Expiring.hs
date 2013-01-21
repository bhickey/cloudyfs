module System.CloudyFS.Expiring where

import Data.DateTime

class Expiring a where
  expiresAt :: a -> DateTime
  ifValid :: DateTime -> a -> Maybe a
  ifValid t x = do
    if isValid t x then Just x else Nothing
  isValid :: DateTime -> a -> Bool
  isValid t x = t > expiresAt x
