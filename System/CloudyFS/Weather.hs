module System.CloudyFS.Weather where

import Data.ByteString.Char8

getWeatherFor :: [String] -> IO ByteString
getWeatherFor _ = do
  let url = "http://w1.weather.gov/xml/current_obs/KPVD.xml" in
    return $ pack (url ++ "\n")
--    do res <- simpleHTTP (getRequest url) >>= getResponseBody
--       return $ pack res
