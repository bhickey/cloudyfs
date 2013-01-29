module System.CloudyFS.Weather where

import Data.Convertible (convert)

import Data.Maybe (fromJust)

import Data.ByteString.Char8 hiding (length)
import Data.Time.Clock
import Data.Time.LocalTime

import System.Posix.Types

import Network.HTTP
import Text.HTML.TagSoup
import qualified Data.Time.RFC2822 as RFC2822

data Weather = Weather {
  time :: EpochTime,
  conditions :: String,
  temp :: String
}

asByteString :: Weather -> ByteString
asByteString w = pack $ conditions w ++ "\n" ++ temp w ++ "\n"

getURL :: String -> IO String
getURL x = getResponseBody =<< simpleHTTP (getRequest x)

fetchWeather :: String -> IO (Maybe Weather)
fetchWeather station = do
    tags <- fmap parseTags $ getURL url
    if tagsOK tags
      then return $ Just $ Weather (getDate tags) (getConditions tags) (getTemperature tags)
      else return Nothing
  where
    tagsOK tags = length (sections (~== "<weather>") tags) > 0
    url = "http://w1.weather.gov/xml/current_obs/" ++ station ++ ".xml"
    getText tag blob = fromTagText $ sections (~== tag) blob !! 0 !! 1
    getDate t = convert $ zonedTimeToUTC $ fromJust $ RFC2822.readRFC2822 (getText "<observation_time_rfc822>" t)
    getConditions = (getText "<weather>")
    getTemperature = (getText "<temperature_string>")
