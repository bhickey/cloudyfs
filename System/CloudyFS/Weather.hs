module System.CloudyFS.Weather where

import Data.Convertible (convert)
import Data.Maybe (fromJust)
import Data.Time.LocalTime

import Data.ByteString.Char8 hiding (length)
import System.CloudyFS.Expiring
import System.Posix.Types
import Network.HTTP
import Text.HTML.TagSoup

import Data.DateTime (DateTime, addMinutes)
import qualified Data.Time.RFC2822 as RFC2822

data Weather = Weather {
  time :: DateTime,
  epochTime :: EpochTime,
  conditions :: String,
  temp :: String
}

instance Expiring Weather where
  expiresAt (Weather _ t _ _) = addMinutes 60 (convert t)

asByteString :: Weather -> ByteString
asByteString w = pack $ conditions w ++ "\n" ++ temp w ++ "\n"

getURL :: String -> IO String
getURL x = getResponseBody =<< simpleHTTP (getRequest x)

fetchWeather :: String -> IO (Maybe Weather)
fetchWeather station = do
    tags <- fmap parseTags $ getURL url
    return $ makeWeather tags
  where 
    url = "http://w1.weather.gov/xml/current_obs/" ++ station ++ ".xml"

makeWeather :: [Tag String] -> Maybe Weather
makeWeather tags =
  if tagsOK tags
    then Just $ Weather (getDate tags) (getEpochDate tags) (getConditions tags) (getTemperature tags)
    else Nothing
  where
    tagsOK tg = length (sections (~== "<weather>") tg) > 0
    getText tg blob = fromTagText $ sections (~== tg) blob !! 0 !! 1
    parseDate t = fromJust $ RFC2822.readRFC2822 (getText "<observation_time_rfc822>" t)
    getEpochDate t = convert $ zonedTimeToUTC $ parseDate t
    getDate t = convert $ parseDate t
    getConditions = (getText "<weather>")
    getTemperature = (getText "<temperature_string>")
