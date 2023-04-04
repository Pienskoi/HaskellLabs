module Utils 
    ( timeToString,
      dateToString,
      stringToTime,
      stringToDate,
      timestamp
    ) where

import Data.Maybe (fromJust)
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Clock (getCurrentTime)

timeToString :: TimeOfDay -> String
timeToString t = formatTime defaultTimeLocale "%H:%M:%S" t

dateToString :: Day -> String
dateToString d = formatTime defaultTimeLocale "%Y-%m-%d" d

stringToTime :: String -> TimeOfDay
stringToTime str = fromJust $ parseTimeM True defaultTimeLocale "%H:%M:%S" str

stringToDate :: String -> Day
stringToDate str = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d" str

timestamp :: IO String
timestamp = do
    now <- getCurrentTime
    return $ formatTime defaultTimeLocale "%Y-%m-%d, %H:%M:%S" now
