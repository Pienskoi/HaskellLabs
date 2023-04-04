module Model
    ( Note(..),
      Notebook
    ) where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)

data Note = Contact { noteId :: Int,
                      name :: String,
                      phoneNumbers :: [String] 
                    }
          | Birthday { noteId :: Int,
                       name :: String,
                       date :: Day
                     }
          | Meeting { noteId :: Int,
                      date :: Day,
                      startTime :: TimeOfDay,
                      endTime :: TimeOfDay,
                      place :: String,
                      topic :: String,
                      isHeld :: Bool
                    } deriving (Eq, Show)

type Notebook = [Note]
