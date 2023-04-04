import Data.Time
import Data.List

data Note = Contact { name :: String,
                      phoneNumbers :: [String] 
                    }
          | Birthday { name :: String,
                       date :: Day
                     }
          | Meeting { date :: Day,
                      startTime :: TimeOfDay,
                      endTime :: TimeOfDay,
                      place :: String,
                      topic :: String,
                      isHeld :: Bool
                    } deriving (Show)

type Notebook = [Note]

class Search a where
    searchByName :: String -> a -> a
    searchByNamePart :: String -> a -> a
    searchByPhoneNumber :: String -> a -> a
    searchMeetingsByDay :: DayOfMonth -> MonthOfYear -> Year -> a -> a
    searchMeetingsByDayRange :: (DayOfMonth, MonthOfYear, Year) -> (DayOfMonth, MonthOfYear, Year) -> a -> a
    getHeldMeetings :: a -> a
    getMeetingsStatistics :: a -> [(String, Int)]

class Combine a where
    combineContactsByName :: String -> [a] -> a

instance Monoid Note where
    mempty = Contact "" []
    mappend = (<>)

instance Semigroup Note where
    (Contact n1 pn1) <> (Contact n2 pn2)
        | n1 == n2 = Contact n1 (pn1 ++ pn2)
        | n2 == "" && pn2 == [] = Contact n1 pn1
        | otherwise = error "Can't combine Contacts with different names"
    note1 <> note2 = error $ "Can't combine " ++ show note1 ++ " with " ++ show note2

instance Search Notebook where
    searchByName name = filter matchName
        where 
            matchName (Contact n _) = n == name
            matchName (Birthday n _) = n == name
            matchName _ = False

    searchByNamePart namePart = filter matchNamePart 
        where 
            matchNamePart (Contact n _) = isInfixOf namePart n
            matchNamePart (Birthday n _) = isInfixOf namePart n
            matchNamePart _ = False

    searchByPhoneNumber phoneNumber = filter matchPhoneNumber
        where 
            matchPhoneNumber (Contact _ pns) = phoneNumber `elem` pns
            matchPhoneNumber _ = False

    searchMeetingsByDay day month year = filter matchDate
        where
            matchDate (Meeting d _ _ _ _ _) = d == fromGregorian year month day
            matchDate _ = False

    searchMeetingsByDayRange (d1, m1, y1) (d2, m2, y2) = filter matchDayRange
        where
            matchDayRange (Meeting d _ _ _ _ _) = d >= fromGregorian y1 m1 d1 && d <= fromGregorian y2 m2 d2
            matchDayRange _ = False

    getHeldMeetings = filter isHeld
        where
            isHeld (Meeting _ _ _ _ _ h) = h
            isHeld _ = False

    getMeetingsStatistics nb = map (\group -> (getMonth (head group), length group)) $ groupBy (\m1 m2 -> getMonth m1 == getMonth m2) $ filter isMeeting nb
            where
                getMonth (Meeting d _ _ _ _ _) = formatTime defaultTimeLocale "%B" d 
                isMeeting (Meeting _ _ _ _ _ _) = True
                isMeeting _ = False

instance Combine Note where
    combineContactsByName name nb = mconcat $ filter matchName nb
        where
            matchName (Contact n _) = n == name
            matchName _ = False


main :: IO ()
main = do
    let myNotebook = [ 
                       Contact "Volodymyr Pienskoi" ["+380501234567"],
                       Contact "Volodymyr Pienskoi" ["+380670987654"],
                       Contact "Volodymyr Zelenskyi" ["+380739876543"],
                       Birthday "Volodymyr Pienskoi" (fromGregorian 2001 6 28),
                       Meeting (fromGregorian 2023 3 30) (TimeOfDay 10 25 0) (TimeOfDay 12 0 0) "Google Meet" "Haskell" True,
                       Meeting (fromGregorian 2023 3 30) (TimeOfDay 12 20 0) (TimeOfDay 13 55 0) "Zoom" "Databases" False,
                       Meeting (fromGregorian 2023 1 18) (TimeOfDay 9 0 0) (TimeOfDay 18 0 0) "Office" "Work" True,
                       Meeting (fromGregorian 2023 4 5) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) "KPI" "Exam" False 
                     ]

    print $ searchByName "Volodymyr Pienskoi" myNotebook

    print $ searchByNamePart "Volodymyr" myNotebook

    print $ searchByPhoneNumber "+380501234567" myNotebook

    print $ searchMeetingsByDay 30 3 2023 myNotebook

    print $ searchMeetingsByDayRange (1, 3, 2023) (2, 5, 2023) myNotebook

    print $ getHeldMeetings myNotebook

    print $ getMeetingsStatistics myNotebook

    print $ combineContactsByName "Volodymyr Pienskoi" myNotebook

