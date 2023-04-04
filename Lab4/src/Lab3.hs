{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lab3
    ( Search(..),
      Combine(..)
    ) where

import Data.Time.Calendar (DayOfMonth, MonthOfYear, Year, fromGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.List
import Model

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
    mempty = Contact 0 "" []
    mappend = (<>)

instance Semigroup Note where
    (Contact i1 n1 pn1) <> (Contact i2 n2 pn2)
        | n1 == n2 = Contact i1 n1 (pn1 ++ pn2)
        | i2 == 0 && n2 == "" && pn2 == [] = Contact i1 n1 pn1
        | otherwise = error "Can't combine Contacts with different names"
    note1 <> note2 = error $ "Can't combine " ++ show note1 ++ " with " ++ show note2

instance Search Notebook where
    searchByName cn = filter matchName
        where 
            matchName (Contact _ n _) = n == cn
            matchName (Birthday _ n _) = n == cn
            matchName _ = False

    searchByNamePart namePart = filter matchNamePart 
        where 
            matchNamePart (Contact _ n _) = isInfixOf namePart n
            matchNamePart (Birthday _ n _) = isInfixOf namePart n
            matchNamePart _ = False

    searchByPhoneNumber phoneNumber = filter matchPhoneNumber
        where 
            matchPhoneNumber (Contact _ _ pns) = phoneNumber `elem` pns
            matchPhoneNumber _ = False

    searchMeetingsByDay day month year = filter matchDate
        where
            matchDate (Meeting _ d _ _ _ _ _) = d == fromGregorian year month day
            matchDate _ = False

    searchMeetingsByDayRange (d1, m1, y1) (d2, m2, y2) = filter matchDayRange
        where
            matchDayRange (Meeting _ d _ _ _ _ _) = d >= fromGregorian y1 m1 d1 && d <= fromGregorian y2 m2 d2
            matchDayRange _ = False

    getHeldMeetings = filter isHeld'
        where
            isHeld' (Meeting _ _ _ _ _ _ h) = h
            isHeld' _ = False

    getMeetingsStatistics nb = map (\g -> (getMonth (head g), length g)) $ groupBy (\m1 m2 -> getMonth m1 == getMonth m2) $ filter isMeeting nb
            where
                getMonth (Meeting _ d _ _ _ _ _) = formatTime defaultTimeLocale "%B" d 
                isMeeting (Meeting _ _ _ _ _ _ _) = True
                isMeeting _ = False

instance Combine Note where
    combineContactsByName cn nb = mconcat $ filter matchName nb
        where
            matchName (Contact _ n _) = n == cn
            matchName _ = False

--     print $ searchByName "Volodymyr Pienskoi" myNotebook

--     print $ searchByNamePart "Volodymyr" myNotebook

--     print $ searchByPhoneNumber "+380501234567" myNotebook

--     print $ searchMeetingsByDay 30 3 2023 myNotebook

--     print $ searchMeetingsByDayRange (1, 3, 2023) (2, 5, 2023) myNotebook

--     print $ getHeldMeetings myNotebook

--     print $ getMeetingsStatistics myNotebook

--     print $ combineContactsByName "Volodymyr Pienskoi" myNotebook

