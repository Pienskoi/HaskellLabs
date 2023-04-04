{-# LANGUAGE DuplicateRecordFields #-}

module Crud
    ( CRUD(..)
    ) where 

import Model
import Data.Maybe (listToMaybe)

class CRUD a where
    createNote :: a -> [a] -> [a] 
    readNote :: Int -> [a] -> Maybe a
    updateNote :: Int -> a -> [a] -> [a]
    deleteNote :: Int -> [a] -> [a]

instance CRUD Note where
    createNote note nb = nb ++ [updateId note]
        where
            updateId (Contact _ cn pn) = Contact newId cn pn
            updateId (Birthday _ cn d) = Birthday newId cn d
            updateId (Meeting _ d st et p t h) = Meeting newId d st et p t h
            updateId _ = error "Invalid note type"
            newId = if null nb then 1 else noteId (last nb) + 1
    
    readNote nid = listToMaybe . filter (\n -> noteId n == nid)

    updateNote nid newNote (note:nb)
        | noteId note == nid = newNote : nb
        | otherwise = note : updateNote nid newNote nb

    deleteNote nid = filter (\n -> noteId n /= nid)
