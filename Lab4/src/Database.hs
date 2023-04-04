module Database
    ( saveNotebook,
      loadNotebook,
      saveNotebookHtml,
      saveLog
    ) where

import System.IO
import Data.List.Split (splitOn)
import Model
import Utils
import qualified Data.ByteString.Char8 as BS (readFile, writeFile, unlines, unpack, pack, append)
import System.Directory (doesFileExist)

saveNotebook :: Notebook -> FilePath -> IO ()
saveNotebook notebook filePath = BS.writeFile filePath serializedNotebook
    where
        serializedNotebook = BS.unlines $ map BS.pack $ map serializeNote notebook

serializeNote :: Note -> String
serializeNote (Contact noteId name phoneNumbers) = "Contact," ++ show noteId ++ "," ++ name ++ "," ++ unwords phoneNumbers
serializeNote (Birthday noteId name date) = "Birthday," ++ show noteId ++ "," ++ name ++ "," ++ dateToString date
serializeNote (Meeting noteId date startTime endTime place topic isHeld) =
  "Meeting," ++ show noteId ++ "," ++ dateToString date ++ "," ++ timeToString startTime ++ "," ++ timeToString endTime ++ "," ++ place ++ "," ++ topic ++ "," ++ show isHeld

loadNotebook :: FilePath -> IO Notebook
loadNotebook filePath = do
    exists <- doesFileExist filePath
    if exists then do 
        contents <- BS.readFile filePath
        let notes = map deserializeNote $ lines $ BS.unpack contents
        return notes
    else return []  

deserializeNote :: String -> Note
deserializeNote line =
    case splitOn "," line of
        ("Contact":noteId:name:phoneNumbers) -> Contact (read noteId) name phoneNumbers
        ("Birthday":noteId:name:date:[]) -> Birthday (read noteId) name (stringToDate date)
        ("Meeting":noteId:date:startTime:endTime:place:topic:isHeldStr:[]) -> Meeting (read noteId) (stringToDate date) (stringToTime startTime) (stringToTime endTime) place topic (read isHeldStr)
        _ -> error "Invalid note format"

saveNotebookHtml :: Notebook -> FilePath -> IO ()
saveNotebookHtml notebook filePath = BS.writeFile filePath serializedNotebook
    where
        serializedNotebook = BS.unlines $ BS.pack "<ul>" : serializedNotes ++ [BS.pack "</ul>"]
        serializedNotes = map BS.pack $ map serializeNoteHtml notebook

serializeNoteHtml :: Note -> String
serializeNoteHtml (Contact noteId name phoneNumbers) = "<li>Contact," ++ show noteId ++ "," ++ name ++ "," ++ unwords phoneNumbers ++ "</li>"
serializeNoteHtml (Birthday noteId name date) = "<li>Birthday," ++ show noteId ++ "," ++ name ++ "," ++ dateToString date ++ "</li>"
serializeNoteHtml (Meeting noteId date startTime endTime place topic isHeld) =
  "<li>Meeting," ++ show noteId ++ "," ++ dateToString date ++ "," ++ timeToString startTime ++ "," ++ timeToString endTime ++ "," ++ place ++ "," ++ topic ++ "," ++ show isHeld ++ "</li>"

saveLog :: String -> FilePath -> IO ()
saveLog msg filePath = do
    now <- timestamp
    exists <- doesFileExist filePath
    if exists then do 
        contents <- BS.readFile filePath
        BS.writeFile filePath $ BS.append contents $ BS.pack $ "\n" ++ now ++ ": " ++ msg
    else BS.writeFile filePath $ BS.pack $ now ++ ": " ++ msg
    