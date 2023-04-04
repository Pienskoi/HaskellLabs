module Main (main) where

import System.Environment
import System.Exit (exitSuccess)
import Data.Maybe (maybeToList)
import Console
import Database
import Model
import Crud
import Utils
import Lab3

outputNotebook :: Notebook -> String -> Options -> IO ()
outputNotebook nb db opts = do
    case logname opts of
        Just logFile -> saveLog "Saving result ..." logFile
        Nothing -> return ()
    if db == "" then return () else saveNotebook nb db
    case html opts of
        Just htmlFile -> saveNotebookHtml nb htmlFile
        Nothing -> return ()
    if silent opts then return () else print nb
    case logname opts of
        Just logFile -> saveLog "Result saved" logFile
        Nothing -> return ()

main :: IO ()
main = do
    dbName:args <- getArgs
    let opts = parseOptions args defaultOptions

    if help opts then do
        print "lab4-exe [DBNAME] [-c|--command COMMANDNAME [ARGS]]"
        print "                  [-l|--log LOGNAME]"
        print "                  [-s|--silent]"
        print "                  [--html FILENAME]"
        print "                  [-h|--help]"
        exitSuccess
    else do return ()

    notebook <- loadNotebook dbName

    case command opts of
        Command "createNote" ["Contact", cn, pn] -> let updatedNotebook = createNote (Contact 0 cn [pn]) notebook in outputNotebook updatedNotebook dbName opts
        Command "createNote" ["Birthday", cn, d] -> let updatedNotebook = createNote (Birthday 0 cn (stringToDate d)) notebook in outputNotebook updatedNotebook dbName opts
        Command "createNote" ["Meeting", d, st, et, p, t, h] -> let updatedNotebook = createNote (Meeting 0 (stringToDate d) (stringToTime st) (stringToTime et) p t (read h)) notebook in outputNotebook updatedNotebook dbName opts
        Command "readNote" [nid] -> let note = readNote (read nid) notebook in outputNotebook (maybeToList note) "" opts
        Command "updateNote" ["Contact", nid, cn, pn] -> let updatedNotebook = updateNote (read nid) (Contact 0 cn [pn]) notebook in outputNotebook updatedNotebook dbName opts
        Command "updateNote" ["Birthday", nid, cn, d] -> let updatedNotebook = updateNote (read nid) (Birthday 0 cn (stringToDate d)) notebook in outputNotebook updatedNotebook dbName opts
        Command "updateNote" ["Meeting", nid, d, st, et, p, t, h] -> let updatedNotebook = updateNote (read nid) (Meeting 0 (stringToDate d) (stringToTime st) (stringToTime et) p t (read h)) notebook in outputNotebook updatedNotebook dbName opts
        Command "deleteNote" [nid] -> let updatedNotebook = deleteNote (read nid) notebook in outputNotebook updatedNotebook dbName opts
        Command "searchByName" [cn] -> let searchedNotebook = searchByName cn notebook in outputNotebook searchedNotebook "" opts
        Command "searchByNamePart" [p] -> let searchedNotebook = searchByNamePart p notebook in outputNotebook searchedNotebook "" opts
        Command "getHeldMeetings" [] -> let searchedNotebook = getHeldMeetings notebook in outputNotebook searchedNotebook "" opts

