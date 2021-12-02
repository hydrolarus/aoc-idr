-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.


||| This module deals with downloading, reading and saving the input data as
||| as well as storing the AoC credentials
module AoC.Input

import System
import System.File
import System.Directory


downloadInput : (year, day : Integer) -> (session : String) -> IO (Maybe String)
downloadInput year day session = do
    let url = "\"https://adventofcode.com/\{show year}/day/\{show day}/input\""
    let command = "curl --cookie \"session=\{session}\" -s \{url}"
    (res, code) <- run command
    
    if code /= 0
        then pure $ Nothing
        else pure $ Just res


tryGetInputFromFile : (day : Integer) -> (inputDir : String) -> IO (Maybe String)
tryGetInputFromFile day inputDir = do
    let path = "\{inputDir}/input_day\{show day}"
    Right content <- readFile path
        | Left FileNotFound => do
            pure Nothing
        | Left err => do
            putStrLn "Unable to read file for day \{show day}: \{show err}"
            pure Nothing
    
    pure $ Just content

saveInputToFile : (day : Integer) -> (inputDir, input : String) -> IO ()
saveInputToFile day inputDir input = do
    let path = "\{inputDir}/input_day\{show day}"

    Right () <- writeFile path input
        | Left err =>
            putStrLn "Unable to store input for day \{show day} in file: \{show err}"
    pure ()


tryReadSession : IO (Maybe String)
tryReadSession = do
    -- TODO this only works on Linux, make more robust
    Just home <- getEnv "HOME"
        | Nothing => pure Nothing
 
    let dirPath = "\{home}/.config/idris-aoc/"
    let path = "\{dirPath}/credentials"

    Right content <- readFile path
        | Left err => do
            putStrLn "Unable to read session file: \{show err}"
            pure Nothing
    pure $ Just content


saveSession : (session : String) -> IO ()
saveSession session = do
    -- TODO this only works on Linux, make more robust
    Just home <- getEnv "HOME"
        | Nothing => pure ()
 
    let dirPath = "\{home}/.config/idris-aoc/"
    let path = "\{dirPath}/credentials"

    Right () <- createDir dirPath
        | Left err => do
            putStrLn "Unable to create directory to store credentials: \{show err}"
            pure ()
    
    Right () <- writeFile path session
        | Left err =>
            putStrLn "Unable to create session file: \{show err}"
    pure ()

session : IO String
session = do
    Nothing <- tryReadSession
        | Just session => pure session
    
    putStr "Enter session key: "
    fflush stdout

    session <- getLine
    saveSession session

    pure session

export
fetchInput : (year : Integer) -> (inputDir : String) -> (day : Integer) -> IO (Maybe String)
fetchInput year inputDir day = do
    Nothing <- tryGetInputFromFile day inputDir
        | Just input => pure $ Just input
    
    ses <- session

    Just input <- downloadInput year day ses
        | Nothing => do
            putStrLn "Unable to download input file"
            pure Nothing

    saveInputToFile day inputDir input

    pure $ Just input

