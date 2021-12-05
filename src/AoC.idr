||| An Advent of Code helper that manages the fetching of input data and
||| running the solutions.
module AoC

import public AoC.Types
import AoC.Run
import AoC.Input

import Data.List
import Data.String

import System
import System.Clock

latestDay : List Day -> Maybe Day
latestDay [] = Nothing
latestDay (a::rest) = loop a rest
    where loop : Day -> List Day -> Maybe Day
          loop x [] = Just x
          loop x (y::rest) =
            if y.dayNumber > x.dayNumber then
                loop y rest
            else
                loop x rest

getDay : List Day -> (day : Integer) -> Maybe Day
getDay [] day = Nothing
getDay (a::rest) day =
    if a.dayNumber == day then
        Just a
    else
        getDay rest day


||| Run the selected day challenge and show the results of the execution.
|||
||| The day to run can be set by specifying `--day <number>` on the CLI.
||| If the day to run is not specified the day with the highest day number
||| will be run.
|||
||| @ year The year for which to download the input data
||| @ inputDir A path to a directory in which the input data will be saved
||| @ days The list of days that have been solved so far
export
run : (year : Integer) -> (inputDir : String) -> (days : List Day) -> IO ()
run year inputDir days = do

    let day = case !getArgs of
            [_, "--day", day] => Just (cast day)
            _ => Nothing

    Just day <- the (IO $ Maybe Day) $ case day of
                    Just day => pure $ getDay days day
                    Nothing => pure $ latestDay days
    | Nothing =>
        putStrLn "No day found to execute"
    
    Just input <- fetchInput year inputDir day.dayNumber
    | Nothing => do
        putStrLn "Unable to get input for day \{show day.dayNumber}"
        
    runDay day input
