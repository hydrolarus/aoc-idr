||| An Advent of Code helper that manages the fetching of input data and
||| running the solutions.
module AoC

import public AoC.Types
import AoC.Run
import AoC.Input

import Data.List
import Data.String

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

||| Run the latest day challenge and show the results of the execution.
|||
||| @ year The year for which to download the input data
||| @ inputDir A path to a directory in which the input data will be saved
||| @ days The list of days that have been solved so far
export
run : (year : Integer) -> (inputDir : String) -> (days : List Day) -> IO ()
run year inputDir days = do

    Just day <- pure $ latestDay days
        | Nothing => do
            putStrLn "No days available to execute"
            pure ()
    
    Just input <- fetchInput year inputDir day.dayNumber
        | Nothing => do
            putStrLn "Unable to get input for day \{show day.dayNumber}"
        
    runDay day input
