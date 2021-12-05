-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.


||| This module deals with running the code for a day of the challenge.
||| Apart from just running the code it also measures the execution time
||| of the parsers and solvers and displays the results in a nice way.
module AoC.Run

import Data.String
import Data.List
import System.Clock

import AoC.Types

measureExecTime : (f : IO a) -> IO (Clock Duration, a)
measureExecTime f = do
    before <- clockTime Monotonic
    res <- f
    after <- clockTime Monotonic
    let duration = timeDifference after before
    pure (duration, res)


humanTime : Clock Duration -> String
humanTime clock =
    let divMod : Integer -> Integer -> (Integer, Integer)
        divMod x num = (x `div` num, x `mod` num)

        pad : (width : Nat) -> (removeTrailingZeros : Bool) -> Integer -> String
        pad width removeTrailingZeros num =
            let raw = show num
                padded = padLeft width '0' raw
            in
            if removeTrailingZeros then
                pack . reverse . dropWhile (== '0') . reverse $ unpack padded
            else
                padded
    in

    let secsFull = seconds clock
        nanosFull = nanoseconds clock

        (mins, secs) = secsFull `divMod` 60

        microNS = 1000
        milliNS = 1000 * microNS

        (millis, mircosFull) = nanosFull `divMod` milliNS
        (micros, nanos) = mircosFull `divMod` microNS
    in
    
    if mins > 0 then
        "\{show mins}:\{pad 2 False secs} m"
    else if secs > 0 then
        "\{show secs}.\{pad 4 True millis} s"
    else if millis > 0 then
        "\{show millis}.\{pad 4 True micros} ms"
    else if micros > 0 then
        "\{show micros}.\{pad 4 True nanos} μs"
    else
        "\{show nanos} ns"


runPart : DayPart -> (input : String) -> IO ()
runPart part input = do
    (parseDuration, parsedInput) <- measureExecTime (part.parser input)
    (solveDuration, result) <- measureExecTime (part.solver parsedInput)
    let showInstance = part.showInstance
    putStrLn $ "\{part.name} = \{show result}"
    putStrLn $ "  Parse: " ++ humanTime parseDuration
    putStrLn $ "  Solve: " ++ humanTime solveDuration
    putStrLn ""


export
runDay : Day -> (input : String) -> IO ()
runDay day input = do
    putStrLn "======"
    putStrLn $ "Day " ++ show day.dayNumber
    putStrLn "======\n"

    putStrLn "Test input"
    for_ day.parts $ \part => do
        parsedInput <- part.parser day.testInput
        result <- part.solver parsedInput
        let showInstance = part.showInstance
        putStrLn $ "  \{part.name} = \{show result}"
    
    putStrLn ""

    for_ day.parts $ \part => do
        runPart part input