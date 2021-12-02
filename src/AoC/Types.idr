-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

||| This module contains the data descriptions used for specifying solutions
||| to the daily challenges.
module AoC.Types

public export
record DayPart where
    constructor MkDayPart
    {dataType : Type}
    {outputType : Type}
    {auto showInstance : Show outputType}
    name : String
    parser : String -> IO dataType
    solver : dataType -> IO outputType

public export
record Day where
    constructor MkDay
    dayNumber : Integer
    parts : List DayPart
