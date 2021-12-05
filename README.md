# aoc-idr

An [Advent of Code](https://adventofcode.com/) helper for [Idris 2](https://github.com/idris-lang/Idris2).

```idris
import AoC

import Data.List
import Data.String
import Data.Maybe

parse : String -> List Integer
parse = map cast . lines

solvePart1 : List Integer -> Integer
solvePart1 xs = fromMaybe 0 $ head' [a * b | a <- xs, b <- xs, a + b == 2020]

testInput : String
testInput = """
1721
979
366
299
675
1456
"""

day01 : Day
day01 = MkDay 1 [
        MkDayPart "Part 1" (pure . parse) (pure . solvePart1)
    ]
    testInput

days : List Day
days = [
    day01
]

main : IO ()
main = do
    run {year = 2020, inputDir = "input", days = days}
```

```
======
Day 1
======

Test input
  Part 1 = 514579

Part 1 = <redacted>
  Parse: 3.0326 μs
  Solve: 6.0033 μs
```

Features:

- automatic downloading of your personalised input data
- timing report of parser and solver execution

## Installation

Run `idris2 --install` in the directory of this project, then use `aoc-idr`
in the `depends` section of `.ipkg` files.

For the downloading of the input data `curl` needs to be installed and in the
`PATH` variable.

The credentials are stored in `.config/aoc-idr/credentials` and only works on
Linux for now. PRs to add support for other operating systems are more than welcome. :)

## Finding your `session` key

Open up the devtools of your browser, and then:

- Firefox: "Storage" tab, Cookies, and copy the "Value" field of the `session` cookie.
- Google Chrome / Chromium: "Application" tab, Cookies, and copy the "Value" field of the `session` cookie.


## License

This project is licensed under the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/).