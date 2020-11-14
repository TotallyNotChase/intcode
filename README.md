# intcode
Interpreter for the aoc2019 intcode esolang, written in haskell.

Just trying to learn """pure mutability""" in practice.

# Features
* Interprets intcode
* Absolutely nothing else

# Usage
## Day 2 solution
```hs
import IntCode ( constructMachine, runMachine, getOutput )

solution :: String -> IO Int
solution inpstr =
    -- Construct the mutable IntCode array 
    constructMachine inpl
    -- Run the machine
    >>= runMachine
    -- Get the final output (result at index 0)
    >>= getOutput
  where
    -- Convert the string into a list of ints
    inpl = map read . splitStrAt "," $ inpstr   
```
Input is the puzzle input, returned value is the answer

## Day 5 solution
```hs
solution :: [Char] -> IO ()
solution inpstr = void $
    -- Construct the mutable IntCode array 
    constructMachine inpl
    -- Run the machine
    >>= runMachine
  where
    -- Convert the string into a list of ints
    inpl = map read . splitStrAt "," $ inpstr
```

Input is the puzzle input, the user should enter 1 to stdin for part 1 and 5 for part 2 - the result will be printed in stdout

## Day 9 solution
```hs
solution :: [Char] -> IO ()
solution inpstr = void $
    -- Construct the mutable IntCode array 
    constructMachine inpl
    -- Run the machine
    >>= runMachine
  where
    -- Convert the string into a list of ints
    inpl = map read . splitStrAt "," $ inpstr
```
Same as day 5 - this time the part 1 input is 1 and part 2 input is 2 - the result will be printed in stdout
