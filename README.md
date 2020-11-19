# intcode
Interpreter for the aoc2019 intcode esolang, written in haskell.

Just trying to learn """pure mutability""" in practice.

# Features
* Interprets intcode
* Absolutely nothing else

# Usage
The `IntCode` module exports a canonical IntCode interpreter. That is, it uses actual stdin and stdout for I/O - as instructed. This is how it works-

That's all fine and dandy, except in later days - you're asked to use that I/O with mass amount of inputs. Which means, you need to automate it - interactively automating stdio is a bit of a pain so there's also `IntCode.ST`

`IntCode.ST` exports functions with the same name as `IntCode` - except there's no stdio here - the inputs should be provided as a list, to `constructMachine`. The outputs will be present in the `outs` property of the `IntMachine` (accesible using the `outs` function)

# API - Module `IntCode`
## `IntMachine` - The Intcode machine
```hs
type Program = IOArray Int Int
type Memory = Map.IntMap Int

data IntMachine = IntMachine
    { insPtr     :: {-# UNPACK #-} !Int  -- ^ The instruction pointer
    , relBasePtr :: {-# UNPACK #-} !Int  -- ^ The relative base pointer
    , programLen :: {-# UNPACK #-} !Int  -- ^ Length of the intcode program
    , intCode    :: !Program             -- ^ Actual intcode program (puzzle input)
    , oobMem     :: !Memory              -- ^ Memory outside of the intcode program
    }
```

## `constructMachine :: [Int] -> IO IntMachine`
Takes a list of integers (the intcode puzzle input) and returns an intcode machine

```hs
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99]
```
## `runMachine :: IntMachine -> IO IntMachine`
Takes the intcode machine constructed from `constructMachine` and runs the program - essentially mutating the machine and returning it
```hs
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] >>= runMachine
```

## `getOutput :: IntMachine -> IO Int`
Takes the intcode machine and returns the value at memory index 0
```hs
-- This returns 30
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] >>= runMachine >>= getOutput
-- This returns 1
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] >>= getOutput
```
## `readMem :: IntMachine -> Int -> IO Int`
A general version of `getOutput` - return the value of the given index from the machine's memory

Canonically, the intcode machine employs a von neumann architecture - which means infinite memory indices. So you can access any memory index an `Int` can represent.
```hs
-- This returns 30
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] >>= runMachine >>= flip readMem 0
-- This returns 1
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] >>= flip readMem 0
-- This returns 99
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] >>= flip readMem 4
-- This returns 0 (untouched memory cell in the infinite band)
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] >>= flip readMem 42
-- This returns 0 (untouched memory cell in the infinite band)
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] >>= runMachine >>= flip readMem 42
```

# API - Module `IntCode.ST`
## `IntMachine` - The Intcode machine
```hs
type Program = STArray s Int Int
type Memory = Map.IntMap Int

data IntMachine s = IntMachine
    { insPtr     :: {-# UNPACK #-} !Int  -- ^ The instruction pointer
    , relBasePtr :: {-# UNPACK #-} !Int  -- ^ The relative base pointer
    , programLen :: {-# UNPACK #-} !Int  -- ^ Length of the intcode program
    , intCode    :: !(Program s)         -- ^ Actual intcode program (puzzle input)
    , oobMem     :: !Memory              -- ^ Memory outside of the intcode program
    , inps       :: !(Seq.Seq Int)       -- ^ Sequence of inputs to feed to the program
    , outs       :: !(Seq.Seq Int)       -- ^ Sequence of outputs produced by the program
    }
```

## `constructMachine :: [Int] -> [Int] -> IO IntMachine`
Takes a list of integers (the intcode puzzle input) and a list of inputs - returns an intcode machine

The list of inputs should be sequential. That is, if the list was `[4, 5, 6, 7, 8]` - At the first input (opcode 3), the program is given 4
at the next one, it is given 5, then 6 and so on

Once all inputs from this list runs out and the program encounters *another* input instruction (opcode 3) - it starts using *the outputs* as the input

Consider `4, 0, 3, 1, 99` - where input list is `[]` (no input provided by user)
* Encounter `4, 0` - output the value at index `0` - i.e `4`
  This value is appended (right side) to the output sequence (`outs`) **and** the `inps` sequence
* Encounter `3, 0` - read input from `inps` and store it at index `0`
  The leftmost value from `inps` is popped off from the sequence and used for the input

If at any point, `inps` is an empty sequence during the input instruction (opcode 3) - the machine halts with an exception

This behavior is helpful for day 7 part 2. Where you need to input 2 ints at first, and all the next inputs will simply be the previous output. All you have to do is give the 2 input list as the second argument to `constructMachine`, and run the machine. Once the 2 inputs are exhausted, it'll start pulling from the outputs and keep going until it halts.

```hs
-- Make an intcode machine that takes 1 as its input in its first encounter with opcode 3 (input op)
constructMachine [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [1]
```
## `runMachine :: IntMachine -> IO IntMachine`
Takes the intcode machine constructed from `constructMachine` and runs the program - essentially mutating the machine and returning it
```hs
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] [] >>= runMachine
```

## `getOutput :: IntMachine -> IO Int`
Takes the intcode machine and returns the value at memory index 0
```hs
-- This returns 30
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] [] >>= runMachine >>= getOutput
-- This returns 1
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] [] >>= getOutput
```
## `readMem :: IntMachine -> Int -> IO Int`
A general version of `getOutput` - return the value of the given index from the machine's memory

Canonically, the intcode machine employs a von neumann architecture - which means infinite memory indices. So you can access any memory index an `Int` can represent.
```hs
-- This returns 30
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] [] >>= runMachine >>= flip readMem 0
-- This returns 1
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] [] >>= flip readMem 0
-- This returns 99
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] [] >>= flip readMem 4
-- This returns 0 (untouched memory cell in the infinite band)
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] [] >>= flip readMem 42
-- This returns 0 (untouched memory cell in the infinite band)
constructMachine [1, 1, 1, 4, 99, 5, 6, 0, 99] [] >>= runMachine >>= flip readMem 42
```

# Solutions to aoc2019 problems

## Day 2 solution
```hs
import IntCode (constructMachine, runMachine, getOutput)

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
