module IntCode.ST
    ( IntMachine
    , constructMachine
    , addInput
    , addInputs
    , getOutputs
    , runIns
    , runMachine
    , readMem
    ) where

import Control.Monad (foldM)
import Control.Monad.ST (ST)
import Data.Foldable (Foldable(toList))
import Data.Array.ST
    ( STArray
    , newListArray
    , readArray
    , writeArray
    )
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import Data.Sequence ((|>))
import qualified Data.IntMap as Map
import qualified Data.Sequence as Seq

import Utils (parseOp)


-- | The IntCode program itself - a mutable array
type Program s = STArray s Int Int

{- | Memory outside of the intcode program - represents infinite memory using lazy map
All memory indices surpassing the length of the program itself are initialized to 0
This means - only the memory indices modified by the program should be kept track of
So, if the program tries to write to a index out of bound - store it in the map
Whenever, the program tries to read from an index out of bound - check if it exists in the map
If it does exist, return the value stored, otherwise - it's just 0 - the unmodified default
-}
type Memory = Map.IntMap Int

{- | The IntCode Machine - contains the actual program + relevant info about itself
This data structue itself is not mutable
A new version, with the necessary fields updated, is returned per recursion step
However, the intCode array itself is mutable - just to avoid extreme inefficiency
The oobMem Map is still immutable - mostly because this doesn't need access a whole lot

The sequence of inputs is provided by the user and is sent to the program sequentially, each time
opcode 3 is executed. If user provided [4, 0, 1, 3] as input, the first time opcode 3 is encountered, input is 4
next time, it is 0 and so on
Once input runs out, outputs from the program itself are used as input - in the order they were outputted
If that runs out as well, the program fails with an exception

The sequence of outputs is a record of all outputs, in order, outputted by the program - available to check
to the user after runMachine succeeds
-}
data IntMachine s = IntMachine
    { insPtr     :: {-# UNPACK #-} !Int  -- ^ The instruction pointer
    , relBasePtr :: {-# UNPACK #-} !Int  -- ^ The relative base pointer
    , programLen :: {-# UNPACK #-} !Int  -- ^ Length of the intcode program
    , intCode    :: !(Program s)         -- ^ Actual intcode program (puzzle input)
    , oobMem     :: !Memory              -- ^ Memory outside of the intcode program
    , inps       :: !(Seq.Seq Int)       -- ^ Sequence of inputs to feed to the program
    , outs       :: !(Seq.Seq Int)       -- ^ Sequence of outputs produced by the program
    }

{- | Construct an IntCode machine from a list of ints
as well as a list of inputs - the inputs are fed into the program
sequentially, everytime it asks for input

If inputSequence was [4, 5, 6, 7, 8] - At the first input, the program is given 4
at the next one, it is given 5, then 6 and so on
-}
constructMachine :: [Int] -> [Int] -> ST s (IntMachine s)
constructMachine l inputSequence = newListArray (0, lenArr - 1) l <&>
    \mutArr -> IntMachine
        { insPtr=0
        , relBasePtr=0
        , programLen=lenArr
        , intCode=mutArr
        , oobMem=Map.empty
        , inps=Seq.fromList inputSequence
        , outs=Seq.empty
        }
  where
    lenArr = length l

-- | Append an input to the inps sequence of the given machine
addInput :: IntMachine s -> Int -> ST s (IntMachine s)
addInput mach inp = pure mach { inps = inps mach |> inp }

-- | Append multiple inputs to the inps sequence of the given machine
addInputs :: IntMachine s -> [Int] -> ST s (IntMachine s)
addInputs = foldM addInput

-- | Get a list of outputs from the intmachine - in the order they were outputted
getOutputs :: IntMachine s -> ST s [Int]
getOutputs = pure . toList . outs

-- | Read value of memory index - considers both the intcode program itself and the infinite memory band
readMem :: IntMachine s -> Int -> ST s Int
readMem mach i = if i < programLen mach
    -- Index is within bounds of the intcode program - get the value from there
    then flip readArray i . intCode $ mach
    -- Index out of bounds, try to find the value in the oobMem map
    -- If that index is not found in the map - it means it is unmodified (i.e 0 by default)
    else pure $ fromMaybe 0 . Map.lookup i . oobMem $ mach

-- | Write value to memory index - considers both the intcode program itself and the infinite memory band
writeMem :: IntMachine s -> Int -> Int -> ST s (IntMachine s)
writeMem mach i e = if i < programLen mach
    -- Index within bounds of the program array - write to it
    then writeArray (intCode mach) i e >> pure mach
    -- Index out of bounds, insert the value in the oobMem map
    else pure mach { oobMem = Map.insert i e . oobMem $ mach }

{- | Read the instruction given by the instruction pointer
and parse it into a tuple containing the actual opcode and the operand modes

The tuple is structured like - (opcode, operand 1 mode, operand 2 mode, operand 3 mode)
-}
readIns :: IntMachine s -> ST s (Int, Int, Int, Int)
readIns mach = parseOp <$> readArray (intCode mach) (insPtr mach)

{- | Run the instruction currently pointed to by the
instruction pointer

Mutates the machine and progresses to the next instruction

Returns the mutated machine
-}
runIns :: IntMachine s -> ST s (Maybe (IntMachine s))
runIns mach =
    -- Read the instruction and parse out the opcode
    readIns mach >>= \ins@(opcode, _, _, _) -> if opcode /= 99
        -- Execute the opcode and return Just the modified machine
        then executeOp mach ins <&> Just
        -- Encountered 99 - return Nothing
        else pure Nothing

{- | Mutate the IntCode machine by running
its instructions

This essentially runs the intcode program and, as
a result, mutates the IntCode passed

After this function succeeds - the IntCode passed
will be mutated accordingly
-}
runMachine :: IntMachine s -> ST s (IntMachine s)
runMachine mach =
    -- Execute the current opcode and obtain a modified machine
    runIns mach >>=
        -- Continue to the next opcode unless newMach is Nothing (runIns encountered 99)
        maybe (pure mach) runMachine

{- | Mutate the IntCode machine by running
its instructions

This essentially runs the intcode program and, as
a result, mutates the IntCode passed

After this function succeeds - the IntCode passed
will be mutated accordingly
-}
executeOp :: IntMachine s -> (Int, Int, Int, Int) -> ST s (IntMachine s)
executeOp mach opGrp = do
    -- opGrp is a number with param modes and opcode grouped up
    -- Separate it into the actual opcode and operand modes
    let (opcode, oprnd1Mode, oprnd2Mode, oprnd3Mode) = opGrp
    -- Execute the opcode, using the operands and their respective modes
    case opcode of
        op
            | op `elem` [1, 2, 7, 8] -> do
            {- Common case for all opcodes that read 3 operands,
            where the third operand is the output index for the instruction
            to write to, and progress the instruction pointer by 4 -}
                -- Read the operands according to modes
                [oprnd1, oprnd2, oprnd3] <- sequence
                    [ readFstOperand oprnd1Mode
                    , readSndOperand oprnd2Mode
                    -- The third one should be interpeted as an output operand
                    , readOutOperand (3 + insPtr mach) oprnd3Mode
                    ]
                -- Accordingly modify the machine
                newMach <- case op of
                    -- Add the operands and write to the machine memory
                    1 -> writeMem mach oprnd3 (oprnd1 + oprnd2)
                    -- Multiply the operands and write to the machine memory
                    2 -> writeMem mach oprnd3 (oprnd1 * oprnd2)
                    -- Write 1 to operand3 if oprnd1 is less than oprnd2, else 0
                    7 -> writeMem mach oprnd3 $ if oprnd1 < oprnd2 then 1 else 0
                    -- Write 1 to operand3 if oprnd1 is equal to oprnd2, else 0
                    8 -> writeMem mach oprnd3 $ if oprnd1 == oprnd2 then 1 else 0
                    _ -> error "Well that wasn't supposed to happen"
                -- Return the new machine - after bumping up its instruction pointer
                pure newMach { insPtr = 4 + insPtr mach }
            | op `elem` [3, 4] -> do
            {- Common case for all opcodes that read 1 operand
            (i.e I/O opcodes) and progress the instruction pointer by 2 -}
                -- Read the only operand
                oprnd1 <- if opcode == 3
                    -- Read the operand as output index for opcode 3
                    then readOutOperand (1 + insPtr mach) oprnd1Mode
                    else readFstOperand oprnd1Mode
                -- Accordingly modify the machine
                newMach <-
                    if op == 3
                    then
                        -- Read input (leftmost element of inps sequence) and write it to oprnd1 index
                        writeMem mach oprnd1 (flip Seq.index 0 . inps $ mach)
                        -- Then return a new mach with the used input removed
                            <&> \m -> m { inps = Seq.drop 1 . inps $ m }
                    else
                        -- Store output into the outs sequence
                        -- Also add it to the inps sequence - for use by opcode3 incase user provided inps runs out
                        pure mach { outs = outs mach |> oprnd1, inps = inps mach |> oprnd1 }
                -- Return the new machine - after bumping up its instruction pointer
                pure newMach { insPtr = 2 + insPtr newMach }
            | op `elem` [5, 6] -> do
            {- Common case for all opcodes that read 2 operands
            and either progress the instruction pointer by 3
            or change it to a completely new instruction index
            according to the operand -}
                -- Read the operands according to modes
                [oprnd1, oprnd2] <- sequence [readFstOperand oprnd1Mode, readSndOperand oprnd2Mode]
                if op == 5
                then
                    -- Change insPtr to oprnd2 if oprn1 is non-zero - otherwise, change insPtr to the next instruction index
                    pure mach { insPtr = if oprnd1 /= 0 then oprnd2 else 3 + insPtr mach }
                else
                    -- Change insPtr to oprnd2 if oprn1 is zero - otherwise, change insPtr to the next instruction index
                    pure mach { insPtr = if oprnd1 == 0 then oprnd2 else 3 + insPtr mach }
            | op == 9 -> do
            {- Opcode 9 is for changing the relative base pointer
            Read the operand and add it to the relative base pointer
            progress the instruction pointer to the next instruction
            return the new machine with these updated values -}
                -- Read the only operand
                oprnd1 <- readFstOperand oprnd1Mode
                -- Add the operand to relBasePtr - also progress the insPtr
                pure mach { relBasePtr = oprnd1 + relBasePtr mach, insPtr = 2 + insPtr mach }
            | otherwise -> error "Fatal: Invalid opcode"
  where
    -- | Read the first operand right after opIx - according to its mode
    readFstOperand mode = flip readOperand mode . (+1) . insPtr $ mach
    -- | Read the second operand right after opIx - according to its mode
    readSndOperand mode = flip readOperand mode . (+2) . insPtr $ mach
    {- | Generic version of the above functions
    Reads operand from given index - according to the mode -}
    readOperand i mode = case mode of
            -- Mode 0 means, the value at i is the index of the actual operand
            0 -> readMem mach i >>= readMem mach
            -- Mode 1 means, the value at i is the actual operand
            1 -> readMem mach i
            -- Mode 2 means, the value at i + relBasePtr is the index of the actual operand
            2 -> readMem mach i >>= readMem mach . (+ relBasePtr mach)
            _ -> error "Fatal: Invalid operand mode"
    {- | Function to read the operand at index i as an output operand
    This means the operand to be read is an index where the instruction will write to 
    Mode 1 is unavailable for this -}
    readOutOperand i mode = case mode of
        -- Mode 0 means the output index is the value at i
        0 -> readMem mach i
        -- Mode 2 means the output index is the value at i + relative base pointer
        2 -> readMem mach i <&> (+ relBasePtr mach)
        _ -> error "Fatal: Invalid output operand mode"
