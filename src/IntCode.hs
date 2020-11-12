module IntCode
    (
        IntCode, constructArray,
        executeOp, parseOp,
        runMachine, getOutput
    ) where

import Control.Monad ( when )
import Data.Array.IO
    ( IOArray, newListArray, readArray, writeArray )

import Utils ( digits, padR )

-- | The IntCode machine - a mutable IO array
type IntCode = IOArray Int Int

-- | Construct an IntCode from a list of ints
constructArray :: [Int] -> IO (IOArray Int Int)
constructArray l = newListArray (0, length l - 1) l

-- | Get the output of the mutated IntCode machine - i.e the value at index 0
getOutput :: IOArray Int Int -> IO Int
getOutput = flip readArray 0

{-|
Mutate the IntCode machine by running
its instructions

This essentially runs the intcode program and, as
a result, mutates the IntCode passed

After this function succeeds - the IntCode passed
will be mutated accordingly
-}
runMachine :: IntCode -> Int -> Int -> IO ()
runMachine mutArr arrLen i =
    -- Make sure we are in bounds
    when (i < arrLen) $
        -- Read the instruction
        -- If it is 99, halt - otherwise execute op and continue
        readArray mutArr i >>=
            -- Just a flipped version of when to allow monadic composition
            flip when (
                    -- Execute the opcode and move to the instruction index returned
                    executeOp mutArr i >>= runMachine mutArr arrLen
                )
                -- Compare the readArray result to 99
                . (/=99)


{-|
Helper function to execute an operator
given its index

Reads the instruction from opIx
Parses it to get the actual opcode and operand modes
Then reads the operands according to opcode, respecting the operand modes
(unless the operand is for output index - in which case, it is always read directly)
Then the opcode is executed using the operands and output index (if any)
This mutates the array accordingly

The result returned is the next instruction index
-}
executeOp :: IntCode -> Int -> IO Int
executeOp mutArr opIx = do
    -- Read the instruction - which is a number with param modes and opcode grouped up
    opGrp <- readArray mutArr opIx
    -- Parse the op group into opcode and operand modes
    let (opcode, oprnd1Mode, oprnd2Mode) = parseOp opGrp
    -- Execute the opcode, using the operands and their respective modes
    case opcode of
        op
            | op `elem` [1, 2, 7, 8] -> do
            {-
            Common case for all opcodes that read 3 operands,
            where the third operand is the output index for the instruction
            to write to, and progress the instruction pointer by 4
            -}
                -- Read the operands according to modes (but force mode 1 on third operand since that's output index)
                [oprnd1, oprnd2, oprnd3] <- sequence [readFstOperand oprnd1Mode, readSndOperand oprnd2Mode, readThrdOperand 1]
                case op of
                    -- Add the operands and write to the array
                    1 -> writeArray mutArr oprnd3 (oprnd1 + oprnd2)
                    -- Multiply the operands and write to the array
                    2 -> writeArray mutArr oprnd3 (oprnd1 * oprnd2)
                    -- Write 1 to operand3 if oprnd1 is less than oprnd2, else 0
                    7 -> writeArray mutArr oprnd3 $ if oprnd1 < oprnd2 then 1 else 0
                    -- Write 1 to operand3 if oprnd1 is equal to oprnd2, else 0
                    8 -> writeArray mutArr oprnd3 $ if oprnd1 == oprnd2 then 1 else 0
                -- Return the next instruction index
                return $ opIx + 4
            | op `elem` [3, 4] -> do
            {-
            Common case for all opcodes that read 1 operand
            (i.e I/O opcodes) and progress the instruction pointer by 2
            -}
                -- Read the only operand (force mode 1 for opcode 3 - since oprnd1 is output index for opcode 3)
                oprnd1 <- readFstOperand $ if op == 3 then 1 else oprnd1Mode
                if op == 3
                then
                    -- Store input in the index given by oprnd1
                    readLn >>= writeArray mutArr oprnd1
                else
                    -- Print output as given by oprnd1
                    print oprnd1
                -- Return the next instruction index
                return $ opIx + 2
            | op `elem` [5, 6] -> do
            {-
            Common case for all opcodes that read 2 operands
            and either progress the instruction pointer by 3
            or return a completely new instruction index according
            to the operand
            -}
                -- Read the operands according to modes
                [oprnd1, oprnd2] <- sequence [readFstOperand oprnd1Mode, readSndOperand oprnd2Mode]
                if op == 5
                then
                    -- Return oprnd2 if oprn1 is non-zero - otherwise, do nothing and progress to the next instruction
                    return $ if oprnd1 /= 0 then oprnd2 else opIx + 3
                else
                    -- Return oprnd2 if oprn1 is zero - otherwise, do nothing and progress to the next instruction
                    return $ if oprnd1 == 0 then oprnd2 else opIx + 3
            | otherwise -> error "Fatal: Invalid opcode"
    where
        -- Read the first operand right after opIx - according to its mode
        readFstOperand :: Int -> IO Int
        readFstOperand mode = flip readOperand mode $ opIx + 1
        -- Read the second operand right after opIx - according to its mode
        readSndOperand :: Int -> IO Int
        readSndOperand mode = flip readOperand mode $ opIx + 2
        -- Read the third operand right after opIx - according to its mode
        readThrdOperand :: Int -> IO Int
        readThrdOperand mode = flip readOperand mode $ opIx + 3
        {-
        Generic version of the above functions
        Reads operand from given index - according to the mode
        -}
        readOperand :: Int -> Int -> IO Int
        readOperand ix mode = do
            case mode of
                -- Mode 0 means, the value at ix is the index of the actual operand
                0 -> readArray mutArr ix >>= readArray mutArr
                -- Mode 1 means, the value at ix is the actual operand
                1 -> readArray mutArr ix
                _ -> error "Fatal: Invalid operand mode"

{-|
Parse the op group to extract the opcode and parameter modes

For op group 1002, it'll return (0, 1, 0, 2)

This means, opcode == 2
            1st param mode == 0
            2nd param mode == 1
            3rd param mode == 0 (omitted due to being leading zero)
-}
parseOp :: Int -> (Int, Int, Int)
parseOp opGrp = digsTuple digsL
    where
        -- Extract the first 2 digits (actual opcode)
        opcode = opGrp `mod` 100
        -- The rest digits should be extracted one by one
        rest = fromIntegral opGrp `div` 100
        -- Extract all the remaining digits and add them after opcode
        -- Then pad the resulting list to a length of 3 on the right side
        digsL = padR 3 0 $ opcode : reverse (digits rest)
        {-
        The resulting digsL will be an element with 3 numbers
        The first one is the actual opcode
        The next 2 are the modes of operand 1 and 2 respectively

        Turn this into a tuple and return it
        -}
        digsTuple [x, y, z] = (x, y, z)
