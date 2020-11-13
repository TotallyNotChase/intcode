import Control.Monad ( when )

import IntCode ( constructMachine, runMachine, getOutput )
import TestUtils ( splitStrAt )

{-|
Construct an IntCode machine from given comma
separated int values

Run the machine and return the final output (element at index 0)
-}
runTest :: String -> IO Int
runTest inp = do
    -- Convert the string into a list of ints
    let inpl = map read . splitStrAt "," $ inp
    -- Construct the intcode machine
    constructMachine inpl
    -- Run the machine
    >>= runMachine
    -- Get the final output (result at index 0)
    >>= getOutput

-- | Test the intcode on the problem from day2 of aoc2019
testDay2 :: IO ()
testDay2 = do
    -- Run the test with part 1 and part 2 inputs
    res1 <- runTest inpP1
    res2 <- runTest inpP2
    when (res1 /= outP1 || res2 /= outP2) $ error $ "testDay2 failed: Expected " ++ show (outP1, outP2) ++ " Got " ++ show (res1, res2)
    where
        -- The puzzle inputs I was given
        inpP1 = "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,10,23,2,13,23,27,1,5,27,31,2,6,31,35,1,6,35,39,2,39,9,43,1,5,43,47,1,13,47,51,1,10,51,55,2,55,10,59,2,10,59,63,1,9,63,67,2,67,13,71,1,71,6,75,2,6,75,79,1,5,79,83,2,83,9,87,1,6,87,91,2,91,6,95,1,95,6,99,2,99,13,103,1,6,103,107,1,2,107,111,1,111,9,0,99,2,14,0,0"
        inpP2 = "1,66,35,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,10,23,2,13,23,27,1,5,27,31,2,6,31,35,1,6,35,39,2,39,9,43,1,5,43,47,1,13,47,51,1,10,51,55,2,55,10,59,2,10,59,63,1,9,63,67,2,67,13,71,1,71,6,75,2,6,75,79,1,5,79,83,2,83,9,87,1,6,87,91,2,91,6,95,1,95,6,99,2,99,13,103,1,6,103,107,1,2,107,111,1,111,9,0,99,2,14,0,0"
        -- The expected answers for my puzzle inputs
        outP1 = 4138687
        outP2 = 19690720

main :: IO ()
main = testDay2
