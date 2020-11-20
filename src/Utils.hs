module Utils (parseOp) where

{-| Turn an integer number to a list of int digits -}
digits :: Integer -> [Int]
digits = reverse . innerF
    where
        -- Actual function that converts an integet to list of int digits
        -- the result is reversed and then returned
        innerF 0 = []
        innerF innerNum = fromInteger innerNum `mod` 10 : innerF (innerNum `div` 10)

-- | Right pad a list to supplied length, using supplied element
padR :: Int -> a -> [a] -> [a]
padR m e xs
    | length xs >= m = xs
    | otherwise = xs ++ replicate (m - length xs) e

{- | Parse the instruction to extract the opcode and parameter modes

For 1002, it'll return (2, 0, 1, 0)

This means, opcode == 2
            1st param mode == 0
            2nd param mode == 1
            3rd param mode == 0 (omitted due to being leading zero)
-}
parseOp :: Int -> (Int, Int, Int, Int)
parseOp opGrp = digsTuple digsL
  where
    -- Extract the first 2 digits (actual opcode)
    opcode = opGrp `mod` 100
    -- The rest digits should be extracted one by one
    rest = fromIntegral opGrp `div` 100
    -- Extract all the remaining digits and add them after opcode
    -- Then pad the resulting list to a length of 4 on the right side
    digsL = padR 4 0 $ opcode : reverse (digits rest)
    {- The resulting digsL will be an element with 4 numbers
    The first one is the actual opcode
    The next 3 are the modes of operand 1 and 2 respectively

    Turn this into a tuple and return it -}
    digsTuple [x, y, z, a] = (x, y, z, a)
