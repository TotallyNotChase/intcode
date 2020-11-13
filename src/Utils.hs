module Utils
    ( digits
    , padR
    ) where

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
