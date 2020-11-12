module TestUtils
    (
        splitStrAt
    ) where

{- Split a string by a list of delimiters -}
splitStrAt :: String -> String -> [String]
splitStrAt _ "" = []
splitStrAt delimiters text@(_:_) = this : splitStrAt delimiters rest
    where
        -- Break at any delimiter from delimiters and store the rest
        (this, rest) =
            case break (`elem` delimiters) text of
                (gr, _:rest') -> (gr, rest')
                (gr, "") -> (gr, "")
