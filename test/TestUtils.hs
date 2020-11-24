module TestUtils
    ( splitStrAt
    , scanlM
    , runAmpsTillHalt
    ) where

import Control.Monad.ST (ST)
import Data.Functor ((<&>))

import IntCode.ST
    ( addInput
    , getOutputs
    , readIns
    , runIns
    , viewInputs
    , IntMachine
    )

-- | Split a string by a list of delimiters
splitStrAt :: String -> String -> [String]
splitStrAt _ "" = []
splitStrAt delimiters text@(_:_) = this : splitStrAt delimiters rest
    where
        -- Break at any delimiter from delimiters and store the rest
        (this, rest) =
            case break (`elem` delimiters) text of
                (gr, _:rest') -> (gr, rest')
                (gr, "") -> (gr, "")

-- | scanl but for monadic actions
scanlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanlM _ q [] = pure [q]
scanlM f q (x:xs) = f q x >>= flip (scanlM f) xs <&> (q:)

{- Functions for use in Day 7 -}
{- | Run each instruction one by one and stop if
an input instruction is encountered with no input
left to use or if the machine has halted

The mutated machine is returned
-}
runUntilInp :: IntMachine s -> ST s (IntMachine s)
runUntilInp mach = do
    -- Read the current instruction and parse out the opcode
    (opcode, _, _, _) <- readIns mach
    -- Check the input sequence of the machine
    inps <- viewInputs mach
    case (opcode, inps) of
        -- Opcode is 3 (input instruction) but no inputs left to use, return
        (3, []) -> pure mach
        -- Run the current instruction and continue (unless machine has halted)
        -- If machine has halted, return the machine
        _ -> runIns mach >>= maybe (pure mach) runUntilInp

{- | Run the list of machines (each machine representing an amplifier)
`m` is the starting machine, which's last output is fed into the first amp
the rest of the amps use the previous amp's output

Which means this is essentially a fold, but all the intermediate states should be remembered
to be returned as a new list of machines - that's a scanl
Discard the first element though, that's just the starting element passed
-}
runAmps :: IntMachine s -> [IntMachine s] -> ST s [IntMachine s]
runAmps m ms = tail <$> scanlM (\acc x -> getLastOutput acc >>= addInput x >>= runUntilInp) m ms
    where
        {- | Get the final output produced by given machine
        If no output has been provided, use 0 (this will come into play for the very first amp run) -}
        getLastOutput mach = getOutputs mach <&> \outs -> if null outs then 0 else last outs

{- | Run all amps till one of them halts
All of the machines in each amp is modified accordingly

In the end, the final list of modified amps are returned

The final output should be the greatest output amongst all of these
machines
-}
runAmpsTillHalt :: IntMachine s -> [IntMachine s] -> ST s [IntMachine s]
runAmpsTillHalt startMach machs =
    -- Run all the amplifiers once
    runAmps startMach machs
    -- Read the current instruction of all the modified machines in the amplifiers
    >>= \newMachs -> mapM readIns newMachs
        >>= (\inss ->
            -- Check if any of the machines has its instruction pointer set to 99 (aka has halted) 
            if any (\(op, _, _, _) -> op == 99) inss
            {- If none of them has halted, runAmpsTillHalt again, passing in the 
            final amplifier machine as starting mach and the modified set of machines -}
            then pure newMachs
            -- Otherwise, return the set of machines at hand - signifying one of them has halted and the process is over
            else runAmpsTillHalt (last newMachs) newMachs
        )
