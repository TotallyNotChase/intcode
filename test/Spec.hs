import qualified IOTest
import qualified STTest

import Control.Monad (void)
import Control.Monad.ST (stToIO)

-- | Function to convert all the ST based tests to IO
testsST :: IO [()]
testsST = mapM stToIO [STTest.testDay2, STTest.testDay5]

main :: IO ()
main = IOTest.testDay2 >> void testsST
