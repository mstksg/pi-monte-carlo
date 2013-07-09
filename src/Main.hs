import System.Random
import Control.Monad.Random
import PIMC

main = do
  let
    params = testParams 10 1.0 1.0
  (evalRandIO $ runMC params 10000) >>= print

