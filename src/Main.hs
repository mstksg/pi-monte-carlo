import System.Random
import Control.Monad.Random
import PIMC

main = do
  let
    system = testSystem 10 1.0 1.0
  res <- evalRandIO $ runMC system 10000
  print res

