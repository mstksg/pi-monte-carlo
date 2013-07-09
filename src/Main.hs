import System.Random
import Control.Monad.Identity
import Control.Monad.Random
import PIMC

main :: IO ()
main = do
  let
    params = testParams 10 1.0 1.0
  seed <- getStdGen
  res <- return $ runIdentity $ evalRandT (runMC params 10000) seed
  print res
  

