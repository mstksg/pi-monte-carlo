import System.Random
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Reader
import PIMC

main :: IO ()
main = do
  let
    params = testParams 10 1.0 1.0
  seed <- getStdGen
  res <- evalRandIO $ runMC params 10000
  -- res <- return $ runIdentity $ evalRandT (runMC params 10000) seed
  -- res <- return $ runReader (evalRandT (runMC params 10000) seed) 0
  print res
  

