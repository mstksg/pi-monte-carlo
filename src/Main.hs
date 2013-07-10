import System.Random
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Reader
import PIMC

main :: IO ()
main = do
  let
    params = testParams 1000 1.0 1.0
  seed <- getStdGen
  res <- evalRandIO $ runMC params 1000
  print $ pathEnergy params res
  res2 <- evalRandIO $ runMCPath params 1000 res
  print $ pathEnergy params res2
  

