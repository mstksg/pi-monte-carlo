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
  runMCSim params 100 1000
  return ()
