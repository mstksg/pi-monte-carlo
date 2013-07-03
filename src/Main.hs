import System.Random
import PIMC

main = do
  let
    system = testSystem 10 1.0 1.0
  putStrLn $ show $ runMC system 10000

