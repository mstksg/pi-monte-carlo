import System.Random
import PIMC

main = do
  let test = testPath
  -- putStrLn $ show $ nudge testPath 5
  -- putStrLn $ show $ testPath
  putStrLn $ show $ tST testPath 2 1 4
  return ()

