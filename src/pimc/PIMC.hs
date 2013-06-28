module PIMC (testPath, nudge) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Metropolis

type Path = V.Vector Double 
type MPath = MV.STVector Double

testPath :: Path
testPath = V.generate 10 (exp . (* 0.5) . fromIntegral)


nudge :: Path -> Integer -> Path
nudge path n = runST $ do
  init <- V.thaw path
  MV.swap init 1 5
  MV.write init 2 (fromIntegral n)
  V.freeze init

