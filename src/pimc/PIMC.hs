module PIMC (testPath, nudge, tST) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.STRef
import Metropolis

type Path = V.Vector Double 
type MPath s = MV.STVector s Double

testPath :: Path
testPath = V.generate 10 (exp . (* 0.5) . fromIntegral)

tST :: Path -> Int -> Int -> Int -> Path
tST path i a b = runST $ do
  mpath <- V.thaw path
  tSThelp mpath i
  MV.swap mpath a b
  V.freeze mpath

tSThelp :: (PrimMonad m) => MPath (PrimState m) -> Int -> m ()
tSThelp mpath i = do
  val <- MV.read mpath i
  MV.write mpath i (val + 1)

nudge = ()

-- sweep :: MPath s -> MPath s
-- sweep path = runST $ do
--   return $ nudge path 2

-- nudge :: (PrimMonad m) => MPath (PrimState m) -> Int -> m ()
-- nudge path n = do

-- nudge :: Path -> Integer -> Path
-- nudge path n = runST $ do
--   init <- V.thaw path
--   MV.swap init 1 5
--   MV.write init 2 (fromIntegral n)
--   V.freeze init

