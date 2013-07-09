module PIMC (testParams, runMC) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Random
import Control.Monad.ST

import Metropolis
import Utils

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Path = V.Vector Double 
type MPath s = MV.STVector s Double

data QSystem = QSystem  { hbar  :: Double
                        , m     :: Double
                        , v     :: (Double -> Double)
                        , v'    :: (Double -> Double)
                        }

data MCParams = MCParams  { dt      :: Double
                          , delta   :: Double
                          , pInit   :: Double
                          , pLength :: Int
                          , qSystem :: QSystem
                          }

testParams :: Int -> Double -> Double -> MCParams
testParams n m omega = MCParams 0.15 1.0 0.0 n qsys
  where
    qsys     = QSystem 1.0 m harmo harmo'
    harmo x  = 0.5 * m * omega^2 * x^2
    harmo' x = m * omega^2 * x

runMC :: (RandomGen g) => MCParams -> Int -> RandT g Identity Path
runMC params n = liftST $ runMC' params init n
  where
   init = V.replicate (pLength params) (pInit params)

runMC' :: (RandomGen g) => MCParams -> Path -> Int -> RandT g (ST s) Path
runMC' params path n = do
  mpath <- lift $ V.thaw path
  replicateM_ n $ sweep params mpath
  lift $ V.freeze mpath
  

sweep :: (RandomGen g) => MCParams -> MPath s -> RandT g (ST s) ()
sweep params mpath = do
  mapM_ (nudge params mpath) [0..(pLength params - 1)]

nudge :: (RandomGen g) => MCParams -> MPath s -> Int -> RandT g (ST s) ()
nudge params mpath i = do
  val <- lift $ MV.read mpath i
  inc <- getRandom
  lift $ MV.write mpath i (val + inc)
