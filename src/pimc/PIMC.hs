module PIMC (testParams, runMC) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
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

runMC :: (RandomGen g) => MCParams -> Int -> Rand g Path
runMC params n = runReaderST (runMC' init n) params
  where
   init = V.replicate (pLength params) (pInit params)

runMC' :: (RandomGen g) => Path -> Int -> RandT g (ReaderT MCParams (ST s)) Path
runMC' path n = do
  params <- lift $ ask
  mpath <- lift $ lift $ V.thaw path
  replicateM_ n $ sweep mpath
  lift $ lift $ V.freeze mpath

sweep :: (RandomGen g) => MPath s -> RandT g (ReaderT MCParams (ST s)) ()
sweep mpath = do
  params <- lift $ ask
  mapM_ (nudge mpath) [0..(pLength params - 1)]

nudge :: (RandomGen g) => MPath s -> Int -> RandT g (ReaderT MCParams (ST s)) ()
nudge mpath i = do
  val <- lift $ lift $ MV.read mpath i
  inc <- getRandom
  lift $ lift $ MV.write mpath i (val + inc)
