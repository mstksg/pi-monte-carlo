module PIMC (testSystem, runMC) where

import Control.Applicative
import Control.Monad
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
                          , pLength :: Int
                          , qSystem :: QSystem
                          }

data MCSystem = MCSystem  { mcParams  :: MCParams
                          , mcPath    :: Path
                          }

instance Show MCSystem where
  show (MCSystem params path) = show path

testPath :: Path
testPath = V.generate 10 (exp . (* 0.5) . fromIntegral)

testSystem :: Int -> Double -> Double -> MCSystem
testSystem n m omega = MCSystem params path
  where
    params   = MCParams 0.15 1.0 n qsys
    qsys     = QSystem 1.0 m harmo harmo'
    harmo x  = 0.5 * m * omega^2 * x^2
    harmo' x = m * omega^2 * x
    path     = V.replicate n 0.0

runMC :: (RandomGen g) => MCSystem -> Int -> Rand g MCSystem
runMC system n = MCSystem params <$> swept
  where
   params = mcParams system
   path   = mcPath system
   qsys   = qSystem params
   swept  = liftST $ runMC' params qsys path n

runMC' :: (RandomGen g) => MCParams -> QSystem -> Path -> Int -> RandT g (ST s) Path
runMC' params qsys path n = do
  mpath <- lift $ V.thaw path
  replicateM_ n $ sweep params qsys mpath
  lift $ V.freeze mpath
  

sweep :: (RandomGen g) => MCParams -> QSystem -> MPath s -> RandT g (ST s) ()
sweep params qsys mpath = do
  mapM_ (nudge params qsys mpath) [0..(pLength params - 1)]

nudge :: (RandomGen g) => MCParams -> QSystem -> MPath s -> Int -> RandT g (ST s) ()
nudge params qsys mpath i = do
  val <- lift $ MV.read mpath i
  inc <- getRandom
  lift $ MV.write mpath i (val + inc)
