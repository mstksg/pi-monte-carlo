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
                        , mass  :: Double
                        , pot   :: (Double -> Double)
                        , pot'  :: (Double -> Double)
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
  mpath <- lift2 $ V.thaw path
  replicateM_ n $ sweep mpath
  lift2 $ V.freeze mpath

sweep :: (RandomGen g) => MPath s -> RandT g (ReaderT MCParams (ST s)) ()
sweep mpath = do
  params <- lift $ ask
  mapM_ (nudge mpath) [0..(pLength params - 1)]

nudge :: (RandomGen g) => MPath s -> Int -> RandT g (ReaderT MCParams (ST s)) ()
nudge mpath i = do
  val <- lift2 $ MV.read mpath i
  inc <- getRandom
  ke <- lift $ kE 1.0 2.0
  -- lift2 $ MV.write mpath i (val + inc)
  lift2 $ MV.write mpath i (ke + inc)

localAction :: MPath s -> Int -> Double -> ReaderT MCParams (ST s) Double
localAction mpath i dx = do
  params <- ask
  let n      = pLength params
      system = qSystem params
      v      = pot system
  x <- lift $ MV.read mpath i
  xleft <- lift $ MV.read mpath (mod (i-1) n)
  xright <- lift $ MV.read mpath (mod (i+1) n)
  kEleft <- kE xleft (x+dx)
  kEright <- kE xright (x+dx)
  let localV  = v (x+dx)
  return $ (dt params) / (hbar system) * (kEleft + kEright + localV)

-- localAction :: MPath s -> Int -> ReaderT MCParams (ST s) (Double -> Double)
-- localAction mpath i = do
--   params <- ask
--   let n      = pLength params
--       system = qSystem params
--       v      = pot system
--   x <- lift $ MV.read mpath i
--   xleft <- lift $ MV.read mpath (mod (i-1) n)
--   xright <- lift $ MV.read mpath (mod (i+1) n)
--   let localKE dx = (kE xleft (x+dx)) + (kE xright (x+dx))
--       localV  dx = v (x+dx)
--   return $ (\dx -> (dt params) / (hbar system) * ((localKE dx) + (localV dx)))

kE :: Double -> Double -> ReaderT MCParams (ST s) Double
kE x0 x1 = do
  params <- ask
  let m   = mass $ qSystem params
      dx  = x1 - x0
      vel = dx / (dt params)
  return $ 0.5 * m * vel^2
