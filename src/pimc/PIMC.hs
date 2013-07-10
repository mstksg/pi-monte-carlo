module PIMC (testParams, runMC, runMCPath, pathEnergy) where

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
runMC params n = runMCPath params n init
  where
   init = V.replicate (pLength params) (pInit params)

runMCPath :: (RandomGen g) => MCParams -> Int -> Path -> Rand g Path
runMCPath params n path = runReaderST (runMC' path n) params

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
  range <- delta <$> lift ask
  x <- lift2 $ MV.read mpath i
  dx <- getRandomR (-range, range)
  s0 <- lift $ localAction mpath i 0.0
  s1 <- lift $ localAction mpath i dx
  let p0 = (-1) * s0
      p1 = (-1) * s1
  if (p1 > p0)
    then
      lift2 $ MV.write mpath i (x + dx)
    else do
      let thresh = exp (p1 - p0)
      r <- getRandom
      when (r < thresh) $
        (lift2 $ MV.write mpath i (x+dx))

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

kE :: Double -> Double -> ReaderT MCParams (ST s) Double
kE x0 x1 = do
  params <- ask
  let m   = mass $ qSystem params
      dx  = x1 - x0
      vel = dx / (dt params)
  return $ 0.5 * m * vel^2

pathEnergy :: MCParams -> Path -> Double
pathEnergy params path = (V.sum $ V.map en path) / (fromIntegral $ pLength params)
  where
    v = pot $ qSystem params
    v' = pot' $ qSystem params
    en x = v x + 0.5 * (v' x) * x

-- energy :: MPath s -> ReaderT MCParams (ST s) Double
-- energy mpath = do
--   return $ MV.map id mpath
--   -- foldM f 0 mpath
