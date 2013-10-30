module PIMC (testParams, runMCSim, pathEnergy) where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.Random
import Control.Monad.ST
import System.IO

import Utils

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Path = V.Vector Double 
type MPath s = MV.STVector s Double

data QSystem = QSystem  { hbar  :: Double
                        , mass  :: Double
                        , pot   :: Double -> Double
                        , pot'  :: Double -> Double
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
    harmo x  = 0.5 * m * omega^(2 :: Int) * x^(2 :: Int)
    harmo' x = m * omega^(2 :: Int) * x

runMCSim :: MCParams -> Int -> Int -> IO Path
runMCSim params chunks chunk = 
  concatM (replicate chunks $ runMCChunk params chunk) (initPath params)

runMCChunk :: MCParams -> Int -> Path -> IO Path
runMCChunk params chunk path = do
  res <- evalRandIO $ runMC params chunk path
  print $ pathEnergy params res
  hFlush stdout
  return res

initPath :: MCParams -> Path
initPath params = V.replicate (pLength params) (pInit params)

runMC :: (RandomGen g) => MCParams -> Int -> Path -> Rand g Path
runMC params n path = runReaderST (runMC' path n) params

runMC' :: (RandomGen g) => Path -> Int -> RandT g (ReaderT MCParams (ST s)) Path
runMC' path n = do
  mpath <- lift2 $ V.thaw path
  replicateM_ n $ sweep mpath
  lift2 $ V.freeze mpath

sweep :: (RandomGen g) => MPath s -> RandT g (ReaderT MCParams (ST s)) ()
sweep mpath = do
  params <- lift ask
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
  if p1 > p0
    then
      lift2 $ MV.write mpath i (x + dx)
    else do
      let thresh = exp (p1 - p0)
      r <- getRandom
      when (r < thresh) $
        lift2 (MV.write mpath i (x+dx))

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
  return $ dt params / hbar system * (kEleft + kEright + localV)

kE :: Double -> Double -> ReaderT MCParams (ST s) Double
kE x0 x1 = do
  params <- ask
  let m   = mass $ qSystem params
      dx  = x1 - x0
      vel = dx / dt params
  return $ 0.5 * m * vel^(2 :: Int)

pathEnergy :: MCParams -> Path -> Double
pathEnergy params path = V.sum (V.map en path) / fromIntegral (pLength params)
  where
    v = pot $ qSystem params
    v' = pot' $ qSystem params
    en x = v x + 0.5 * v' x * x

-- energy :: MPath s -> ReaderT MCParams (ST s) Double
-- energy mpath = do
--   return $ MV.map id mpath
--   -- foldM f 0 mpath
