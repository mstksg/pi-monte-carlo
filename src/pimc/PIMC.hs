module PIMC (testSystem, runMC) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.STRef
import Metropolis

type Path = V.Vector Double 
type MPath s = MV.STVector s Double

data QSystem = QSystem  { hbar  :: Double
                        , m     :: Double
                        , v     :: (Double -> Double)
                        , v'    :: (Double -> Double)
                        }

data MCParams = MCParams  { dt      :: Double
                          , delta   :: Double
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
    params   = MCParams 0.15 1.0 qsys
    qsys     = QSystem 1.0 m harmo harmo'
    harmo x  = 0.5 * m * omega^2 * x^2
    harmo' x = m * omega^2 * x
    path     = V.replicate n 0.0

runMC :: MCSystem -> Int -> MCSystem
runMC system n =  MCSystem params swept
  where
   params = mcParams system
   path   = mcPath system
   qsys   = qSystem params
   swept  = runMC' params qsys path n
   -- swept  = runST $ do
   --    mpath <- V.thaw path
   --    sweep params qsys path
   --    V.freeze mpath

runMC' :: MCParams -> QSystem -> Path -> Int -> Path
runMC' params qsys path n = runST $ do
  mpath <- V.thaw path
  replicateM_ n $ do
    sweep params qsys mpath
  V.freeze mpath

sweep :: MCParams -> QSystem -> MPath s -> ST s ()
sweep params qsys mpath = do
  val <- MV.read mpath 1
  MV.write mpath 1 (val + 1)


-- tST :: Path -> Int -> Int -> Int -> Path
-- tST path i a b = runST $ do
--   mpath <- V.thaw path
--   tSThelp mpath i
--   MV.swap mpath a b
--   V.freeze mpath

-- tSThelp :: PrimMonad m => MPath (PrimState m) -> Int -> m ()
-- tSThelp mpath i = do
--   val <- MV.read mpath i
--   MV.write mpath i (val + 1)

-- nudge = ()

-- runMC :: PrimMonad m => MPath (PrimState m) -> Int -> m ()
-- runMC 

-- runMC :: MPath s -> MPath s
-- runMC path = runST $ do
--   return $ nudge path 2

-- nudge :: (PrimMonad m) => MPath (PrimState m) -> Int -> m ()
-- nudge path n = do

-- nudge :: Path -> Integer -> Path
-- nudge path n = runST $ do
--   init <- V.thaw path
--   MV.swap init 1 5
--   MV.write init 2 (fromIntegral n)
--   V.freeze init

