{-# LANGUAGE RankNTypes #-}

module Utils (liftST) where

import Control.Monad.Random
import Control.Monad.ST

liftST :: (RandomGen g) => (forall s. RandT g (ST s) a) -> Rand g a
liftST st = do
  splittedSeed <- getSplit
  return $ runST $ evalRandT st splittedSeed

