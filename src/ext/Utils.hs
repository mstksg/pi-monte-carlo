{-# LANGUAGE RankNTypes #-}

module Utils (liftST) where

import Control.Monad.Random
import Control.Monad.ST

liftST :: (RandomGen g, Monad m) => (forall s. RandT g (ST s) a) -> RandT g m a
liftST st = do
  splittedSeed <- getSplit
  return $ runST $ evalRandT st splittedSeed

