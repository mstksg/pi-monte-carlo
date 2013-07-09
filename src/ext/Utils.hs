{-# LANGUAGE RankNTypes #-}

module Utils (runReaderST) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.ST

runReaderST :: (RandomGen g) => (forall s. RandT g (ReaderT r (ST s)) a) -> r -> Rand g a
runReaderST st env = do
  splittedSeed <- getSplit
  return $ runST $ runReaderT (evalRandT st splittedSeed) env
