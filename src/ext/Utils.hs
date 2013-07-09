{-# LANGUAGE RankNTypes #-}

module Utils (runReaderST, lift2) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.ST

runReaderST :: (RandomGen g) => (forall s. RandT g (ReaderT r (ST s)) a) -> r -> Rand g a
runReaderST st env = do
  splittedSeed <- getSplit
  return $ runST $ runReaderT (evalRandT st splittedSeed) env

lift2 :: (Monad (t1 m), Monad m, MonadTrans t, MonadTrans t1) => m a -> t (t1 m) a
lift2 = lift . lift
