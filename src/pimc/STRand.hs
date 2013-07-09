{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, Rank2Types #-}

module STRand (STRand) where

import Control.Monad.Primitive
import Control.Monad.ST.Trans
import Control.Monad.ST.Trans.Internal
import Control.Monad.Random
import GHC.Base
import Control.Monad

-- data STRand s g

type STRand s g = STT s (Rand g)

instance PrimMonad (STRand s g) where
  type PrimState (STRand s g) = s
  primitive = STT
  -- internal (STRand s g) = s 

-- newtype STRand s g a = STRand (STRep s g a)
-- type STRep s g a = State# s -> (# (State# s, g), a #)

-- instance Functor (STRand s) where
--     fmap f (STRand m) = STRand $ \ s ->
--       case (m s) of { (# new_s, r #) ->
--       (# new_s, f r #) }

-- instance Monad (STRand s g) where
--     {-# INLINE return #-}
--     {-# INLINE (>>)   #-}
--     {-# INLINE (>>=)  #-}
--     return x = STRand (\s -> (# s, x #))
--     m >> k   = m >>= \ _ -> k
--     (STRand m) >>= k
--       = STRand (\ s ->
--         case (m s) of { (# new_s, r #) ->
--         case (k r) of { STRand k2 ->
--         (k2 new_s) }})

-- data STret s a = STret (State# s) a
