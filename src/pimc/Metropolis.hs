module Metropolis (metroStep) where

import Control.Monad.Random

metroStep :: (RandomGen g) => (Double -> Double) -> Double -> Double -> Rand g Double
metroStep p delta x = do
  dx <- getRandomR (-delta, delta)
  let
    newx = x + dx
    p0 = p x
    p1 = p newx
    weightselect = do
      let odds = exp (p1 - p0)
      select <- getRandom
      return (if (select < odds) then newx else x)
  if (p1 > p0)
    then return newx
    else weightselect

