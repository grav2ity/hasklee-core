module Hasklee.Sample where

import Data.Maybe
import Linear

import Hasklee.Transform


sample' :: (Trans a, Metric b)
        => a -> Maybe a -> a -> (a -> b a) -> (a -> b a) -> ([(a, a, b a)], a)
sample' d slen mlen f df =
  let
    initialStep = slen ^/ norm (df 0)
    go (tlen, s) as =
      let step = d / norm (df s)
          leftover = s - mlen
          new = (s, tlen, f s)
      in
        if leftover > 0 then (new:as, leftover * norm (df s))
          else go (tlen + d, s + step) (new:as)
  in go (fromMaybe 0 slen, fromMaybe 0 initialStep) []
