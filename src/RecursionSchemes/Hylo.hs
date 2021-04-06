{-# LANGUAGE GADTs #-}
module RecursionSchemes.Hylo where

import Control.Category ((>>>))

import RecursionSchemes.Custom.Gather
import RecursionSchemes.Custom.Scatter
import qualified Data.Functor.Foldable as F


thenHylo
  :: (F.Corecursive t, F.Recursive t)
  => CorecFun a t
  -> RecFun t b
  -> a -> b
thenHylo (CorecFun f) (RecFun g)
  = seed >>> F.hylo g f >>> recur

hylo
  :: (F.Corecursive t, F.Recursive t)
  => RecFun t b
  -> CorecFun a t
  -> a -> b
hylo = flip thenHylo
