{-# LANGUAGE FlexibleInstances, FunctionalDependencies, GADTs, UndecidableInstances #-}
module RecursionSchemes.Custom.Scatter where

import qualified Data.Functor.Foldable as F


type Scatter base a pos
  = pos -> Either a (base pos)


class PosTrans f where
  transformed :: pos -> f pos

class Seed a pos | pos -> a where
  seed :: a -> pos

instance {-# OVERLAPPABLE #-} (PosTrans f, Seed a pos) => Seed a (f pos) where
  seed = transformed . seed


data CorecFun a t where
  CorecFun :: (Seed a pos, base ~ F.Base t)
           => (pos -> base pos)
           -> CorecFun a t

runCorecFun
  :: F.Corecursive t
  => CorecFun a t
  -> a -> t
runCorecFun (CorecFun f)
  = F.ana f . seed

corecFun
  :: (Seed a pos, base ~ F.Base t)
  => Scatter base a pos
  -> (a -> base pos)
  -> CorecFun a t
corecFun scatter f
  = CorecFun (either f id . scatter)
