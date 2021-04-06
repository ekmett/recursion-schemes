{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, FunctionalDependencies, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
module RecursionSchemes.Internal.Gather where

import Control.Comonad.Cofree (Cofree((:<)))
import GHC.TypeLits

import RecursionSchemes.Custom.Gather
import qualified Data.Functor.Foldable as F


newtype CataPos a = CataPos
  { cataRecur :: a }

instance Recur a (CataPos a) where
  recur = cataRecur

cata :: Gather base a (CataPos a)
cata a _
  = CataPos a


class Untouched t pos | pos -> t where
  untouched :: pos -> t

instance {-# OVERLAPPABLE #-} (PosUntrans f, Untouched t pos) => Untouched t (f pos) where
  untouched = untouched . untransformed

data ParaPosT t pos = ParaPosT
  { paraUntouched     :: t
  , paraUntransformed :: pos
  }

instance Untouched t (ParaPosT t pos) where
  untouched = paraUntouched

instance PosUntrans (ParaPosT t) where
  untransformed = paraUntransformed

paraT
  :: (F.Corecursive t, base ~ F.Base t)
  => Gather base a pos
  -> Gather base a (ParaPosT t pos)
paraT gather a ps = ParaPosT
  { paraUntouched     = F.embed . fmap paraUntouched $ ps
  , paraUntransformed = gather a . fmap paraUntransformed $ ps
  }

para
  :: (F.Corecursive t, base ~ F.Base t)
  => Gather base a (ParaPosT t (CataPos a))
para
  = paraT cata


class Fast (s :: Symbol) b pos | pos -> b where
  fast :: pos -> b

instance {-# OVERLAPPABLE #-} (PosUntrans f, Fast s b pos) => Fast s b (f pos) where
  fast = fast @s . untransformed

data ZygoPosT (s :: Symbol) posB pos = ZygoPosT
  { zygoFast          :: posB
  , zygoUntransformed :: pos
  }

instance Recur b posB => Fast s b (ZygoPosT s posB pos) where
  fast = recur . zygoFast

instance PosUntrans (ZygoPosT s posB) where
  untransformed = zygoUntransformed

withZygotizedRecFun
  :: RecFun t a
  -> ( forall pos. Recur a pos
    => (F.Base t pos -> pos)
    -> r
     )
  -> r
withZygotizedRecFun (RecFun f) cc = cc f

zygoT
  :: forall s base posB a pos. Functor base
  => (base posB -> posB)
  -> Gather base a pos
  -> Gather base a (ZygoPosT s posB pos)
zygoT f gather a ps = ZygoPosT
  { zygoFast          = f . fmap zygoFast $ ps
  , zygoUntransformed = gather a . fmap zygoUntransformed $ ps
  }

zygo
  :: forall s base posB a. Functor base
  => (base posB -> posB)
  -> Gather base a (ZygoPosT s posB (CataPos a))
zygo f = zygoT f cata


newtype HistoPosT base pos = HistoPosT
  { unHistoPosT :: Cofree base pos }

type instance F.Base (HistoPosT base pos) = base
instance Functor base => F.Recursive (HistoPosT base pos) where
  project (HistoPosT (_ :< ps)) = fmap HistoPosT ps

instance PosUntrans (HistoPosT base) where
  untransformed (HistoPosT (pos :< _)) = pos

histoT
  :: forall base a pos. Functor base
  => Gather base a pos
  -> Gather base a (HistoPosT base pos)
histoT gather a ps = HistoPosT (pos :< cofrees)
  where
    pos :: pos
    pos = gather a . fmap untransformed $ ps

    cofrees :: base (Cofree base pos)
    cofrees = fmap unHistoPosT ps

histo
  :: Functor base
  => Gather base a (HistoPosT base (CataPos a))
histo = histoT cata
