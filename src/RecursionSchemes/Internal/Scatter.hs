{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, FunctionalDependencies, LambdaCase, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
module RecursionSchemes.Internal.Scatter where

import Control.Monad.Free (Free(..))
import GHC.TypeLits

import RecursionSchemes.Custom.Scatter
import qualified Data.Functor.Foldable as F


newtype AnaPos a
  = AnaSeed a

instance Seed a (AnaPos a) where
  seed = AnaSeed

ana :: Scatter base a (AnaPos a)
ana (AnaSeed a) = do
  Left a 


class Done t pos | pos -> t where
  done :: t -> pos

instance {-# OVERLAPPABLE #-} (PosTrans f, Done t pos) => Done t (f pos) where
  done = transformed . done

data ApoPosT t pos
  = ApoDone t
  | ApoTransformed pos

instance Done t (ApoPosT t pos) where
  done = ApoDone

instance PosTrans (ApoPosT t) where
  transformed = ApoTransformed

apoT
  :: (F.Recursive t, base ~ F.Base t)
  => Scatter base a pos
  -> Scatter base a (ApoPosT t pos)
apoT scatter = \case
  ApoDone t -> do
    pure $ fmap ApoDone $ F.project t
  ApoTransformed pos -> do
    fmap ApoTransformed <$> scatter pos

apo
  :: (F.Recursive t, base ~ F.Base t)
  => Scatter base a (ApoPosT t (AnaPos a))
apo
  = apoT ana


class Switch (s :: Symbol) b pos | pos -> b where
  switch :: b -> pos

instance {-# OVERLAPPABLE #-} (PosTrans f, Switch s b pos) => Switch s b (f pos) where
  switch = transformed . switch @s

data GapoPosT (s :: Symbol) posB pos
  = GapoSwitch posB
  | GapoTransformed pos

instance Seed b posB => Switch s b (GapoPosT s posB pos) where
  switch = GapoSwitch . seed

instance PosTrans (GapoPosT s posB) where
  transformed = GapoTransformed

withGapotizedCorecFun
  :: CorecFun a t
  -> ( forall pos. Seed a pos
    => (pos -> F.Base t pos)
    -> r
     )
  -> r
withGapotizedCorecFun (CorecFun f) cc = cc f

gapoT
  :: forall s base posB a pos. Functor base
  => (posB -> base posB)
  -> Scatter base a pos
  -> Scatter base a (GapoPosT s posB pos)
gapoT f scatter = \case
  GapoSwitch posB -> do
    pure $ fmap GapoSwitch $ f posB
  GapoTransformed pos -> do
    fmap GapoTransformed <$> scatter pos

gapo
  :: forall s base posB a. Functor base
  => (posB -> base posB)
  -> Scatter base a (GapoPosT s posB (AnaPos a))
gapo f = gapoT f ana


newtype FutuPosT base pos = FutuPosT
  { unFutuPosT :: Free base pos }

type instance F.Base (FutuPosT base pos) = base
instance Functor base => F.Corecursive (FutuPosT base pos) where
  embed = FutuPosT . Free . fmap unFutuPosT

instance PosTrans (FutuPosT base) where
  transformed = FutuPosT . Pure

futuT
  :: forall base a pos. Functor base
  => Scatter base a pos
  -> Scatter base a (FutuPosT base pos)
futuT scatter = \case
  FutuPosT (Pure pos) -> do
    fmap transformed <$> scatter pos
  FutuPosT (Free ps) -> do
    pure $ fmap FutuPosT ps

futu
  :: Functor base
  => Scatter base a (FutuPosT base (AnaPos a))
futu = futuT ana
