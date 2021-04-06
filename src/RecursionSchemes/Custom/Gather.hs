{-# LANGUAGE FlexibleInstances, FunctionalDependencies, GADTs, ScopedTypeVariables, UndecidableInstances #-}
module RecursionSchemes.Custom.Gather where

import qualified Data.Functor.Foldable as F


type Gather base a pos
  = a -> base pos -> pos


class PosUntrans f where
  untransformed :: f pos -> pos

class Recur a pos | pos -> a where
  recur :: pos -> a

instance {-# OVERLAPPABLE #-} (PosUntrans f, Recur a pos) => Recur a (f pos) where
  recur = recur . untransformed


data RecFun t a where
  RecFun :: (Recur a pos, base ~ F.Base t)
         => (base pos -> pos)
         -> RecFun t a

runRecFun
  :: F.Recursive t
  => RecFun t a
  -> t -> a
runRecFun (RecFun f)
  = recur . F.cata f

recFun
  :: (Recur a pos, base ~ F.Base t)
  => Gather base a pos
  -> (base pos -> a)
  -> RecFun t a
recFun gather f
  = RecFun $ \ps -> gather (f ps) ps


traceRecFun
  :: forall t base a
   . ( F.Recursive t, base ~ F.Base t
     , Traversable base
     , Show t, Show a
     )
  => String
  -> RecFun t a
  -> t -> IO a
traceRecFun name = \(RecFun f) t -> recur <$> go f 1 t
  where
    go :: (base ~ F.Base t, Recur a pos)
       => (base pos -> pos)
       -> Int
       -> t -> IO pos
    go f depth t = do
      putStrLn $ take depth (repeat '>') ++ " " ++ name ++ " " ++ show t
      ps <- traverse (go f (depth + 1)) (F.project t)
      let p = f ps
      putStrLn $ take depth (repeat '<') ++ " " ++ name ++ " " ++ show t ++ " = " ++ show (recur p)
      pure p
