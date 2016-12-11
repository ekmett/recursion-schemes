{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (foldl')
import Test.HUnit

data Expr a
    = Lit a
    | Add (Expr a) (Expr a)
    | Expr a :* [Expr a]
  deriving (Show)

makeBaseFunctor ''Expr

expr1 :: Expr Int
expr1 = Add (Lit 2) (Lit 3 :* [Lit 4])

-- This is to test newtype derivation
--
-- Kind of a list
newtype L a = L { getL :: Maybe (a, L a) }
  deriving (Show, Eq)

makeBaseFunctor ''L

cons :: a -> L a -> L a
cons x xs = L (Just (x, xs))

nil :: L a
nil = L Nothing

main :: IO ()
main = do
    let expr2 = ana divCoalg 55 :: Expr Int
    14 @=? cata evalAlg expr1
    55 @=? cata evalAlg expr2

    let lBar = cons 'b' $ cons 'a' $ cons 'r' $ nil
    "bar" @=? cata lAlg lBar
    lBar @=? ana lCoalg "bar"
  where
    evalAlg (LitF x)   = x
    evalAlg (AddF x y) = x + y
    evalAlg (x :*$ y) = foldl' (*) x y

    divCoalg x
        | x < 5     = LitF x
        | even x    = 2 :*$ [x']
        | otherwise = AddF x' (x - x')
      where
        x' = x `div` 2

    lAlg (LF Nothing)        = []
    lAlg (LF (Just (x, xs))) = x : xs

    lCoalg []       = LF { getLF = Nothing } -- to test field renamer
    lCoalg (x : xs) = LF { getLF = Just (x, xs) }
