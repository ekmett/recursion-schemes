module RecursionSchemes
  ( Base, ListF(..), Recursive(project), Corecursive(embed)

  , RecFun, runRecFun, recFun, traceRecFun
  , Recur(..), CataPos, cata
  , Untouched(..), ParaPosT, paraT, para
  , Fast(..), ZygoPosT, withZygotizedRecFun, zygoT, zygo
  , HistoPosT, histoT, histo

  , CorecFun, runCorecFun, corecFun
  , Seed(..), AnaPos, ana
  , Done(..), ApoPosT, apoT, apo
  , Switch(..), GapoPosT, gapoT, gapo
  , FutuPosT, futuT, futu
  ) where

import Data.Functor.Foldable hiding (cata, para, zygo, histo, ana, apo, gapo, futu)
import RecursionSchemes.Custom.Gather
import RecursionSchemes.Custom.Scatter
import RecursionSchemes.Internal.Gather
import RecursionSchemes.Internal.Scatter
