{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
 -XTemplateHaskell
 -XFlexibleContexts
 -XDeriveFunctor
#-}

module Tensor where

import Prelude hiding ((+), (*), (-), fromInteger)
import Algebra.Additive as Additive
import Algebra.Ring hiding (product)
import Control.Monad
import Control.Monad.Free
import Control.Lens
import Data.Maybe
import Text.Printf
import qualified Data.Set as S
import Data.List
import qualified Data.Map as M
import Data.Functor
import Control.Applicative

import MonadUtilities
import Polynomial
import Args
import Shape

-- |
-- == Tensors

--these isn't what I actually want... for demonstration purposes only...
data TInt = TI Int | TIL [TInt] deriving Show

data TFloat = TF Float | TFL [TFloat] deriving Show

data T = TInt TInt | TFloat TFloat | Ref String | Add T T | Mul T T
       | TFun String [T] PyArgs ([[Polynomial]] -> Shape)
--for simplicity can merge Add, Mul into TFun

instance Show T where
    show = \case
              TInt is -> show is
              TFloat fs -> show fs
              Ref str -> str
              Add t1 t2 -> printf "(%s + %s)" (show t1) (show t2) 
              Mul t1 t2 -> printf "(%s * %s)" (show t1) (show t2) 
              TFun s li _ _ -> s ++ (show li)

instance Additive.C T where
    zero = TFloat (TF 0)
    (+) = Add
    negate = Mul (TFloat (TF (-1)))
    --ENeg

instance Algebra.Ring.C T where
    (*) = Mul
    fromInteger = TFloat . TF . Algebra.Ring.fromInteger

tryAdd :: Shape -> Shape -> Shape
tryAdd s1 s2 = case (s1,s2) of
                  (Just x, Just y) -> 
                      let
                          (shorter, longer) = if (length x) < (length y) then (x,y) else (y,x)
                      in
                        if shorter `isSuffixOf` longer then Just longer else Nothing
                  --assume simplified already
                  _ -> Nothing

--FIX: This isn't how matmul works in tensorflow.
tryMul :: Shape -> Shape -> Shape
tryMul s1 s2 = case (s1, s2) of
                 (Just li1, Just li2) -> if last li1 == head li2 then Just ((init li1) ++ (tail li2)) else Nothing
                 _ -> Nothing
