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

import Prelude hiding ((+), (*), (-))
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
import Expr
import Args

-- |
-- == Tensors
data TVal = F Float | L [TVal] | Ref String | Add TVal TVal | Mul TVal TVal
          | TFun String [TVal] PyArgs ([[Expr]] -> Shape)
-- I Int

instance Show TVal where
    show = \case
              F i -> show i
              L li -> printf "[%s]" (intercalate "," $ map show li)
              Ref str -> str
              Add t1 t2 -> printf "(%s + %s)" (show t1) (show t2) 
              Mul t1 t2 -> printf "(%s * %s)" (show t1) (show t2) 
              TFun s li _ _ -> s ++ (show li)

instance Additive.C TVal where
    zero = F 0
    (+) = Add
    negate = Mul (F (-1))
    --ENeg

instance Algebra.Ring.C TVal where
    (*) = Mul
    fromInteger = F . Algebra.Ring.fromInteger

data T = T TVal Shape

val (T val _) = val
shape (T _ sh) = sh
-- TODO: make this a lens

instance Show T where
    show (T x _) = show x

tryAdd :: Shape -> Shape -> Shape
tryAdd s1 s2 = case (s1,s2) of
                  (Just x, Just y) -> if x==y then Just x else Nothing --assume simplified already
                  _ -> Nothing

tryMul :: Shape -> Shape -> Shape
tryMul s1 s2 = case (s1, s2) of
                 (Just li1, Just li2) -> if last li1 == head li2 then Just ((init li1) ++ (tail li2)) else Nothing
                 _ -> Nothing

instance Additive.C T where
    zero = T (F 0) (Just [EInt 1])
    (T v1 s1) + (T v2 s2) = T (v1 + v2) (s1 `tryAdd` s2)  
    negate (T v s) = T (Additive.negate v) s
    --ENeg

instance Algebra.Ring.C T where
    (T v1 s1) * (T v2 s2) = T (v1 * v2) (s1 `tryMul` s2) 
    fromInteger n = T (Algebra.Ring.fromInteger n) (Just [EInt 1])
