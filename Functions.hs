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

module Functions where

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
import Tensor
import Graph
import Args

-- |
-- == Making functions
makeFun :: String -> PyArgs -> ([Expr] -> Shape) -> T -> T
makeFun str args f x = T (TFun str [val x] args (\[x] -> f x)) (shape x >>= f)
-- note Shape = Maybe [Expr]
-- TFun String [TVal] PyArgs ([Shape] -> Maybe Shape)

make_ :: (a -> PyArgs -> b -> c -> d) -> a -> b -> c -> d
make_ f a = f a M.empty

makeFun_ :: String -> ([Expr] -> Shape) -> T -> T
makeFun_ = make_ makeFun

--ex. conv2d($1, $2, $stride, **): $1, 2 are from args, $stride is lookup in pyargs, ** is rest of stuff in dictionary.

sigmoid :: T -> T
sigmoid = makeFun_ "sigmoid" Just 
--define a whole host this way.

-- |
-- == Monadic functions

{-
stack :: Int -> (a -> Flow a) -> a -> Flow a
stack n f x = repeatM n f x
-}

chainM :: (Monad m) => [a -> m a] -> a -> m a
chainM li x = foldl (>>=) (pure x) li

stacks :: String -> Int -> (a -> Flow a) -> a -> Flow a
stacks str n f = chainM (map (\i -> scope (str++(show i)) . f) [1..n])
--stacks str n f x = chainM (\y -> scope (map ((str++).show) [1..n]) $ f y) x
--repeatM n (\y -> scope str $ f y) x


--scanM

