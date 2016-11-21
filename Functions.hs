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

makeFun2 :: String -> PyArgs -> ([Expr] -> [Expr] -> Shape) -> T -> T -> T
makeFun2 str args f x y = T (TFun str [val x, val y] args (\[x,y] -> f x y)) 
                          (do 
                            x' <- shape x
                            y' <- shape y
                            f x' y')

makeFun2_ :: String -> ([Expr] -> [Expr] -> Shape) -> T -> T -> T
makeFun2_ a = makeFun2 a M.empty

makeFunL :: String -> PyArgs -> ([[Expr]] -> Shape) -> [T] -> T
makeFunL str args f xs = T (TFun str (map val xs) args f) (sequence (map shape xs) >>= f)

--ex. conv2d($1, $2, $stride, **): $1, 2 are from args, $stride is lookup in pyargs, ** is rest of stuff in dictionary.

--define a whole host this way.

pack :: [T] -> T
pack = makeFunL "pack" (M.empty) (\_ -> Nothing) 

sigmoid :: T -> T
sigmoid = makeFun_ "sigmoid" Just 

softmax :: T -> T
softmax = makeFun_ "softmax" Just

tanh :: T -> T
tanh = makeFun_ "tanh" Just

concatenate :: Int -> [T] -> T
concatenate n ts = T (TFun "concat" (map val ts) (M.fromList [("axis", p (1::Int))]) (Just . head)) Nothing
--fix dims! 
-- TFun String [TVal] PyArgs ([Shape] -> Maybe Shape)

zeros :: (Argable a) => a -> T
zeros a =  T (TFun "zeros" [] (M.fromList [("shape", p a)]) (Just . head)) Nothing

(.!) :: (Argable a) => T -> a -> T
(.!) x a =  T (TFun "get" [val x] (M.fromList [("index", p a)]) (Just . head)) Nothing

(.*) :: T -> T -> T
(.*) = makeFun2_ ".*" (\x y -> if x==y then Just x else Nothing)

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

chooseLeft :: Maybe a -> Maybe a -> Maybe a
chooseLeft a b = case (a,b) of
                   (Just l, _) -> Just l
                   (_, Just r) -> Just r
                   _ -> Nothing
