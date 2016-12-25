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

import Prelude hiding ((+), (*), (-), sum, fromInteger)
import Algebra.Additive as Additive
import Algebra.Ring hiding (product)
import Control.Monad
import Control.Monad.Free
import Control.Lens
import Data.Maybe
import Text.Printf
import qualified Data.Set as S
import Data.List hiding (sum)
import qualified Data.Map as M
import Data.Functor
import Control.Applicative

import MonadUtilities
import Polynomial
import Tensor
import Graph
import Args
import Shape
import Utilities

-- |
-- == Making functions
makeFun :: String -> PyArgs -> ([Polynomial] -> Shape) -> T -> T
makeFun str args f x = TFun str [x] args (\[x] -> f x)
-- note Shape = Maybe [Polynomial]
-- TFun String [TVal] PyArgs ([Shape] -> Maybe Shape)

make_ :: (a -> PyArgs -> x) -> a -> x
make_ f a = f a M.empty

makeFun_ :: String -> ([Polynomial] -> Shape) -> T -> T
makeFun_ = make_ makeFun

makeFun2 :: String -> PyArgs -> ([Polynomial] -> [Polynomial] -> Shape) -> T -> T -> T
makeFun2 str args f x y = TFun str [x, y] args (\[x,y] -> f x y) 

makeFun2_ :: String -> ([Polynomial] -> [Polynomial] -> Shape) -> T -> T -> T
makeFun2_ = make_ makeFun2

makeFunL :: String -> PyArgs -> ([[Polynomial]] -> Shape) -> [T] -> T
makeFunL str args f xs = TFun str xs args f

--ex. conv2d($1, $2, $stride, **): $1, 2 are from args, $stride is lookup in pyargs, ** is rest of stuff in dictionary.

--define a whole host this way.

pack :: [T] -> T
pack = makeFunL "pack" (M.empty) (\li -> if all (==(li!!0)) li then Just ((fromInteger $ toInteger $ length li):(li!!0)) else Nothing)

sigmoid :: T -> T
sigmoid = makeFun_ "sigmoid" Just 

softmax :: T -> T
softmax = makeFun_ "softmax" Just

tanh :: T -> T
tanh = makeFun_ "tanh" Just

concatenate :: Int -> [T] -> T
concatenate n ts = TFun "concat" ts (M.fromList [("axis", p n)]) 
                   (\li -> 
                        let 
                            a = removeAt n (li!!0)
                        in
                          if (all ((==a) . (removeAt n)) li)
                          then Just $ insertAt n (sum $ map (!!n) li) a
                          else Nothing)

--(Just . head)
--fix dims! 
-- TFun String [TVal] PyArgs ([Shape] -> Maybe Shape)

zeros :: Shape -> T
zeros a =  TFun "zeros" [] (M.fromList [("shape", p (showShape a))]) (\_ -> a)

(.!) :: T -> Int -> T
(.!) x n = TFun "get" [x] (M.fromList [("index", p n)]) (Just . tail . (!!0))

(.!!) :: (Argable a) => T -> a -> T
(.!!) x a =  TFun "get" [x] (M.fromList [("index", p a)]) (\_ -> Nothing)

(.*) :: T -> T -> T
(.*) = makeFun2_ ".*" (\x y -> if x `isPrefixOf` y then Just y else Nothing)

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


chooseLeft :: Maybe a -> Maybe a -> Maybe a
chooseLeft a b = case (a,b) of
                   (Just l, _) -> Just l
                   (_, Just r) -> Just r
                   _ -> Nothing
