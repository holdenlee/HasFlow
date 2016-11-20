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

module HasFlow where

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
import Functions
import Compiler

test :: Flow T
test = do
  setDefaultInits "default"
  (a,b) <- scope "foo" $ do
         a <- initVar "a" [4] "init"
         b <- initVarWithDefault "b" [4]
         return (a, b)  
  c <- save (a + b)
  d <- return (c + c)
  e <- get "a"
  return (e + d)

do_test = putStrLn (compile test)

