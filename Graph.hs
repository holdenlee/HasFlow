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

module Graph where

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
import Args

-- |
-- = TensorFlow graph and monad

data TGraph next = 
    SetDefaultInits String next
    | InitVar String Shape Py (T -> next)
    | InitVarWithDefault String Shape (T -> next)
--    | GetScope String (String -> next) 
--    | SetScope String next
    | AddScope String next
--    | NewScope next
    | ExitScope next
    | Get String (T -> next)
    | Save T (T -> next) 
      deriving Functor

--    | Fun String T (T -> (String, T)) (T -> next)


type Flow = Free TGraph 

setDefaultInits str = liftF (SetDefaultInits str ())

initVar :: (Shapable a, Argable s) => String -> a -> s -> Flow T
initVar str li f =  Free $ InitVar str (s li) (p f) Pure
--liftF (InitVar str li f id)

initVarWithDefault :: (Shapable a) => String -> a -> Flow T
initVarWithDefault str li =  Free $ InitVarWithDefault str (s li) Pure
--liftF (InitVarWithDefault str id)

get str = liftF (Get str id)
save t = liftF (Save t id)

--getScope str = liftF (GetScope str id)
--setScope str = liftF (SetScope str ())
addScope str = liftF (AddScope str ())
exitScope = liftF (ExitScope ())

scope :: String -> Flow a -> Flow a
scope str tf = do
--  cur <- getScope
  addScope str
  x <- tf
--  setScope cur
  exitScope
  return x

--set default init within scope
