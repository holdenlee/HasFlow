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
import Algebra.Ring
import Control.Monad
import Control.Monad.Free
import Control.Lens
import Text.Printf
import qualified Data.Set as S
import Data.List
import Data.Functor
import Control.Applicative

-- # TYPES

-- ## Expressions

data Expr = EInt Integer | ERef String | EAdd Expr Expr | EMul Expr Expr | ENeg Expr

instance Additive.C Expr where
    zero = EInt 0
    (+) = EAdd
    negate = ENeg

instance Algebra.Ring.C Expr where
    (*) = EMul
    fromInteger = EInt

type Shape = [Expr]

data T = Ref String | Num Float | Plus T T

instance Show T where
    show = \case
           Ref str -> str
           Num x -> show x
           Plus t1 t2 -> printf "(%s+%s)" (show t1) (show t2)

data TGraph next = 
    SetDefaultInits String next
    | InitVar String [Int] String (T -> next)
    | InitVarWithDefault String [Int] (T -> next)
--    | GetScope String (String -> next) 
--    | SetScope String next
    | AddScope String next
--    | NewScope next
    | ExitScope next
    | Get String (T -> next)
    | Save T (T -> next) deriving Functor

type TF = Free TGraph 

{-
instance Functor TGraph where
    fmap f = \case
             SetDefaultInits str next -> SetDefaultInits str (f next)
             InitVar str li def g -> InitVar str li def (f . g)
             InitVarWithDefault str li g -> InitVarWithDefault str li (f.g)
             AddScope str next -> AddScope str (f next)
             ExitScope next -> ExitScope (f next)
             Get str g -> Get str (f . g)
             Save t g -> Save t (f . g)
-}

setDefaultInits str = liftF (SetDefaultInits str ())

initVar :: String -> [Int] -> String -> TF T
initVar str li f =  Free $ InitVar str li f Pure
--liftF (InitVar str li f id)

initVarWithDefault :: String -> [Int] -> TF T
initVarWithDefault str li =  Free $ InitVarWithDefault str li Pure
--liftF (InitVarWithDefault str id)

get str = liftF (Get str id)
save t = liftF (Save t id)

--getScope str = liftF (GetScope str id)
--setScope str = liftF (SetScope str ())
addScope str = liftF (AddScope str ())
exitScope = liftF (ExitScope ())

scope :: String -> TF a -> TF a
scope str tf = do
--  cur <- getScope
  addScope str
  x <- tf
--  setScope cur
  exitScope
  return x

--compile :: TF T -> String
--compile 

data ProgramData = ProgramData {_indent :: Int, _defaultInits :: String, _scopeList :: [String], _vars :: S.Set String, _curIndex :: Int}

alphabet = "abcdefghijklmnopqrstuvwxyz"

--this isn't actually what I want but whatever...
varNames = (map (\x -> [x]) alphabet)++((:) <$> alphabet <*> varNames) & map ('_':)

makeLenses ''ProgramData

withIndent pd str = (replicate (4*(pd ^. indent)) ' ')++str++"\n"

compile :: TF T -> String
compile = compile' (ProgramData {_indent = 0, _defaultInits = "", _scopeList = [], _vars = S.empty, _curIndex = 0})
--no scope right now

compile' :: ProgramData -> TF T -> String
compile' pd = \case
              Free (SetDefaultInits str next) -> compile' (pd & defaultInits .~ str) next
              Free (InitVar str dims f nextf) -> (withIndent pd (printf "%s = get_variable(%s, %s, %s)" str str (show dims) f)) ++ (compile' (pd & vars %~ S.insert (printf "%s/%s" (intercalate "/" $ pd ^. scopeList) str)) (nextf $ Ref str))
              Free (InitVarWithDefault str dims nextf) -> compile' pd (Free $ InitVar str dims (pd ^. defaultInits) nextf)
              Free (AddScope str next) -> (withIndent pd (printf "with tf.variable_scope(\"%s\")" str))++(compile' (pd & indent %~ (+1)  & scopeList %~ (++[str])) next)
              Free (ExitScope next) -> compile' (pd & indent %~ (\x -> x - 1)) next
              Free (Get str nextf) -> compile' pd (nextf (Ref str))
              Free (Save t nextf) -> 
                  let curVar = varNames !! (pd ^. curIndex)
                  in (withIndent pd (printf "%s = %s" curVar (show t))) ++ (compile' (pd & vars %~ S.insert curVar & curIndex %~ (+1)) (nextf (Ref curVar)))
              Pure t -> (withIndent pd (printf "%s = %s" (varNames!!(pd ^. curIndex)) (show t))) -- ++ compile' (pd & vars %~ S.insert (varNames!!(pd ^. curIndex)) & curIndex %~ (+1))

test :: TF T
test = do
  setDefaultInits "default"
  (a,b) <- scope "foo" $ do
         a <- initVar "a" [4] "init"
         b <- initVarWithDefault "b" [4]
         return (a, b)  
  c <- save (Plus a b)
  d <- return (Plus c c)
  e <- get "a"
  return (Plus e d)

do_test = putStrLn (compile test)
