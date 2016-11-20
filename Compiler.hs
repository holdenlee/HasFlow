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

module Compiler where

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

--put this in HasFlow.Compilers.Base

-- |
-- = Compilers

data ProgramData = ProgramData {_defaultInits :: String, _scopeList :: [String], _vars :: S.Set String, _curIndex :: Int}

alphabet = "abcdefghijklmnopqrstuvwxyz"

listprod :: [[a]] -> [[a]]
listprod lis = foldl (\r1 l -> (\x y -> x ++ [y]) <$> r1 <*> l) (map (\x -> [x]) (lis!!0)) (tail lis)
--(\x y -> x ++ [y]) <$> (map (\x -> [x]) li1) <*> li2

listpow :: [a] -> Int -> [[a]]
listpow li n = listprod (replicate n li)

varNames = concat (map (listpow alphabet) [1..]) & map ('_':)
-- (map (\x -> [x]) alphabet)++((:) <$> alphabet <*> varNames) & map ('_':)

makeLenses ''ProgramData

getIndent pd = 4*(length (pd ^. scopeList))

withIndent pd str = (replicate (getIndent pd) ' ')++str++"\n"

compile :: Flow T -> String
compile = compile' (ProgramData {_defaultInits = "", _scopeList = [], _vars = S.empty, _curIndex = 0})
--no scope right now

compile' :: ProgramData -> Flow T -> String
compile' pd = \case
              Free (SetDefaultInits str next) -> compile' (pd & defaultInits .~ str) next
              Free (InitVar str dims f nextf) -> (withIndent pd (printf "%s = get_variable(%s, %s, %s)" str str (show dims) f)) ++ (compile' (pd & vars %~ S.insert (printf "%s/%s" (intercalate "/" $ pd ^. scopeList) str)) 
                                                 (nextf $ T (Ref str) Nothing))
              Free (InitVarWithDefault str dims nextf) -> compile' pd (Free $ InitVar str dims (pd ^. defaultInits) nextf)
              Free (AddScope str next) -> (withIndent pd (printf "with tf.variable_scope(\"%s\")" str))++(compile' (pd & scopeList %~ (++[str])) next)
              Free (ExitScope next) -> compile' (pd & scopeList %~ init) next
              Free (Get str nextf) -> compile' pd 
                                      (nextf $ T (Ref str) Nothing)
              Free (Save t nextf) -> 
                  let curVar = varNames !! (pd ^. curIndex)
                  in (withIndent pd (printf "%s = %s" curVar (show t))) ++ (compile' (pd & vars %~ S.insert curVar & curIndex %~ (+1)) 
                     (nextf $ T (Ref curVar) Nothing))
              Pure t -> (withIndent pd (printf "%s = %s" (varNames!!(pd ^. curIndex)) (show t))) -- ++ compile' (pd & vars %~ S.insert (varNames!!(pd ^. curIndex)) & curIndex %~ (+1))
