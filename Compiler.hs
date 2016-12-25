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
import Text.Regex
import Data.Char
import qualified Data.Set as S
import Data.String.Utils
import Data.List
import qualified Data.Map as M
import Data.Functor
import Control.Applicative

import MonadUtilities
import Utilities
import Polynomial
import Tensor
import Graph
import Functions
import Args
import Shape

--put this in HasFlow.Compilers.Base

-- |
-- = Compiler

data ProgramData = ProgramData {_defaultInits :: String, _scopeList :: [String], _vars :: M.Map String T, _curIndex :: Int}

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
compile = compile' (ProgramData {_defaultInits = "", _scopeList = [], _vars = M.empty, _curIndex = 0})
--no scope right now

compile' :: ProgramData -> Flow T -> String
compile' pd = \case
              Free (SetDefaultInits str next) -> compile' (pd & defaultInits .~ str) next
              Free (InitVar str dims f nextf) -> (withIndent pd (printf "%s = get_variable(\"%s\", %s, %s)" str str (showShape dims) (show f))) ++ (compile' (pd & vars %~ M.insert (printf "%s/%s" (intercalate "/" $ pd ^. scopeList) str) (T (Ref str) dims)) 
                                                 (nextf $ T (Ref str) Nothing))
              Free (InitVarWithDefault str dims nextf) -> compile' pd (Free $ InitVar str dims (PCode $ pd ^. defaultInits) nextf)
              Free (InitPH str dims nextf) -> (withIndent pd (printf "%s = get_variable(\"%s\", %s, var_type=\"placeholder\")" str str (showShape dims))) ++ (compile' (pd & vars %~ M.insert (printf "%s/%s" (intercalate "/" $ pd ^. scopeList) str) (T (Ref str) dims)) (nextf $ T (Ref str) Nothing))
              Free (AddScope str next) -> (withIndent pd (printf "with tf.variable_scope(\"%s\"):" str))++(compile' (pd & scopeList %~ (++[str])) next)
              Free (ExitScope next) -> compile' (pd & scopeList %~ init) next
              Free (Get str nextf) -> compile' pd 
                                      (nextf $ T (Ref str) Nothing)
              Free (Save t nextf) -> 
                  let curVar = varNames !! (pd ^. curIndex)
                  in (withIndent pd (printf "%s = %s" curVar (compileT t))) ++ (compile' (pd & vars %~ M.insert curVar t & curIndex %~ (+1))
                     (nextf $ T (Ref curVar) Nothing))
              Pure t -> (withIndent pd (printf "%s = %s" (varNames!!(pd ^. curIndex)) (show t))) -- ++ compile' (pd & vars %~ S.insert (varNames!!(pd ^. curIndex)) & curIndex %~ (+1))

compileT = compileTV . val

compileTV :: TVal -> String
compileTV = \case 
                        F x -> show x
                        L li -> printf "[%s]" $ intercalate "," (map compileTV li)
                        Ref str -> str
                        Add t1 t2 -> printf "(%s + %s)" (compileTV t1) (compileTV t2) 
                        Mul t1 t2 -> printf "tf.matmul(%s, %s)" (compileTV t1) (compileTV t2) 
                        TFun s li args _ -> 
                            case M.lookup s funMap of
                              Just (str, defArgs) -> entryToF (str, defArgs) li args 
                              _ -> printf "(ERROR: FUNCTION %s NOT FOUND)" s
-- entryToF :: (String, PyArgs) -> [T] -> PyArgs -> String

--printf "%s(%s)" s (intercalate "," $ map compileTV li)
{-
data TVal = F Float | L [TVal] | Ref String | Add TVal TVal | Mul TVal TVal
          | TFun String [TVal] PyArgs ([[Polynomial]] -> Shape)
-}

{-
replaces :: [(String, String)] -> String -> String
replaces = foldIterate (uncurry replace) 
-}
--foldl1 (\s1 (x,y) -> replace x y s1)

repeatUntilNothing :: (a -> Maybe a) -> a -> a
repeatUntilNothing f x = case f x of
                           Nothing -> x
                           Just y -> repeatUntilNothing f y

entryToF :: (String, PyArgs) -> [TVal] -> PyArgs -> String
entryToF (str, defArgs) li args = repeatUntilNothing
                          (\st -> do
                             (beg, match, after, _) <- matchRegexAll (mkRegex "\\$([a-zA-Z]+|[0-9]+|\\$)") st
                             let m = match!!1
                             let ms = tail match
                             repl <-
                                 if (isAlpha m) 
                                 then fmap show $ chooseLeft (M.lookup ms args) (M.lookup ms defArgs)
                                 else if (isDigit m)
                                      then do
                                        t <- li `mindex` ((read ms) - 1)
                                        return (compileTV t)
                                      else return (printf "[%s]" $ intercalate "," (map compileTV li))
                             return (beg++repl++after)) str

funMap :: M.Map String (String, PyArgs)
funMap = M.fromList
         [("concat", ("tf.concat($axis, $$)", M.fromList [("axis", p (1::Int))])),
          ("get", ("$1[$index]", M.empty)), -- ?
          --M.fromList [("index", p (0::Int))]
          ("pack", ("tf.pack($$)", M.empty)), -- ?
          ("sigmoid", ("tf.sigmoid($1)", M.empty)),
          ("softmax", ("tf.softmax($1)", M.empty)),
          ("tanh", ("tf.tanh($1)", M.empty)),
          ("zeros", ("zeros($shape)", M.empty)),
          (".*", ("$1 * $2", M.empty))]
          
