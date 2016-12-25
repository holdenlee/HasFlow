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
import Data.String.Utils hiding (join)
import Data.List
import qualified Data.Map as M
import Data.Functor
import Control.Applicative
import Control.Monad.Writer.Lazy

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

data ProgramData = ProgramData {_defaultInits :: String, _scopeList :: [String], _vars :: M.Map String T, _shapes :: M.Map String Shape, _curIndex :: Int}

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

withIndents pd = concat . map (withIndent pd)

compile :: Flow T -> String
compile = compile' (ProgramData {_defaultInits = "", _scopeList = [], _vars = M.empty, _shapes = M.empty, _curIndex = 0})
--no scope right now

compile' :: ProgramData -> Flow T -> String
compile' pd = \case
              Free (SetDefaultInits str next) -> compile' (pd & defaultInits .~ str) next
              Free (InitVar str dims f nextf) -> (withIndent pd (printf "%s = get_variable(\"%s\", %s, %s)" str str (showShape dims) (show f))) ++ (compile' (pd & vars %~ M.insert (printf "%s/%s" (intercalate "/" $ pd ^. scopeList) str) (Ref str))
                                                 (nextf $ Ref str))
              Free (InitVarWithDefault str dims nextf) -> compile' pd (Free $ InitVar str dims (PCode $ pd ^. defaultInits) nextf)
              Free (InitPH str dims nextf) -> (withIndent pd (printf "%s = get_variable(\"%s\", %s, var_type=\"placeholder\")" str str (showShape dims))) ++ (compile' (pd & vars %~ M.insert (printf "%s/%s" (intercalate "/" $ pd ^. scopeList) str) (Ref str)) (nextf $ Ref str))
              Free (AddScope str next) -> (withIndent pd (printf "with tf.variable_scope(\"%s\"):" str))++(compile' (pd & scopeList %~ (++[str])) next)
              Free (ExitScope next) -> compile' (pd & scopeList %~ init) next
              Free (Get str nextf) -> compile' pd 
                                      (nextf $ Ref str)
              Free (Save t nextf) -> 
                  let curVar = varNames !! (pd ^. curIndex)
                      (tc, sh) = compileT (pd ^. shapes) t
                  in (withIndent pd (printf "%s = %s" curVar tc)) ++ (compile' 
                     (pd & vars %~ M.insert curVar t 
                         & curIndex %~ (+1)
                         & shapes %~ M.insert curVar sh)
                     (nextf $ Ref curVar))
              Pure t -> (withIndent pd (printf "%s = %s" (varNames!!(pd ^. curIndex)) (show t))) -- ++ compile' (pd & vars %~ S.insert (varNames!!(pd ^. curIndex)) & curIndex %~ (+1))

--throwing away logging - change this
compileT :: M.Map String Shape -> T -> (String, Shape)
compileT m t = 
    let ((str, sh),w) = runWriter $ compileT' m t
    in (str, sh)

compileT' :: M.Map String Shape -> T -> Writer [String] (String, Shape)
compileT' m = \case
              TFloat x -> pure $ (show x, toShape (1::Int)) --shape not implemented
              TInt x -> pure $ (show x, toShape (1::Int)) --shape not implemented
              Ref str -> pure $ (str, join $ M.lookup str m)
              Add t1 t2 -> do
                (tc1, sh1) <- compileT' m t1
                (tc2, sh2) <- compileT' m t2
                let sh = tryAdd sh1 sh2
                let cur = printf "(%s + %s)" tc1 tc2
                tell $ [printf "# %s : %s" cur (showShape sh)]
                return (cur, sh)
              Mul t1 t2 -> do
                (tc1, sh1) <- compileT' m t1
                (tc2, sh2) <- compileT' m t2
                let sh = tryMul sh1 sh2
                let cur = printf "(%s * %s)" tc1 tc2
                tell $ [printf "# %s : %s" cur (showShape sh)]
                return (cur, sh)
              TFun s li args f -> 
                  do
                    -- [(String, Shape)]
                    results <- mapM (compileT' m) li
                    let (tcs, shs) = unzip results
                    -- shs :: [Shape = Maybe [Polynomial]]
                    let sh = sequence shs >>= f
                    let cur = case M.lookup s funMap of
                                Just (str, defArgs) -> entryToF (str, defArgs) tcs args
                                Nothing -> printf "(ERROR: FUNCTION %s NOT FOUND)[%s]" s (intercalate "," tcs)
                    tell $ [printf "# %s : %s" cur (showShape sh)]
                    return (cur, sh)

entryToF :: (String, PyArgs) -> [String] -> PyArgs -> String
entryToF (str, defArgs) cli args = loopUntilFail
                          (\st -> do
                             (beg, match, after, _) <- matchRegexAll (mkRegex "\\$([a-zA-Z]+|[0-9]+|\\$)") st
                             let m = match!!1
                             let ms = tail match
                             repl <-
                                 if (isAlpha m) 
                                 then fmap show $ chooseLeft (M.lookup ms args) (M.lookup ms defArgs)
                                 else if (isDigit m)
                                      then cli `mindex` ((read ms) - 1)
                                      else return (printf "[%s]" $ intercalate "," (cli))
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
          
