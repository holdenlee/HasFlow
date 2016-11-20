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

module Expr where

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

--put this in HasFlow.Expr

-- |
-- = TYPES
-- == Expressions (for dimensions)

data Expr = EInt Integer | ERef String | EAdd Expr Expr | EMul Expr Expr | EAdds [Expr] | EMuls [Expr] deriving (Show, Eq)

instance Additive.C Expr where
    zero = EInt 0
    (+) = EAdd
    negate = EMul (EInt (-1))
    --ENeg

instance Algebra.Ring.C Expr where
    (*) = EMul
    fromInteger = EInt

eaddsToList :: Expr -> [Expr]
eaddsToList e = case e of
                  EAdds es -> es
                  _ -> [e]

emulsToList :: Expr -> [Expr]
emulsToList e = case e of
                  EMuls es -> es
                  _ -> [e]

simplifyMonomial :: [Expr] -> [Expr]
simplifyMonomial li = 
    let
        li' = mapMaybe (\case
                         ERef s -> Just s
                         _ -> Nothing) li
        li'' = mapMaybe (\case
                          EInt n -> Just n
                          _ -> Nothing) li
    in
      (EInt (product li'')):(map ERef (sort li'))
                       

normExpr :: Expr -> Expr
normExpr e = case e of
               EInt n -> e
               ERef s -> e
               EAdd e1 e2 -> normExpr $ EAdds [e1,e2]
               EMul e1 e2 -> normExpr $ EMuls [e1,e2]
               EAdds es -> 
                    let es' = map normExpr es
                    in EAdds $ concat $ map eaddsToList es
                    --TODO: need to combine like terms!!!
               EMuls es -> 
                   let es' = map normExpr es
                   in EAdds $ map (EMuls . simplifyMonomial) $ sequence $ map eaddsToList es
                    --simplify products

--put this in HasFlow.Tensor

-- |
-- == Shape

type Shape = Maybe [Expr]
