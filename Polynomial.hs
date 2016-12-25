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
 -XUndecidableInstances
 -XViewPatterns
#-}

module Polynomial where

import Prelude hiding ((+), (*), (-))
import Algebra.Additive as Additive hiding (sum)
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
import Utilities

--put this in HasFlow.Expr

-- |
-- = TYPES
-- == Expressions (for dimensions)

data Monomial = Monomial Integer [String] deriving Eq

--compare the terms, ignore the coefficient
instance Ord Monomial where
    compare (Monomial _ x) (Monomial _ y) = compare x y

monoMult :: Monomial -> Monomial -> Monomial 
monoMult (Monomial c1 x1) (Monomial c2 x2) = Monomial (c1*c2) (sort (x1++x2))

newtype Polynomial = Polynomial [Monomial] deriving Eq

unpoly :: Polynomial -> [Monomial]
unpoly (Polynomial li) = li

pmap f = Polynomial . map f . unpoly

simplifyPoly :: [Monomial] -> [Monomial] 
simplifyPoly = mapReduce (\(Monomial i x) -> (x, i)) (\x is -> let isum = sum is in if isum==0 then [] else [Monomial isum x])

pref :: String -> Polynomial
pref s = Polynomial [Monomial 1 [s]]

instance Show Monomial where
    show (Monomial c li) = 
        case (c,li) of
          (_, []) -> show c
          (1, _) -> intercalate "*" li
          _ -> (show c)++"*"++(intercalate "*" li)

instance Show Polynomial where
    show (Polynomial li) = 
        case li of
          [] -> "0"
          _ -> intercalate "+" $ map show li

instance Additive.C Polynomial where
    zero = Polynomial []
    li1 + li2 = Polynomial $ simplifyPoly ((unpoly li1) ++ (unpoly li2))
    negate = pmap (\(Monomial c x) -> Monomial (-c) x)

instance Algebra.Ring.C Polynomial where
    li1 * li2 = Polynomial $ simplifyPoly $ monoMult <$> (unpoly li1) <*> (unpoly li2)
    fromInteger c = Polynomial $ if c==0 then [] else [Monomial c []]


testExpr :: IO ()
testExpr = do
    --let a = ERef "a"
    --let b = ERef "b"
    --let c = ERef "c"
    let a = pref "a"
    let b = pref "b"
    let c = pref "c"
    forM_ [a*b, (a*b)*c, a*(b*c), a*a, a+a, a*b*a, a+b, a+a+b, (a+b)*(a+b), (a+b)*(a-b)] (putStrLn . show)
