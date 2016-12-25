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

module Expr where

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

--put this in HasFlow.Expr

-- |
-- = TYPES
-- == Expressions (for dimensions)

mapReduce :: (Ord b) => (a -> (b,c)) -> (b -> [c] -> [d]) -> [a] -> [d]
mapReduce mf rf = concat . M.elems . M.mapWithKey rf . foldl 
                  (\mp a -> 
                            let 
                                (b,c) = mf a
                            in
                              M.alter (\case 
                                        Nothing -> Just [c]
                                        Just x -> Just (c:x)) b mp) M.empty

data Monomial = Monomial Integer [String] deriving Eq

--compare the terms, ignore the coefficient
instance Ord Monomial where
    compare (Monomial _ x) (Monomial _ y) = compare x y

monoMult :: Monomial -> Monomial -> Monomial 
monoMult (Monomial c1 x1) (Monomial c2 x2) = Monomial (c1*c2) (sort (x1++x2))

newtype Polynomial = Polynomial [Monomial]

unpoly :: Polynomial -> [Monomial]
unpoly (Polynomial li) = li

pmap f = Polynomial . map f . unpoly

simplifyPoly :: [Monomial] -> [Monomial] 
simplifyPoly = mapReduce (\(Monomial i x) -> (x, i)) (\x is -> let isum = sum is in if isum==0 then [] else [Monomial isum x])

pref :: String -> Polynomial
pref s = Polynomial [Monomial 1 [s]]

instance Show Monomial where
    show (Monomial c li) = 
        (if c==1 then "" else (show c)++"*")++(intercalate "*" li)

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

data Expr = EInt Integer | ERef String | EAdd Expr Expr | EMul Expr Expr | EAdds [Expr] | EMuls [Expr] deriving (Eq, Ord)

instance Show Expr where
    show = \case
           EInt x -> show x
           ERef str -> str
           EAdd e1 e2 -> printf "(%s+%s)" (show e1) (show e2)
           EMul e1 e2 -> printf "(%s*%s)" (show e1) (show e2)
           EAdds le -> printf "(%s)" $ intercalate "+" $ map show le
           EMuls le -> printf "(%s)" $ intercalate "*" $ map show le

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

prepForSeq :: Expr -> [[Expr]]
prepForSeq e = case e of
                 EMuls es -> map (\x -> [x]) es
                 EAdds es -> [es]
                 EInt i -> [[EInt i]]
                 ERef a -> [[ERef a]]

simplifyMonomial :: [Expr] -> [Expr]
simplifyMonomial li = 
    let
        li' = mapMaybe (\case
                         ERef s -> Just s
                         _ -> Nothing) li
        li'' = mapMaybe (\case
                          EInt n -> Just n
                          _ -> Nothing) li
        coeff = product li''
        term = map ERef (sort li')
    in
      if coeff==1 then term else (EInt coeff):term 

freqTable :: (Ord a) => [a] -> [(a, Int)]
freqTable li = map (\li -> (li!!0, length li)) $ group $ sort li

normExpr :: Expr -> Expr
normExpr e = case e of
               EInt n -> e
               ERef s -> e
               EAdd e1 e2 -> normExpr $ EAdds [e1,e2]
               EMul e1 e2 -> normExpr $ EMuls [e1,e2]
               EAdds es -> 
                    let 
                        es' = map normExpr es
                    in normMonomialSum es'
               EMuls es -> 
                   let es' = map normExpr es
                   in EMuls es'
--                   in EAdds $ map (EMuls . (concat . map emulsToList)) $ sequence $ (es' >>= prepForSeq)
--normMonomialSum $
--simplifyMonomial . 
                   --simplify products

normMonomialSum :: [Expr] -> Expr
normMonomialSum es = 
    let li = map (\(t,c) -> scalarMultMonomial c t) $ freqTable $ concat $ map eaddsToList es
    in if (length li) == 1 then li!!0 else EAdds li
--put this in HasFlow.Tensor

scalarMultMonomial :: Int -> Expr -> Expr
scalarMultMonomial (toInteger -> c) t = case t of
                           EMuls ((EInt k):rest) -> 
                               let pr = c*k
                               in if pr==1 then removeRedundant (EMuls rest) else EMuls ((EInt pr):rest)
                           EMuls li -> 
                               if c==1 then removeRedundant (EMuls li) else EMuls ((EInt c):li)
                           other -> (EInt 999)*other --shouldn't be in this case

removeRedundant :: Expr -> Expr
removeRedundant ex = case ex of
                        EMuls [x] -> x
                        _ -> ex

-- |
-- == Shape

type Shape = Maybe [Expr]

showShape :: Shape -> String
showShape = \case
            Just e -> show e
            Nothing -> "[]"

class Shapable a where
    toShape :: a -> Shape

s :: (Shapable a) => a -> Shape
s = toShape


instance Shapable [Int] where
    toShape x = Just (map (EInt . toInteger) x)
{-
instance Shapable [Integer] where
    toShape x = Just (map EInt x)
-}
{-
instance Integral a => Shapable a where
    toShape x = Just [EInt $ toInteger x]

instance Integral a => Shapable [a] where
    toShape x = Just (map (EInt . toInteger) x)
-}
--instance Shapable Integer where
--    toShape x = Just [EInt x]

instance Shapable Int where
    toShape x = Just [EInt (toInteger x)]

instance Shapable String where
    toShape x = Just [ERef x]

instance Shapable [String] where
    toShape x = Just (map ERef x)

instance Shapable () where
    toShape () = Nothing

instance Shapable Shape where
    toShape = id

testExpr :: IO ()
testExpr = do
    --let a = ERef "a"
    --let b = ERef "b"
    --let c = ERef "c"
    let a = pref "a"
    let b = pref "b"
    let c = pref "c"
    forM_ [a*b, (a*b)*c, a*(b*c), a*a, a+a, a*b*a, a+b, a+a+b, (a+b)*(a+b), (a+b)*(a-b)] (putStrLn . show)
