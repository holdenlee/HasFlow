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
#-}

module Shape where

import Prelude hiding ((+), (*), (-), fromInteger)
import Algebra.Additive as Additive hiding (sum)
import Algebra.Ring hiding (product)
import Data.Maybe

import Polynomial

-- |
-- == Shape

type Shape = Maybe [Polynomial]

showShape :: Shape -> String
showShape = \case
            Just e -> show e
            Nothing -> "[]"

class Shapable a where
    toShape :: a -> Shape

s :: (Shapable a) => a -> Shape
s = toShape


instance Shapable [Int] where
    toShape x = Just (map (fromInteger . toInteger) x)

instance Shapable Int where
    toShape x = Just [fromInteger $ toInteger x]

instance Shapable String where
    toShape x = Just [pref x]

instance Shapable [String] where
    toShape x = Just (map pref x)

instance Shapable () where
    toShape () = Nothing

instance Shapable Shape where
    toShape = id
