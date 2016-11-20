{-# OPTIONS
 
 -XTypeSynonymInstances
 -XFlexibleInstances
 -XLambdaCase
 -XTypeFamilies
 -XUndecidableInstances
 -XMultiParamTypeClasses
#-}

module Args where

import Text.Printf
import Data.List
import qualified Data.Map as M

data Py = PI Int | PF Float | PCode String | PL [Py] 

instance Show Py where
    show = \case
           PI x -> show x
           PF x -> show x
           PCode str -> str
           PL li -> printf "[%s]" $ intercalate "," (map show li)

type PyArgs = M.Map String Py

{-
data HTrue

data HFalse

data HNone

type family MakeFalse a where
    MakeFalse HTrue = HFalse
    MakeFalse HFalse = HFalse
    MakeFalse HNone = HNone

type family ArgPred a where
    ArgPred Int = HTrue
    ArgPred Float = HTrue
    ArgPred String = HTrue
    ArgPred [a] = MakeFalse (ArgPred a)
    ArgPred a = HNone

class Argable' a b where
    toArg' :: b -> a  -> Py

instance Argable' Int HTrue where
    toArg' _ = PI

instance Argable' Float HTrue where
    toArg' _ = PF

instance Argable' String HTrue where
    toArg' _ = PCode

instance Argable' [a] HFalse where
    toArg' _ li = PL (map (toArg' (undefined::HTrue)) li)
-}

class Argable a where
    toArg :: a -> Py

instance Argable Int where
    toArg = PI

instance Argable Float where
    toArg = PF

-- https://mail.haskell.org/pipermail/ghc-devs/2014-July/005830.html
instance {-# OVERLAPPING #-} Argable String where
    toArg = PCode

instance {-# OVERLAPPABLE #-} (Argable a) => (Argable [a]) where
    toArg li = PL (map toArg li)
-- printf "[%s]" . intercalate ","

p :: (Argable a) => a -> Py
p = toArg
