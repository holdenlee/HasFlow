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
