{-# OPTIONS
 
 -XTypeSynonymInstances
 -XFlexibleInstances
#-}

module Args where

--import Text.Printf
--import Data.List
import qualified Data.Map as M

data Py = PI Int | PF Float | PCode String | PL [Py] 

type PyArgs = M.Map String Py

class Argable a where
    toArg :: a -> Py

instance Argable Int where
    toArg = PI

instance Argable Float where
    toArg = PF

instance Argable String where
    toArg = PCode

instance (Argable a) => (Argable [a]) where
    toArg li = PL (map toArg li)
-- printf "[%s]" . intercalate ","

p :: (Argable a) => a -> Py
p = toArg
