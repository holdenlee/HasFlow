module Args where

import Data.Map as M

data Py = PI Int | PF Float | PCode String | PL [Py] 

type PyArgs = M.Map String Py
