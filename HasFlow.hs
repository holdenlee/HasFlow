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

module HasFlow where

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
import Data.Functor
import Control.Applicative

-- # TYPES

-- ## Expressions (for dimensions)

data Expr = EInt Integer | ERef String | EAdd Expr Expr | EMul Expr Expr | EAdds [Expr] | EMuls [Expr] deriving (Show, Eq)
--  | ENeg Expr

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

-- ## Shape

type Shape = Maybe [Expr]

-- data Python = PI Int | PF Float | PCode String
--add lists

-- ## Tensors
data TVal = F Float | L [TVal] | Ref String | Add TVal TVal | Mul TVal TVal
-- I Int

instance Show TVal where
    show = \case
              F i -> show i
              L li -> printf "[%s]" (intercalate "," $ map show li)
              Ref str -> str
              Add t1 t2 -> printf "(%s + %s)" (show t1) (show t2) 
              Mul t1 t2 -> printf "(%s * %s)" (show t1) (show t2) 

instance Additive.C TVal where
    zero = F 0
    (+) = Add
    negate = Mul (F (-1))
    --ENeg

instance Algebra.Ring.C TVal where
    (*) = Mul
    fromInteger = F . Algebra.Ring.fromInteger

data T = T TVal Shape -- deriving Show

instance Show T where
    show (T x _) = show x

tryAdd :: Shape -> Shape -> Shape
tryAdd s1 s2 = case (s1,s2) of
                  (Just x, Just y) -> if x==y then Just x else Nothing --assume simplified already
                  _ -> Nothing

tryMul :: Shape -> Shape -> Shape
tryMul s1 s2 = case (s1, s2) of
                 (Just li1, Just li2) -> if last li1 == head li2 then Just ((init li1) ++ (tail li2)) else Nothing
                 _ -> Nothing

instance Additive.C T where
    zero = T (F 0) (Just [EInt 1])
    (T v1 s1) + (T v2 s2) = T (v1 + v2) (s1 `tryAdd` s2)  
    negate (T v s) = T (Additive.negate v) s
    --ENeg

instance Algebra.Ring.C T where
    (T v1 s1) * (T v2 s2) = T (v1 * v2) (s1 `tryMul` s2) 
    fromInteger n = T (Algebra.Ring.fromInteger n) (Just [EInt 1])

--data T = Ref String | Num Float | Plus T T | Mul T T

{-
instance Show T where
    show = \case
           Ref str -> str
           Num x -> show x
           Plus t1 t2 -> printf "(%s+%s)" (show t1) (show t2)
-}

data TGraph next = 
    SetDefaultInits String next
    | InitVar String [Int] String (T -> next)
    | InitVarWithDefault String [Int] (T -> next)
--    | GetScope String (String -> next) 
--    | SetScope String next
    | AddScope String next
--    | NewScope next
    | ExitScope next
    | Get String (T -> next)
    | Save T (T -> next) deriving Functor

type TF = Free TGraph 

{-
instance Functor TGraph where
    fmap f = \case
             SetDefaultInits str next -> SetDefaultInits str (f next)
             InitVar str li def g -> InitVar str li def (f . g)
             InitVarWithDefault str li g -> InitVarWithDefault str li (f.g)
             AddScope str next -> AddScope str (f next)
             ExitScope next -> ExitScope (f next)
             Get str g -> Get str (f . g)
             Save t g -> Save t (f . g)
-}

setDefaultInits str = liftF (SetDefaultInits str ())

initVar :: String -> [Int] -> String -> TF T
initVar str li f =  Free $ InitVar str li f Pure
--liftF (InitVar str li f id)

initVarWithDefault :: String -> [Int] -> TF T
initVarWithDefault str li =  Free $ InitVarWithDefault str li Pure
--liftF (InitVarWithDefault str id)

get str = liftF (Get str id)
save t = liftF (Save t id)

--getScope str = liftF (GetScope str id)
--setScope str = liftF (SetScope str ())
addScope str = liftF (AddScope str ())
exitScope = liftF (ExitScope ())

scope :: String -> TF a -> TF a
scope str tf = do
--  cur <- getScope
  addScope str
  x <- tf
--  setScope cur
  exitScope
  return x

--compile :: TF T -> String
--compile 

data ProgramData = ProgramData {_indent :: Int, _defaultInits :: String, _scopeList :: [String], _vars :: S.Set String, _curIndex :: Int}

alphabet = "abcdefghijklmnopqrstuvwxyz"

--this isn't actually what I want but whatever...
varNames = (map (\x -> [x]) alphabet)++((:) <$> alphabet <*> varNames) & map ('_':)

makeLenses ''ProgramData

withIndent pd str = (replicate (4*(pd ^. indent)) ' ')++str++"\n"

compile :: TF T -> String
compile = compile' (ProgramData {_indent = 0, _defaultInits = "", _scopeList = [], _vars = S.empty, _curIndex = 0})
--no scope right now

compile' :: ProgramData -> TF T -> String
compile' pd = \case
              Free (SetDefaultInits str next) -> compile' (pd & defaultInits .~ str) next
              Free (InitVar str dims f nextf) -> (withIndent pd (printf "%s = get_variable(%s, %s, %s)" str str (show dims) f)) ++ (compile' (pd & vars %~ S.insert (printf "%s/%s" (intercalate "/" $ pd ^. scopeList) str)) 
                                                 (nextf $ T (Ref str) Nothing))
              Free (InitVarWithDefault str dims nextf) -> compile' pd (Free $ InitVar str dims (pd ^. defaultInits) nextf)
              Free (AddScope str next) -> (withIndent pd (printf "with tf.variable_scope(\"%s\")" str))++(compile' (pd & indent %~ (+1)  & scopeList %~ (++[str])) next)
              Free (ExitScope next) -> compile' (pd & indent %~ (\x -> x - 1)) next
              Free (Get str nextf) -> compile' pd 
                                      (nextf $ T (Ref str) Nothing)
              Free (Save t nextf) -> 
                  let curVar = varNames !! (pd ^. curIndex)
                  in (withIndent pd (printf "%s = %s" curVar (show t))) ++ (compile' (pd & vars %~ S.insert curVar & curIndex %~ (+1)) 
                     (nextf $ T (Ref curVar) Nothing))
              Pure t -> (withIndent pd (printf "%s = %s" (varNames!!(pd ^. curIndex)) (show t))) -- ++ compile' (pd & vars %~ S.insert (varNames!!(pd ^. curIndex)) & curIndex %~ (+1))

test :: TF T
test = do
  setDefaultInits "default"
  (a,b) <- scope "foo" $ do
         a <- initVar "a" [4] "init"
         b <- initVarWithDefault "b" [4]
         return (a, b)  
  c <- save (a + b)
  d <- return (c + c)
  e <- get "a"
  return (e + d)

do_test = putStrLn (compile test)
