{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XLambdaCase
 -XPolyKinds
#-}

module Utilities where
import System.Environment
import Control.Monad
import Data.Tree
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Hashable
import Data.Either
import Data.Maybe
import Debug.Trace
import Prelude -- necessary for Hint.

-- * Function combinators

infixr 0 `c2`
c2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
c2 f g x y = f $ g x y
--or: c2 f g = (f .) . (g .)
--or (point-free): (.).(.)

infixr 0 `c3`
c3 :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
c3 f g x y z = f $ g x y z
--or: c3 f g = ((f .) .) . ((g .) .)
--or (point-free): (.).(.).(.)

doIf :: Bool -> (a -> a) -> (a -> a)
doIf p f = (\x -> if p then f x else x)

doIfElse :: Bool -> (a -> a) -> (a -> a) -> (a -> a)
doIfElse p f g = (\x -> if p then f x else g x)

doIfElseCond :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> a
doIfElseCond p f g x = if p x then f x else g x

tryWithDefault::(a->Maybe b) -> b -> a -> b
tryWithDefault f def x = 
  case (f x) of
    Nothing -> def
    Just y -> y

ifelselist:: [(Bool, a)] -> a -> a
ifelselist li y = 
  case li of 
    [] -> y
    ((b,x):li2) -> if b then x else ifelselist li2 y

iflist :: [(Bool, a)] -> a
iflist li = 
  case li of 
    ((b,x):li2) -> if b then x else iflist li2

-- * Loops/folds

tryDo :: (a -> Maybe a) -> a -> a
tryDo f x = case f x of
             Nothing -> x
             Just y -> y

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = 
  if (p x) then while p f (f x) else x

loopUntil :: (a -> Bool) -> (a -> a) -> a -> a    
loopUntil p f x = 
  if (p x) then x else loopUntil p f (f x)

loopUntilFail :: (a -> Maybe a) -> a -> a
loopUntilFail f x = 
  case (f x) of 
    Just y -> loopUntilFail f y
    Nothing -> x

stopBefore :: (a -> Bool) -> (a -> a) -> a -> a    
stopBefore p f x = 
  if (p (f x)) then x else stopBefore p f (f x)

repeatTimes :: Int -> (a-> a) -> a -> a
repeatTimes n f x = 
    if n <=0 then x else repeatTimes (n-1) f (f x)
--repeatTimes n f = (foldl (.) id $ replicate n f)

foldIterate:: (a -> c -> c) -> [a] -> c -> c
foldIterate f as x = foldl (flip f) x as

foldIterate2:: (a -> b -> c -> c) -> [a] -> [b] -> c -> c
foldIterate2 f as bs x = foldl (\y -> \(xa, xb) -> f xa xb y) x (zip as bs)

for' :: [a] -> b -> (a -> b -> b) -> b
for' li x0 f = foldl (flip f) x0 li

-- * Function composition

infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixr 0 <<|
(<<|) :: (b -> c) -> (a -> b) -> (a -> c)
(<<|) = (.)

infixl 0 |>>
(|>>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>>) = flip (.)

-- * Tuples 

third :: (a,b,c) -> c
third (_,_,z) = z
--or: ^. _3

appendFun :: (a -> b) -> a -> (a,b)
appendFun f x = (x, f x)

prependFun :: (a -> b) -> a -> (b,a)
prependFun f x = (f x, x)

-- * Maps and Lists

mapReduce :: (Ord b) => (a -> (b,c)) -> (b -> [c] -> [d]) -> [a] -> [d]
mapReduce mf rf = concat . M.elems . M.mapWithKey rf . foldl 
                  (\mp a -> 
                            let 
                                (b,c) = mf a
                            in
                              M.alter (\case 
                                        Nothing -> Just [c]
                                        Just x -> Just (c:x)) b mp) M.empty

removeAt :: Int -> [a] -> [a]
removeAt n li = part1++(tail part2) where (part1,part2) = splitAt n li

insertAt :: Int -> a -> [a] -> [a]
insertAt n x li = part1++[x]++part2 where (part1,part2) = splitAt n li

isInitialSegment :: Eq a => [a] -> [a] -> Bool
isInitialSegment = isJust `c2` stripPrefix

insertMultiple :: (Ord a) => [(a, b)] -> M.Map a b -> M.Map a b 
insertMultiple li h = foldl (\hm -> (\(x,y) -> M.insert x y hm)) h li

listUpdateFun :: Int -> (a-> a) -> [a] -> [a]
listUpdateFun n f li = listUpdate n (f (li !! n)) li

listUpdatesFun :: (Int -> Bool) -> (a-> a) -> [a] -> [a]
listUpdatesFun p f li = map (\(i,x) -> doIf (p i) f x) (zip [1..] li)

replaceSublist :: Int -> Int -> [a] -> [a] -> [a]
replaceSublist m n li li2 =
      let 
        front = take m li2
        back = drop (max m n) li2
      in
        front ++ li ++ back

listUpdate :: Int -> a -> [a] -> [a]
listUpdate n x li = replaceSublist n (n+1) [x] li

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f li = case li of 
                   h:rest -> case f h of
                               Nothing -> mapFilter f rest
                               Just x -> x : mapFilter f rest
                   _ -> []

filterZip:: (b->Bool) -> [a] -> [b] -> [(a,b)]
filterZip p as bs = filter (\(x,y) -> p y) (zip as bs)

cofilter :: (b->Bool) -> [a] -> [b] -> ([a],[b])
cofilter p as bs = unzip (filterZip p as bs)

lookupList:: (Ord a) => [a] -> M.Map a b -> [Maybe b]
lookupList as mp = fmap (flip M.lookup mp) as

--unsafe
lookupList2:: (Ord a) => [a] -> M.Map a b -> [b]
lookupList2 as mp = fmap ((M.!) mp) as

--I'm surprised this doesn't exist.
mlookup :: Int -> [a] -> Maybe a
mlookup n li = if 0<=n && n<(length li) then Just (li!!n) else Nothing

mindex :: [a] -> Int -> Maybe a
mindex = flip mlookup
--or use indexed lens 

--NOTE: if you're doing this a lot, use array instead
sublist :: Int -> Int -> [a] -> [a]
sublist m n ls =  take (n-m) . drop m $ ls
-- >=m, <n

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 = zipWith

enumerate :: [a] -> [(Int, a)]
enumerate li = zip [1..] li

zenumerate :: [a] -> [(Int, a)]
zenumerate li = zip [0..] li

emap :: ((Int,a)->b) -> [a] -> [b]
emap f li = map f (enumerate li)

zemap :: ((Int,a)->b) -> [a] -> [b]
zemap f li = map f (zenumerate li)

keepi :: (Int -> Bool) -> [a] -> [a]
keepi f li = map snd (filter (f.fst) (enumerate li))

freqTable :: (Ord a) => [a] -> [(a, Int)]
freqTable li = map (\li -> (li!!0, length li)) $ group $ sort li

-- * Other

--unsafe
maybeLeft :: Either a b -> Maybe a
maybeLeft x = case x of
              Left y -> Just y
              Right _ -> Nothing

maybeRight :: Either a b -> Maybe b
maybeRight x = case x of
              Left _ -> Nothing
              Right y -> Just y

fromLeft = fromJust . maybeLeft

fromRight = fromJust . maybeRight

-- * Debugging

-- debug x y = x

debug = flip trace

debugShow x = trace (show x) x

debugSummary x f = trace (show (f x)) x

-- * Numbers

lclamp :: (Ord a) => a -> a -> a
lclamp lo x = if x < lo then lo else x

rclamp :: (Ord a) => a -> a -> a
rclamp hi x = if x > hi then hi else x

clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi x 
    | x > hi    = hi
    | x < lo    = lo
    | otherwise = x
