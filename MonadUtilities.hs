{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module MonadUtilities where

import Control.Monad.State.Lazy
import Data.Tuple

repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM i f x = if i==0 
                then pure x
                else f x >>= repeatM (i-1) f

--(a -> b -> (a, c)) -> a -> t b -> (a, t c)
{-
mapAccumLM :: Monad m => (a -> b -> m (a,c)) -> a -> [b] -> m (a, [c])
mapAccumLM f start li = case li of
                     [] -> return ([], start)
                     h:rest -> do
                             (a, c) <- f start h
                             (a', cs) <- mapAccumLM f a rest
                             return (a', c:cs)
-}
-- (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
-- ((a,[c]) -> b -> m (a,[c])) -> (a,[c]) -> t a -> m (a, [c])
{-
mapAccumLM f start li = foldlM (\(a, cs) b -> 
                                do
                                  (a', c) <- f a b
                                  return (a', cs++[c])) --inefficient?
                               (start, [])
-}
mapAccumLM :: (Traversable t, Monad m) => (a -> b -> m (a,c)) -> a -> t b -> m (a, t c)
mapAccumLM f start li = fmap swap $ 
                        runStateT 
                        (mapM (\b -> 
                                  do
                                    a <- get
                                    (a', c) <- lift $ f a b
                                    put a'
                                    return c) -- :: b -> StateT a m c
                         li) start
