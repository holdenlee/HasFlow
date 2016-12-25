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

{-
mapAccumLM :: Monad m => (a -> b -> m (a,c)) -> a -> [b] -> m (a, [c])
mapAccumLM f start li = case li of
                     [] -> return ([], start)
                     h:rest -> do
                             (a, c) <- f start h
                             (a', cs) <- mapAccumLM f a rest
                             return (a', c:cs)
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
