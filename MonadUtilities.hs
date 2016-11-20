{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module MonadUtilities where

repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM i f x = if i==0 
                then pure x
                else f x >>= repeatM (i-1) f
