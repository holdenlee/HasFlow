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

import Prelude hiding ((+), (*), (-), tanh)
import Algebra.Additive as Additive
import Algebra.Ring hiding (product)
import Control.Monad
import Control.Monad.Free
import Control.Lens
import Data.Maybe
import Text.Printf
import qualified Data.Set as S
import Data.List
import qualified Data.Map as M
import Data.Functor
import Control.Applicative

import MonadUtilities
import Expr
import Tensor
import Graph
import Functions
import Compiler
import Args

test :: Flow T
test = do
  setDefaultInits "default"
  (a,b) <- scope "foo" $ do
         a <- initVar "a" (4::Int) "init"
         b <- initVarWithDefault "b" (4::Int)
         return (a, b)  
  c <- save (a * b)
  d <- return (c + c)
  e <- get "a"
  return (e + d)

do_test = putStrLn (compile test)

lstm_step :: T -> (T,T) -> T -> T -> T -> T -> T -> T -> T -> T -> T -> T -> Flow (T, (T,T))
lstm_step x mem wf bf wi bi wC bC wo bo wo1 bo1 = do
    let (c, h) = mem
    hx <- save $ concatenate 1 [h,x]
    f <- save $ sigmoid (hx * wf + bf)
    i <- save $ sigmoid (hx * wi + bi)
    c_add <- save $ tanh (hx * wC + bC)
    c1 <- save $ (f .* c + i .* c_add)
    o <- save $ sigmoid(hx * wo + bo)
    h1 <- save $ o * (tanh c1)
    out <- save $ softmax (h1 * wo1 + bo1)
    return (out, (c1, h1))

scanM :: Monad m => (c -> a -> m (b,c)) -> c -> [a] -> m [b]
scanM f start li = case li of
                     [] -> return []
                     h:rest -> do
                             (b, c) <- f start h
                             r <- scanM f c rest
                             return (b:r)

scanlM :: (c -> T -> Flow (T,c)) -> c -> T -> Int -> Flow [T]
scanlM f start li n = scanM (\c i -> 
                                 do 
                                   let t = li .! i
                                   f c t) start [1..n]
    

lstm xs ys batches l m n = do
    setDefaultInits "tf.truncated_normal_initializer(stddev=1e-2)"
    [wf, wi, wc, wo] <- mapM (\x -> initVarWithDefault x [m+n, m]) ["wf", "wi", "wC", "wo"]
    wo1 <- initVarWithDefault "wo1" [m,n]
    [bf, bi, bc, bo] <- mapM (\x -> initVarWithDefault x m) ["bf", "bi", "bC", "bo"]
    bo1 <- initVarWithDefault "bo1" n
    let c = zeros [batches,m] 
    let h = zeros [batches,m]
    return wf
{-
    (outs, end) = scan(lambda mem, x: step_lstm1(x, mem, Wf, bf, Wi, bi, WC, bC, Wo, bo, Wo1, bo1), 
                       (C,h), xs, l)
-}

{-
def lstm_fs_(xs, ys, batches, l, m, n):
    #(name, shape=None, initializer=None,dtype=tf.float32, var_type="variable")
    [Wf, Wi, WC, Wo] = map(lambda name: variable_on_cpu(name, shape=[m+n,m], initializer=tf.truncated_normal_initializer(stddev=1e-2)), ["Wf", "Wi", "WC", "Wo"])
    Wo1 = variable_on_cpu( "Wo1", shape=[m, n], initializer=tf.truncated_normal_initializer(stddev=1e-2))
    [bf, bi, bC, bo] = map(lambda name: variable_on_cpu(name, shape=[m], initializer=tf.truncated_normal_initializer(stddev=1e-2)), ["bf", "bi", "bC", "bo"])
    bo1 = variable_on_cpu( "bo1", shape=[n], initializer=tf.truncated_normal_initializer(stddev=1e-2))
    # C = variable_on_cpu("C", shape=[m], var_type="variable")
    # h = variable_on_cpu("h", shape=[m], var_type="variable")
    #C = tf.ones([batches,m])
    C = tf.zeros([batches,m])
    #h = tf.zeros([m])
    #h = tf.ones([batches,m])
    h = tf.zeros([batches,m])
    (outs, end) = scan(lambda mem, x: step_lstm1(x, mem, Wf, bf, Wi, bi, WC, bC, Wo, bo, Wo1, bo1), 
                       (C,h), xs, l)
    yhats = tf.pack(outs)
    #print(ys)
    #print(yhats)
    loss = cross_entropy(ys, yhats,t=1e-6)
    #tf.nn.sparse_softmax_cross_entropy_with_logits(outs, yhats, name='xentropy')
    #loss = cross_entropy(outs, yhats)
    #is not actually accuracy
    accuracy = cross_entropy(ys[-1], yhats[-1])
    #tf.nn.sparse_softmax_cross_entropy_with_logits(outs[-1], yhats[-1])
    return {"loss": loss, "inference": yhats, "accuracy": accuracy}
-}
