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
import Polynomial
import Shape
import Tensor
import Graph
import Functions
import Compiler
import Args
import Control.Monad.Writer.Lazy

-- Basic operations
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

do_test = putStrLn (compile_ test)

-- Basic layers
sigmoid_f :: T -> T -> T -> Flow T
sigmoid_f a b x = save $ sigmoid (x * a + b)

sigmoid_layer :: Polynomial -> Polynomial -> T -> Flow T
sigmoid_layer m n x = do
  a <- initVarWithDefault "A" [m,n]
  b <- initVarWithDefault "b" [n]
  sigmoid_f a b x

multilayer_code :: Flow T
multilayer_code = do
  setDefaultInits "tf.truncated_normal_initializer(stddev=1e-2)"
  let b = pref "b_dim"
  let n = pref "n_dim"
  x <- initPH "x" [b, n] --initialize placeholder
  stacks "sigmoid_layer" 2 (sigmoid_layer n n) x

multilayer_test = putStrLn $ compile_ multilayer_code

multilayer_test2 = putStrLn $ compileWithShapes multilayer_code 

lstm_step :: T -> T -> T -> T -> T -> T -> T -> T -> T -> T -> (T, T) -> T -> Flow ((T,T), T)
lstm_step wf bf wi bi wC bC wo bo wo1 bo1 mem x = do
    let (c, h) = mem
    hx <- save $ concatenate 1 [h,x]
    f <- save $ sigmoid (hx * wf + bf)
    i <- save $ sigmoid (hx * wi + bi)
    c_add <- save $ tanh (hx * wC + bC)
    c1 <- save $ (f .* c + i .* c_add)
    o <- save $ sigmoid(hx * wo + bo)
    h1 <- save $ o .* (tanh c1)
    out <- save $ softmax (h1 * wo1 + bo1)
    return ((c1, h1), out)

mapAccumLT :: (c -> T -> Flow (c,T)) -> c -> T -> Int -> Flow (c, [T])
mapAccumLT f start li n = mapAccumLM (\c i -> 
                                 do 
                                   let t = li .! i
                                   f c t) start [1..n]

lstm_code = do
              let l = 2::Int
              let batches = 1::Int
              let m = 4
              let n = 5
              xs <- initPH "xs" [l, batches, n]
              ys <- initPH "ys" [l, batches, n]
              lstm xs ys batches l m n

lstm_test = putStrLn $ compile_ lstm_code

lstm_test2 = do
  let (ans, log) = runWriter (compile lstm_code)
  putStrLn (unlines log)

lstm xs ys batches l m n = do
    setDefaultInits "tf.truncated_normal_initializer(stddev=1e-2)"
    [wf, wi, wC, wo] <- mapM (\x -> initVarWithDefault x [m+n, m]) ["wf", "wi", "wC", "wo"]
    wo1 <- initVarWithDefault "wo1" [m,n]
    [bf, bi, bC, bo] <- mapM (\x -> initVarWithDefault x m) ["bf", "bi", "bC", "bo"]
    bo1 <- initVarWithDefault "bo1" n
    let c = zeros (toShape [batches,m]) 
    let h = zeros (toShape [batches,m])
    (end, outs) <- mapAccumLT (lstm_step wf bf wi bi wC bC wo bo wo1 bo1) (c,h) xs l
    return (pack outs)



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
