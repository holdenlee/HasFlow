# HasFlow

Write Tensorflow code in a modular fashion. Example in `Test.hs`:

```
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
```

Compile the code using `compile_`. Use `compileWithShapes` to check and output the dimensions of the tensors.

```
multilayer_test = putStrLn $ compile_ multilayer_code

multilayer_test2 = putStrLn $ compileWithShapes multilayer_code 
```

Running: 

```
$ multilayer_test

_a = get_variable("x", [b_dim,n_dim], var_type="placeholder")
with tf.variable_scope("sigmoid_layer1"):
    _b = get_variable("A", [n_dim,n_dim], tf.truncated_normal_initializer(stddev=1e-2))
    _c = get_variable("b", [n_dim], tf.truncated_normal_initializer(stddev=1e-2))
    _d = tf.sigmoid(((_a * _b) + _c))
with tf.variable_scope("sigmoid_layer2"):
    _e = get_variable("A", [n_dim,n_dim], tf.truncated_normal_initializer(stddev=1e-2))
    _f = get_variable("b", [n_dim], tf.truncated_normal_initializer(stddev=1e-2))
    _g = tf.sigmoid(((_d * _e) + _f))
_h = _g
```

Inferring the shapes:

```
$ multilayer_test2 

# x : [b_dim,n_dim]
_a = get_variable("x", [b_dim,n_dim], var_type="placeholder")
with tf.variable_scope("sigmoid_layer1"):
# A : [n_dim,n_dim]
    _b = get_variable("A", [n_dim,n_dim], tf.truncated_normal_initializer(stddev=1e-2))
# b : [n_dim]
    _c = get_variable("b", [n_dim], tf.truncated_normal_initializer(stddev=1e-2))
# _a : [b_dim,n_dim]
# _b : [n_dim,n_dim]
# (_a * _b) : [b_dim,n_dim]
# _c : [n_dim]
# ((_a * _b) + _c) : [b_dim,n_dim]
# tf.sigmoid(((_a * _b) + _c)) : [b_dim,n_dim]
    _d = tf.sigmoid(((_a * _b) + _c))
with tf.variable_scope("sigmoid_layer2"):
# A : [n_dim,n_dim]
    _e = get_variable("A", [n_dim,n_dim], tf.truncated_normal_initializer(stddev=1e-2))
# b : [n_dim]
    _f = get_variable("b", [n_dim], tf.truncated_normal_initializer(stddev=1e-2))
# _d : [b_dim,n_dim]
# _e : [n_dim,n_dim]
# (_d * _e) : [b_dim,n_dim]
# _f : [n_dim]
# ((_d * _e) + _f) : [b_dim,n_dim]
# tf.sigmoid(((_d * _e) + _f)) : [b_dim,n_dim]
    _g = tf.sigmoid(((_d * _e) + _f))
```

# TODO

*   Shape computation
	*   Check broadcasting behavior.
	*   What if shape depend on arguments?
*   Higher-order functions on tensors
	*   Tricky because, ex. type signature is something like `(c -> T -> Flow (c, T)) -> c -> T -> Flow (c, [T])`. To do shape-checking we need to constrain `c`. Idea: Add a type class `MadeOfT` containing nested tuples of `T`'s.
*   Add more functions!
*   Instance declarations right now require type annotations like `1::Int`. How to get around this?
*   Python foreign function interface
