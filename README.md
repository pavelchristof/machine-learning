# machine-learning
The library provides building blocks out of which complex machine learning models can be built. These
building blocks are for example:
* Linear functions
* Affine functions
* Monoid homomorphisms
* Scalar functions, like 'sin' or 'exp'
* Softmax function
* Catamorphisms, which are a generalization of folds. In the context of machine learning these are
  a generalization of recursive neural networks.

Once you compose a model and specify a cost function you can train it using provided algorithms 
(currently adaGrad). The library takes care of computing the gradient using a technique known as 
automatic differentiation.

The library is fully type safe, including the dimensions of vectors and matrices.

Look at https://github.com/pawel-n/cml for a more complete implementation in Scala.

Example model
=============

The following model implements simple classification using a neural network with single hidden layer,
with sigmoid activation function:

```haskell
type Features = V 10 -- We represent features as a 10 element vector.
type Classes = V 4 -- There are 4 possible classes.

type ExampleModel
    = AffineMap Features (V 50)
  :>> Over (V 50) Sigmoid
  :>> AffineMap (V 50) Classes
  :>> Over Classes Sigmoid

-- The cost function 
--     J(e, a) = - (e * log a + (1 - e) * log (1 - a))
-- with regularization.
cost :: Cost ExampleModel
cost = logistic + 0.01 * l2reg
```
