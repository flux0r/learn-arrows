import Prelude hiding (Either(..))

------------------------------------------------------------------------------
-- | Notes and code from Paterson's "A new notation for arrows" and Hughes'
-- "Generalising monads to arrows".


------------------------------------------------------------------------------
-- | Some helper functions.

(-*-) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(f -*- g) (x, y) = (f x, g y)

ass :: ((a, b), c) -> (a, (b, c))
ass ((x, y), z) = (x, (y, z))

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


------------------------------------------------------------------------------
-- | An arrow type is a binary type constructor with three pieces of data:
-- arr, (>>>), and first, which satisfy the following laws.
--
-- Composition is associative.
--  (f  >>> g)  >>> h   = f >>> (g  >>> h)
--
--  Extensionality.
--  arr id  >>> f       = f
--  f   >>> arr id      = f
--
--  An arrow is pure if it has no side-effects; i.e., it is equal to arr f for
--  some f. Combinators for pure arrows should behave as they do for
--  functions.
--  first   (arr f)     = arr (f -*- id)
--
--  Composition preserving.
--  first   (f >>> g)   = first f >>> first g
--
--  The first function should only depend on first components of pairs.
--  first f >>> arr fst = arr fst >>> f
--
--  Not too sure about the intuition for this one...
--  first f >>> arr (id -*- g)  = arr (id -*- g) >>> first f
-- 
--  Or these two...
--  arr (g . f)                 = arr f >>> arr g
--  first (first f) >> arr ass  = arr ass >>> first f

class Arrow a where
    arr     :: (b -> c) -> a b c
    (>>>)   :: a b c -> a c d -> a b d
    first   :: a b c -> a (b, d) (c, d)


------------------------------------------------------------------------------
-- | Some arrows can define a combinator that invokes an arrow it receives as
-- input. For the regular function (->) arrow, this just means higher-order
-- functions. The laws for this new class are the following.
--
-- In terms of regular functions, currying and then applying the identity
-- is the same as the identity on pairs.
--  first (arr (\x -> arr (\y -> (x, y)))) >>> app = arr id
--
--  Operations can move in or out of the applied arrow.
--  first (arr (g >>>)) >>> app = second g >>> app
--  first (arr (>>> h)) >>> app = app >>> h

class Arrow a => ArrowApply a where
    app :: a (a b c, b) c


------------------------------------------------------------------------------
-- | Some arrows can define a combinator that can choose between two arrows
-- based on the input.

-- The input should be of type Either and the class has some laws.
--
-- left (arr f)     = arr (left f)
-- left (f >>> g)   = left f >>> left g
-- arr Left >>> left f      = f >>> arr Left
-- right (arr g) >>> left f = left f >>> right (arr g)

data Either a b = Left a | Right b

class Arrow a => ArrowChoice a where
    left :: a b c -> a (Either b d) (Either c d)


------------------------------------------------------------------------------
-- | Some useful functions for arrow types. (***) does not necessarily
-- preserve composition because of effects.

second :: (Arrow a) => a b c -> a (d, b) (d, c)
second f = arr swap >>> first f >>> arr swap

(***) :: (Arrow a) => a b c -> a b' c' -> a (b, b') (c, c')
f *** g = first f >>> second g

(&&&) :: (Arrow a) => a b c -> a b c' -> a b (c, c')
f &&& g = (arr $ \x -> (x, x)) >>> (f *** g)

right :: (ArrowChoice a) => a b c -> a (Either d b) (Either d c)
right f = arr mirror >>> left f >>> arr mirror
  where
    mirror (Left x)     = Right x
    mirror (Right x)    = Left x

(+++) ::ArrowChoice a
      => a b c 
      -> a b' c' 
      -> a (Either b b') (Either c c')
f +++ g = left f >>> right g

(|||) :: ArrowChoice a => a b c -> a d c -> a (Either b d) c
f ||| g = f +++ g >>> arr un
  where
    un (Left x)     = x
    un (Right x)    = x


newtype Kleisli m a b = K (a -> m b)

instance Monad m => Arrow (Kleisli m) where
    arr f       = K $ return . f
    K f >>> K g = K $ \x -> f x >>= g
    first (K f) = K $ \(x, y) -> f x >>= \z -> return (z, y)

first' :: (Monad m) => Kleisli m a b -> Kleisli m (a, c) (b, c)
first' (K f) = K $ \(x, y) -> do
    z <- f x
    return (z, y)

instance Arrow (->) where
    arr f = f
    f >>> g = g . f
    first f = f -*- id
