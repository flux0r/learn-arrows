{-# LANGUAGE NoImplicitPrelude, TypeOperators, TypeFamilies,
             FlexibleContexts, UndecidableInstances, GADTs #-}

import qualified Prelude as P

main = P.undefined

type Ob c a = c a a

class Category c where
    src :: c x y -> Ob c x
    tgt :: c x y -> Ob c y
    (.) :: c y z -> c x y -> c x z

class (Category (Dom f), Category (Cod f)) => Functor f where
    type Dom f  :: * -> * -> *
    type Cod f  :: * -> * -> *
    type f % a  :: *
    (<$>)       :: f -> Dom f a b -> Cod f (f % a) (f % b)

data Id (c :: * -> * -> *) = Id

instance Category c => Functor (Id c) where
    type Dom (Id c) = c
    type Cod (Id c) = c
    type Id c % a   = a
    _ <$> f         = f

data g :.: h where
    (:.:) :: (Functor g, Functor h, Cod h ~ Dom g) => g -> h -> g :.: h

instance (Category (Cod g), Category (Dom h)) => Functor (g :.: h) where
  type Dom (g :.: h)    = Dom h
  type Cod (g :.: h)    = Cod g
  type (g :.: h) % a    = g % (h % a)
  (g :.: h) <$> f = g <$> (h <$> f)

data COb :: (* -> * -> *) -> *

data Cat :: * -> * -> * where
    CMor    :: (Functor f, Category (Dom f), Category (Cod f))
            => f
            -> Cat (COb (Dom f)) (COb (Cod f))

instance Category Cat where
    src (CMor _)        = CMor Id
    tgt (CMor _)        = CMor Id
    CMor c . CMor c'    = CMor (c :.: c')

data X :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
    X :: c a b -> c' a' b' -> X c c' (a, a') (b, b')

instance (Category c, Category c') => Category (c `X` c') where
    src (c `X` c')  = src c `X` src c'
    tgt (c `X` c')  = tgt c `X` tgt c'
    (c `X` c') . (k `X` k') = (c . k) `X` (c' . k')

class Category c => HasBinaryProducts c where
    type BinaryProduct (c :: * -> * -> *) x1 x2 :: *
    p1 :: Ob c x1 -> Ob c x2 -> c (BinaryProduct c x1 x2) x1
    p2 :: Ob c x1 -> Ob c x2 -> c (BinaryProduct c x1 x2) x2
    (&&&) :: c y x1 -> c y x2 -> c y (BinaryProduct c x1 x2)
    (***) :: c x1 y1
          -> c x2 y2
          -> (c (BinaryProduct c x1 x2) (BinaryProduct c y1 y2))
    l *** r = (l . p1 (src l) (src r)) &&& (r . p2 (src l) (src r))
