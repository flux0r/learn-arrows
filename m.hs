{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Prelude as P
import Prelude (($))

infixr 8 .

type Obj p a = p a a

class Category p where
    src :: p a b -> Obj p a
    tgt :: p a b -> Obj p b
    (.) :: p b c -> p a b -> p a c

instance Category (->) where
    src _ = \ x -> x
    tgt _ = \ x -> x
    f . g = \ x -> f $ g x

data Dual p a b = Dual { unDual :: p b a }

instance Category p => Category (Dual p) where
    src = Dual . tgt . unDual
    tgt = Dual . src . unDual
    p . q = Dual (unDual q . unDual p)

data Mealy i o = Mealy (i -> (o, Mealy i o))

(|.|) :: Mealy o' o -> Mealy i o' -> Mealy i o
(Mealy f) |.| (Mealy g) = Mealy $ \ i ->
    let (o', m) = g i
        (o, m') = f o'
    in  (o, m' |.| m)

instance Category Mealy where
    src m = Mealy $ \ i -> (i, src m)
    tgt m = Mealy $ \ o -> (o, tgt m)
    (.) = (|.|)

data Prod :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
    Prod :: p a b -> q c d -> Prod p q (a, c) (b, d)

instance (Category p, Category q) => Category (Prod p q) where
    src (Prod a a')             = Prod (src a) (src a')
    tgt (Prod a a')             = Prod (tgt a) (tgt a')
    (Prod x x') . (Prod y y')   = Prod (x . y) (x' . y')

class (Category (Dom f), Category (Cod f)) => Functor f where
    type Dom f      :: * -> * -> *
    type Cod f      :: * -> * -> *
    type f :%: a    :: *
    (%)             :: f -> Dom f a b -> Cod f (f :%: a) (f :%: b)
