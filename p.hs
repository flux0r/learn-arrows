{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (repeat)
import Control.Exception (Exception, IOException, SomeException, onException, throwIO, try)
import Data.Typeable (Typeable)

main = undefined

data Proc f o = forall i.
                Await (f i) (i -> Proc f o) (Proc f o) (Proc f o)
              | Emit [o] (Proc f o)
              | Halt

emitAll :: [o] -> Proc f o -> Proc f o
emitAll os (Emit os' p) = Emit (os ++ os') p
emitAll os p            = Emit os p

emit :: o -> Proc f o -> Proc f o
emit o p = emitAll [o] p

await :: f i -> Proc f o
await x = Await x (const Halt) Halt Halt

(+-+) :: Proc f o -> Proc f o -> Proc f o
Halt +-+ p'                 = p'
(Emit os p) +-+ p'          = emitAll os (p +-+ p')
(Await r k fb end) +-+ p'   = Await r ((+-+ p') . k) (fb +-+ p') end

(<$>) :: (o' -> o) -> Proc f o' -> Proc f o
f <$> Halt                  = Halt
f <$> (Emit os p)           = Emit (fmap f os) (f <$> p)
f <$> (Await r k fb end)    = Await r ((f <$>) . k) (f <$> fb) (f <$> end)

unit :: o -> Proc f o
unit = flip emit Halt

join :: Proc f (Proc f o) -> Proc f o
join Halt                   = Halt
join (Emit (p:ps) p')       = p +-+ (join $ Emit ps p')
join (Emit [] p')           = join p'
join (Await req k fb end)   = Await req (join . k) (join fb) (join end)

bind :: Proc f o' -> (o' -> Proc f o) -> Proc f o
p `bind` f = join $ f <$> p

repeat :: Proc f o -> Proc f o
repeat p = let p' = iter p in p'
  where
    iter x = case x of
        Halt                    -> iter p
        (Await req k fb end)    -> Await req (iter . k) fb end
        (Emit os x')            -> emitAll os (iter x')

class Partial f where
    attempt :: Exception e => f a -> f (Either e a)
    failure :: Exception e => e -> f a

data End = End deriving (Show, Typeable, Eq)
instance Exception End

collect :: (Monad f, Partial f) => Proc f o -> f [o]
collect p = let iter' = iter p [] in iter'
  where
    iter cur acc = case cur of
        (Emit os p)         -> iter p (acc ++ os)
        Halt                -> return acc
        (Await r k p end)   -> attempt r >>= \ att -> case att of
            (Left e)
                | e == End      -> iter p acc
                | otherwise     -> iter (end +-+ await (failure e)) acc
            (Right a)           -> iter (k a) acc
