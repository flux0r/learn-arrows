{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

import Prelude hiding (repeat, zip, zipWith)
import Control.Exception (Exception, IOException, SomeException, onException, throwIO, try)
import Data.Typeable (Typeable)
import System.IO (openFile, IOMode (ReadMode, WriteMode, AppendMode), hClose,
                  hIsEOF, hGetLine, hPutStr)
import Control.Exception (throw)

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
repeat p = iter p
  where
    iter x = case x of
        Halt                    -> iter p
        (Await req k fb end)    -> Await req (iter . k) fb end
        (Emit os x')            -> emitAll os (iter x')

class Partial f where
    attempt :: Exception e => f a -> f (Either e a)
    failure :: Exception e => e -> f a

instance Partial IO where
    attempt = try
    failure = throwIO

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

resource :: f a -> (a -> f b) -> (a -> f o) -> Proc f o
resource acq rel k = Await acq (\ r -> iter (k r) (rel r)) Halt Halt
  where
    iter k end = Await k
                       (\ o -> emit o (iter k end))
                       (await end)
                       (await end)

linesP :: FilePath -> Proc IO String
linesP fn = resource (openFile fn ReadMode) (\ h -> hClose h) (\ h -> getL h)
  where
    getL h = hIsEOF h >>= \ end ->
        if end then throw End else hGetLine h

kill :: Proc f o' -> Proc f o
kill Halt               = Halt
kill (Await _ _ _ c)    = drain c
kill (Emit os p)        = kill p

drain :: Proc f o' -> Proc f o
drain Halt              = Halt
drain (Emit os p)       = drain p
drain (Await r k p c)   = Await r (drain . k) (drain p) (drain c)

data Is a b where
    Refl :: Is a a

instance Eq (Is a b) where
    Refl == Refl = True

type Proc1 i o = Proc (Is i) o

await1 :: (i -> Proc1 i o) -> Proc1 i o -> Proc1 i o
await1 k p = Await Refl k p Halt

halt1 :: Proc1 i o
halt1 = Halt

emit1 :: o -> Proc1 i o -> Proc1 i o
emit1 x p = emit x p

emitAll1 :: [o] -> Proc1 i o -> Proc1 i o
emitAll1 xs p = emitAll xs p

lift :: (i -> o) -> Proc1 i o
lift f = await1 (flip emit Halt . f) Halt

filter1P :: (o -> Bool) -> Proc1 o o
filter1P f = await1 (\ i -> if f i then emit i Halt else halt1) Halt

(<<-) :: Proc1 o' o -> Proc i o' -> Proc i o
Halt <<- _                      = Halt
(Emit os0 p0) <<- p'            = emitAll os0 (p0 <<- p')
pp@(Await Refl k0 p0 c0) <<- p' = case p' of
    Halt                -> p0 <<- Halt
    (Emit os p)         -> feed k0 p0 c0 os p
    (Await r1 k1 p1 c1) -> Await r1
                                 (\ i -> pp <<- k1 i)
                                 (p0 <<- p1)
                                 (c0 <<- c1)
  where
    feed k p c [] t     = await1 k p <<- t
    feed k p c (x:xs) t = case k x of
        (Await Refl k' p' c')   -> feed k' p' c' xs t
        p                       -> p <<- emitAll xs t

filterP :: (o -> Bool) -> Proc i o -> Proc i o
filterP f = (filter1P f <<-)

data T a b c where
    L :: T a b a
    R :: T a b b

type Tee i i' o = Proc (T i i') o

haltT :: Tee i i' o
haltT = Halt

emitT :: o -> Tee i i' o -> Tee i i' o
emitT = emit

awaitL :: (i -> Tee i i' o) -> (Tee i i' o) -> (Tee i i' o)
awaitL k t = Await L k t Halt

awaitR :: (i' -> Tee i i' o) -> (Tee i i' o) -> (Tee i i' o)
awaitR k t = Await R k t Halt

zipWithP :: (i -> i' -> o) -> Tee i i' o
zipWithP f = repeat $ awaitL (\ i ->
    awaitR (\ i' ->
       emit (f i i') Halt) Halt) Halt

zipP :: Tee i i' (i, i')
zipP = zipWithP (,)

passL :: Tee i i' i
passL = awaitL (flip emitT passL) Halt

passR :: Tee i i' i'
passR = awaitR (flip emitT passR) Halt

awaitLOr :: Tee i i' o -> (i -> Tee i i' o) -> Tee i i' o
awaitLOr = flip awaitL

awaitROr :: Tee i i' o -> (i' -> Tee i i' o) -> Tee i i' o
awaitROr = flip awaitR

zipWithAll :: i -> i' -> (i -> i' -> o) -> Tee i i' o
zipWithAll x y f =
    let pR = (\ r -> f x r) <$> passR
        pL = (\ l -> f l y) <$> passL
    in  repeat $ awaitLOr pR (\ i ->
            awaitROr pL (\ i' ->
            emitT (f i i') Halt))

tee :: Proc f a -> Proc f b -> Tee a b c -> Proc f c 
tee p p' Halt               = kill p +-+ kill p' +-+ Halt
tee p p' (Emit xs t)        = Emit xs (tee p p' t)
tee p p' t@(Await L k t' c) = case p of
    Halt                -> kill p' +-+ Halt
    (Await rL kL tL cL) -> Await rL
                                 ((\ x -> tee x p' t) . kL)
                                 (tee tL p' t)
                                 (tee cL p' t)
    (Emit xs xp)        -> feedL xs xp p' k t' c
tee p p' t@(Await R kR pR cR) = case p' of
    Halt                    -> kill p +-+ Halt
    (Await rR kR' pR' cR')  -> Await rR
                                     ((\ x -> tee p x t) . kR')
                                     (tee p pR' t)
                                     (tee p cR' t)
    (Emit ys yp)            -> feedR ys yp p kR pR cR

feedL :: [a]
      -> Proc f a
      -> Proc f b
      -> (a -> Proc (T a b) c)
      -> Proc (T a b) c
      -> Proc (T a b) c
      -> Proc f c
feedL [] tail other recv fb c = tee tail other $ Await L recv fb c
feedL (x:xs) tail other recv fb c = let t2 = recv x in case t2 of
    Await L recv2 fb2 c2    -> feedL xs tail other recv2 fb2 c2
    Await _ _ _ _           -> tee (Emit xs tail) other t2
    p                       -> tee (Emit xs tail) other p

feedR :: [a]
      -> Proc f a
      -> Proc f b
      -> (a -> Proc (T b a) c)
      -> Proc (T b a) c
      -> Proc (T b a) c
      -> Proc f c
feedR [] tail other recv fb c = tee other tail $ Await R recv fb c
feedR (x:xs) tail other recv fb c = let t2 = recv x in case t2 of
    Await R recv2 fb2 c2    -> feedR xs tail other recv2 fb2 c2
    Await _ _ _ _           -> tee other (Emit xs tail) t2
    p                       -> tee other (Emit xs tail) p

type Sink f o = Proc f ()

fileW :: String -> FilePath -> Bool -> Sink IO String
fileW s fn append =
    let acq = openFile fn (if append then AppendMode else WriteMode)
    in  resource acq hClose (\ h -> hPutStr h s)

runP :: Proc f (f o) -> Proc f o
runP Halt               = Halt
runP (Await r k p c)    = Await r (runP . k) (runP p) (runP c)
runP (Emit [] p)        = runP p
runP (Emit (x:xs) p)    = Await x (\ o -> emit o (runP $ emitAll xs p)) Halt Halt

zipWith p p' f = tee p p' (zipWithP f)

to p snk = runP (zipWith p snk (\(o, f) -> f o))

through p p' = runP $ zipWith p p' (\ (o, f) -> f o)

disconnectIn Halt = Halt
disconnectIn (Emit xs p) = Emit xs (disconnectIn p)
