{-# LANGUAGE RankNTypes, TypeOperators #-}

import System.IO
import Control.Monad
import Control.Exception

------------------------------------------------------------------------------
-- Source: "Interleaving data and effects"                                  --
--                                                                          -- 
--         ROBERT ATKEY, PATRICIA JOHANN, NEIL GHANI                        -- 
--         University of Strathclyde, Glasgow, G1 1XH, UK                   --
--                                                                          --
--         BART JACOBS Radboud                                              --
--         University, Nijmegen, Netherlands                                --
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- Introduction                                                             --
------------------------------------------------------------------------------


-- Using the initial f-algebra methodology with data types like the following
-- is difficult because they mix together the pure and effectful parts of the
-- code. This is most obvious in the type signature of ``fileListIO''.

data List'IO = NilIO | ConsIO Char ListIO
newtype ListIO = ListIO (IO List'IO)

fileListIO :: Handle -> ListIO
fileListIO h = ListIO $ iter h (ListIO (return NilIO))
  where
    iter h acc@(ListIO cs) = hIsEOF h >>= check cs acc
    check cs acc e =
        if e
            then cs
            else hGetChar h >>= \c -> return $ ConsIO c acc


-- This data type explicitly mentions effects between the elements of the list
-- and describes lists of values interleaved with effects.
data List' m a = NilM | ConsM a (List m a)
newtype List m a = List (m (List' m a))


-- This function gives back a value in the IO monad so it's possible to
-- explicitly close the handle.
hContents :: Handle -> List IO Char
hContents h = List $ hIsEOF h >>= check
  where
    check e = if e then return NilM else hGetChar h >>= step
    step c  = return $ ConsM c (hContents h)


-- This is an example of an Iteratee-like data structure that describes
-- functions that alternatively read from input and perform effects in a
-- monad, eventually yielding output.

data Reader' m a b = Input (Maybe a -> Reader m a b) | Yield b
newtype Reader m a b = Reader (m (Reader' m a b))



------------------------------------------------------------------------------
-- Background: Initial F-algebras                                           --
------------------------------------------------------------------------------


-- A functor is a pair (f, fmapf) where f is a type operator and fmapf is a
-- function
--
--          fmapf: (a -> b) -> f a -> f b
-- such that
--
--               fmapf id = id,
--          fmapf (g . h) = fmapf g . fmapf h .


-- (List m, fmapList)
fmapList :: Monad m => (a -> b) -> List m a -> List m b
fmapList f (List fs) = List $ fs >>= step
  where
    step (ConsM x xs)   = return $ ConsM (f x) (fmapList f xs)
    step _              = return NilM


-- (Reader m a, fmapReader)
fmapReader :: Monad m => (b -> c) -> Reader m a b -> Reader m a c
fmapReader f (Reader fs) = Reader $ fs >>= step
  where
    step (Yield b) = return $ Yield (f b)
    step (Input k) = return $ Input (fmapReader f . k)


-- An F-algebra is a pair (a, fAlga) of a carrier type a and a structure map
--
--          fAlga: f a -> a .
--
-- An F-algebra homomorphism between two F-algebras (a, fAlga) and (b, fAlgb)
-- is a function
--
--          h: a -> b, h . fAlga = fAlgb . fmapf h .
--
-- An initial F-algebra is an F-algebra (m f, construct) such that for any
-- F-algebra (a, fAlga), there exists a unique F-algebra homomorphism
--
--          hfAlga: m f -> a .
--
-- Proof principle 1 (Initial F-algebras).  If (m f, construct) is an initial
-- F-algebra, (a, fAlga) is an F-algebra, and g: m f -> a is a function, then
-- to prove
--
--          hfAlga = g,
--
-- it suffices to show that g is an F-algebra homomorphism:
--
--          g . construct = fAlga . fmapf g.


-- An F-algebra is a functor f, a carrier type a, and a function f a -> a.
-- We'll just call the function an Algebra.
type Algebra f a = f a -> a


-- The carrier of an initial F-algebra over a functor f.
newtype Mu f = In { unIn :: f (Mu f) }


-- The F-algebra structure map is the constructor for Mu.
construct :: f (Mu f) -> Mu f
construct = In


-- The F-algebra homomorphisms out of Mu f are defined in terms of the functor
-- structure fmap and recursion.
fHom :: Functor f => Algebra f a -> Mu f -> a
fHom alg = alg . fmap (fHom alg) . unIn


-- The functor type for an initial F-algebra.
data ListF r a = NilF | ConsF r a

fmapListF :: (a -> b) -> ListF r a -> ListF r b
fmapListF _ NilF        = NilF
fmapListF f (ConsF r x) = ConsF r (f x)

instance Functor (ListF r) where
    fmap = fmapListF


-- The initial algebra maps into all algebras defined over a given functor.
-- Its carrier type is the fix point of the functor f.
type ListFInit r = Algebra (ListF r) (Mu (ListF r))


-- An algebra over (ListF r) with the carrier [a]: regular Haskell lists.
algToList :: Algebra (ListF a) [a]
algToList NilF          = []
algToList (ConsF r a)   = (r:a)


-- An algebra over (ListF r) with the carrier Int.
algSum :: Algebra (ListF Int) Int
algSum NilF             = 0
algSum (ConsF x acc)    = x + acc



------------------------------------------------------------------------------
--  List append I: pure lists                                               --
------------------------------------------------------------------------------


append :: Mu (ListF r) -> Mu (ListF r) -> Mu (ListF r)
append xs ys = fHom alg xs
  where
    alg NilF            = ys
    alg (ConsF a xs)    = construct (ConsF a xs)


-- We can prove associativity of append in the following manner. From the
-- definition of append:
--
--              append (construct NilF) ys
--          =   append (In NilF) ys
--          =   ys,
--
--  and
--
--              append (construct (ConsF a xs)) ys
--          =   construct (ConsF a (append xs ys)).
--
-- Theorem. For all xs, ys, zs :: Mu (ListF a),
--  
--          append xs (append ys zs) = append (append xs ys) zs .
--
-- Proof. If we let g = \xs.append (append xs ys) zs then,
--
--                fAlg NilF = append ys zs,
--        fAlg (ConsF a xs) = construct (ConsF a xs) .
--
-- We need to prove that for all x :: ListF a (Mu (ListF a)),
--
--      append (append (construct x) ys) zs =
--              fAlg (fmap (\xs.append (append xs ys) zs) x) .
--
-- If x = NilF,
--
--          append (append (construct NilF) ys) zs
--      =   append ys zs
--      =   fAlg NilF
--      =   fAlg (fmap g NilF)
--      =   fAlg (fmap (\xs.append (append xs ys) zs) NilF).
--
-- If x = ConsF a xs,
--
--          append (append (construct (ConsF a xs)) ys) zs
--      =   append (construct (ConsF a (append xs ys))) zs
--      =   construct (ConsF a (append (append xs ys) zs))
--      =   fAlg (ConsF a (append (append xs ys) zs))
--      =   fAlg (fmap g (ConsF a xs)
--      =   fAlg (fmap (\xs.append (append xs ys) zs) (Cons a xs)).



------------------------------------------------------------------------------
-- Background: monadic effects and monadic recursion schemes                --
------------------------------------------------------------------------------


-- A monad is a quadruple (m, fmapm, returnm, joinm) of a type constructor m
-- and three functions:
--
--          fmapm :: (a -> b) -> m a -> m b
--        returnm :: a -> m a
--          joinm :: m (m a) -> m a
--
-- such that the pair (m, fmapm) is a functor and the following properties are
-- satisfied:
--
--              joinm . returnm = id
--        joinm . fmapm returnm = id
--          joinm . fmapm joinm = joinm . joinm,
--
-- along with the following naturality laws:
--
--                  returnm . f = fmapm f . returnm
--      joinm . fmapm (fmapm f) = fmapm f . joinm .
--
--  If (m1, fmapm1, returnm1, joinm1) and (m2, fmapm2, returnm2, joinm2) are
--  monads, a monad morphism between them is a function
--
--              h :: m1 a -> m2 a
--
--  such that
--
--                 h . fmapm1 g = fmapm2 g . h
--                 h . returnm1 = returnm2
--                   h . joinm1 = joinm2 . h . fmapm1 h



------------------------------------------------------------------------------
-- Separating data and effects with F-and-M-algebras                        --
------------------------------------------------------------------------------


-- Given a monad (m, fmapm, returnm, joinm), an M-Eilenberg-Moore-algebra
-- consists of a pair (a, mAlga) of a type a and a function
--
--          mAlga :: m a -> a
--
-- such that
--
--          mAlga . returnm = id
--            mAlga . joinm = mAlga . fmapm mAlga .
--
-- The m-Eilenberg-Moore-algebra (a, mAlga) represents a way of performing the
-- effects of the monad m in the type a while preserving the returnm and joinm
-- operations of the monad structure.

data ErrorM a = Ok a | Error String

fmapErrM :: (a -> b) -> ErrorM a -> ErrorM b
fmapErrM f (Ok a)       = Ok (f a)
fmapErrM _ (Error e)    = Error e

returnErrM :: a -> ErrorM a
returnErrM x = Ok x

joinErrM :: ErrorM (ErrorM a) -> ErrorM a
joinErrM (Ok (Ok x))    = Ok x
joinErrM (Ok (Error e)) = Error e
joinErrM (Error e)      = Error e

mAlgErrM :: ErrorM (IO a) -> IO a
mAlgErrM (Ok r)     = r
mAlgErrM (Error e)  = throw (ErrorCall e)
