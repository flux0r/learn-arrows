{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative ((<$>))
import Data.Monoid
import Debug.Trace
import Control.Exception (onException)
import System.IO (IOMode (ReadMode, WriteMode, AppendMode), openFile, hClose,
                  hIsEOF, hGetLine, hPutStrLn, readFile)

infixl 6 <<-
infixl 5 <||

main = do
    putStrLn (show $ apply (zipP idP idP) [3.1, 1.4, 9.3])

data Process i o = Emit [o] (Process i o)
                | Await (i -> Process i o) (Process i o)
                | Halt

apply :: Process i o -> [i] -> [o]
apply Halt              _       = []
apply (Emit os p)       xs      = os ++ apply p xs
apply (Await receive p) (x:xs)  = apply (receive x) xs
apply (Await _ p)       []      = apply Halt []


emitAll :: [o] -> Process i o -> Process i o
emitAll os (Emit os' p) = Emit (os ++ os') p
emitAll os p            = Emit os p

mapP :: (a -> b) -> Process i a -> Process i b
mapP f Halt                 = Halt
mapP f (Emit os p)          = emitAll (f <$> os) (mapP f p)
mapP f (Await receive p)    = Await (mapP f . receive) (mapP f p)

instance Functor (Process i) where
    fmap = mapP

emit :: a -> Process i a -> Process i a
emit o p = emitAll [o] p

combine :: Process i o -> Process i o -> Process i o
Halt                `combine` p = p
(Emit os' p')       `combine` p = emitAll os' (p' `combine` p)
(Await receive p')  `combine` p =
        Await (flip combine p . receive) (p' `combine` p)

bind :: Process i o' -> (o' -> Process i o) -> Process i o
Halt                `bind`  _ = Halt
(Emit [] p)         `bind`  f = p `bind` f
(Emit (o:os) p)     `bind`  f = (f o) `combine` (emitAll os p `bind` f)
(Await receive p)   `bind`  f = Await (flip bind f . receive) (p `bind` f)

unit :: o -> Process i o
unit = flip emit Halt

repeatP :: Process i o -> Process i o
repeatP p = iter p
 where
   iter Halt            = iter p
   iter (Await f p')    = Await (iter . f) p'
   iter (Emit xs p')    = Emit xs (iter p')

(<<-) :: Process o' o -> Process i o' -> Process i o
Halt                <<- _   = Halt
(Emit os0 p0)       <<- p'  = emitAll os0 (p0 <<- p')
pp@(Await f0 p0)    <<- p'  = case p' of
       (Emit os1 p1)   -> iter f0 p0 os1 p1
       Halt            -> p0 <<- Halt
       (Await g0 p1)   -> Await (\ i -> pp <<- g0 i) (p0 <<- p1)
  where
    iter f p [] p'      = Await f p <<- p'
    iter f p (x:xs) p'  = case f x of
       (Await f' pp)       -> iter f' pp xs p'
       pp                  -> pp <<- (Emit xs p')

idP :: Process o o
idP = lift id

lift :: (i -> o) -> Process i o
lift f = Await (\ i -> emitAll [f i] (lift f)) Halt

instance Monad (Process i) where
    return = unit
    (>>=)  = bind

filterP :: (i -> Bool) -> Process i i
filterP pred = repeatP $ Await guard (filterP pred)
  where
    guard i | pred i    = emit i Halt
            | otherwise = Halt

sumP :: Process Double Double
sumP = iter 0.0
  where
    iter x = Await
        (\ y ->
            let total = x + y
            in  emit total (iter total)) 
        Halt

takeP :: (Num a, Ord a) => a -> Process b b
takeP n
    | n <= 0    = Halt
    | otherwise = Await (\ x -> emit x (takeP $ n - 1)) Halt

dropP :: (Num a, Ord a) => a -> Process b b
dropP n
    | n <= 0    = idP
    | otherwise = Await (\ x -> dropP (n - 1)) Halt

takeWhileP :: (a -> Bool) -> Process a a
takeWhileP f = Await guard (takeWhileP f)
  where
    guard x
        | f x       = emit x $ takeWhileP f
        | otherwise = Halt

echo :: Process o o
echo = Await (\ i -> emit i Halt) Halt

dropWhileP :: (a -> Bool) -> Process a a
dropWhileP f = repeatP $ Await guard (dropWhileP f)
  where
    guard x
        | f x       = dropWhileP f
        | otherwise = idP

countP :: Process i Integer
countP = lift floor <<- sumP <<- lift (const 1)

meanP :: Process Double Double
meanP = iter 0.0 0.0
  where
    iter summ n =
        Await (\ x ->
                let summ' = summ + x
                    n' = n + 1
                in  emit ((summ + x)/(n + 1)) $ iter summ' n')
              Halt

iter :: a -> (a -> i -> (o, a)) -> Process i o
iter acc f =
    Await (\ i -> let (o, acc') = f acc i
                in  emit o (iter acc' f))
          Halt

sumIter :: Process Double Double
sumIter = iter 0.0 (\ x y -> (x + y, x + y))

countIter :: Process i Integer
countIter = iter 1 (\ acc i -> (acc, acc + 1))

zipRemainder :: (a -> b -> c) -> [a] -> [b] -> ([c], ([a], [b]))
zipRemainder f [] []            = ([], ([], []))
zipRemainder f xs []            = ([], (xs, []))
zipRemainder f [] ys            = ([], ([], ys))
zipRemainder f (x:xs) (y:ys)    =
    let r = zipRemainder f xs ys
    in  ((x `f` y):(fst r), (fst . snd $ r, snd . snd $ r))

zipP :: Process a b -> Process a c -> Process a (b, c)
zipP (Emit bs p1) (Await g p2) =
    Await (\ i -> zipP (Emit bs p1) (g i))
          (zipP p1 p2)
zipP (Await f p1) (Emit cs p2) =
    Await (\ i -> zipP (f i) (Emit cs p2))
          (zipP p1 p2)
zipP _ Halt = Halt
zipP Halt _ = Halt
zipP (Emit bs p1) (Emit cs p2) = Emit bc (zipP p1' p2')
  where
    (bc, (rb, rc)) = zipRemainder (,) bs cs
    mkP [] p = p
    mkP xs p = Emit xs p
    p1' = mkP rb p1
    p2' = mkP rc p2
zipP (Await f p1) (Await g p2) =
    Await (\ i -> (zipP (f i) (g i)))
          (zipP p1 p2)

zippedMean :: Process Double Double
zippedMean = let div = lift $ uncurry (/) in
    div <<- zipP sumP (lift fromIntegral <<- countP)

zipWithIndex :: Process i o -> Process i (o, Integer)
zipWithIndex p = zipP p countP

exists :: (a -> Bool) -> Process a Bool
exists f = iter False (\ acc x -> (acc || x, acc || x)) <<- lift f

class Source s where
    (<||) :: Process i o -> s i -> s o

mapS :: Source s => (i -> o) -> s i -> s o
f `mapS` s = lift f <|| s

filterS :: Source s => (a -> Bool) -> s a -> s a
filterS f s = filterP f <|| s

data ResourceR o = forall r i.
    ResourceR (IO r) (r -> IO ()) (r -> IO (Maybe i)) (Process i o)

instance Source ResourceR where
    p <|| (ResourceR f g h p') = ResourceR f g h (p <<- p')

iterS :: IO [o] -> IO (Maybe i) -> Process i o -> IO () -> IO [o]
iterS acc k p end = doit
  where
    doit = case p of
        Halt            -> end >> acc
        (Emit xs p')    ->
                let newacc = onException (acc >>= return . (++ xs)) end
                in  iterS newacc k p' end
        (Await f p')    -> onException k end >>= check
          where
            check Nothing   = iterS acc (return Nothing) p' end
            check (Just i)  = iterS acc k (f i) end

collect :: ResourceR o -> IO [o]
collect (ResourceR acquire release step p) = acquire >>= doit
  where
    doit r = iterS (return []) (step r) p (release r)

linesS :: FilePath -> ResourceR String
linesS fn = doit
  where
    doit = ResourceR (openFile fn ReadMode) hClose getL idP
    getL h = do
        end <- hIsEOF h
        if end then return Nothing else hGetLine h >>= return . Just

gt40k :: FilePath -> ResourceR Bool
gt40k fn = exists (> 40000) <<- countP <|| linesS fn

class Sink s where
    (||>) :: s i' -> Process i i' -> s i

data ResourceW i = forall r i'.
    ResourceW (IO r) (r -> IO ()) (r -> (i' -> IO ())) (Process i i')

instance Sink ResourceW where
    (ResourceW f g h p) ||> p' = ResourceW f g h (p <<- p')

file :: FilePath -> Bool -> ResourceW String
file fn append = ResourceW h hClose (\ h -> (\s -> hPutStrLn h s)) idP
  where
    h = openFile fn (if append then AppendMode else WriteMode)
