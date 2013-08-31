import Control.Applicative ((<$>))
import Data.Monoid

infixl 8 <<-

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
Halt                <<- _   = Halt <<- Halt
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

dropWhileP :: (a -> Bool) -> Process a a
dropWhileP f = repeatP $ Await guard (dropWhileP f)
  where
    guard x
        | f x       = dropWhileP f
        | otherwise = idP

countP :: Process i Integer
countP = lift (toInteger . floor) <<- sumP <<- lift (const 1.0)

meanP = iter 0.0 0.0
  where
    iter summ n = Await
        (\ x -> let summ' = summ + x
                    n' = n + 1
                in  emit ((summ + x)/(n + 1)) $ iter summ' n')
        Halt

iter :: a -> (a -> i -> (o, a)) -> Process i o
iter acc f = Await
    (\ i -> let (o, acc') = f acc i
            in  emit o (iter acc' f))
    Halt

sumIter :: Process Double Double
sumIter = iter 0.0 (\ x y -> (x + y, x + y))

countIter = iter 0 (\ acc i -> (acc, acc + 1))
