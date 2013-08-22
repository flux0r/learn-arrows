import qualified Prelude as P
import Prelude hiding (tail)

data Process i o = Emit [o] (Process i o)
                 | Await (i -> Process i o) (Process i o)
                 | Halt

(<<-) :: Process i o -> Process o o' -> Process i o'
this <<- p2 = case p2 of
    Halt            -> Halt
    Emit h t        -> Emit h (this <<- t)
    Await f fb      -> case this of
        Emit h t        -> iterP h t f fb
        Halt            -> Halt <<- fb
        Await g gb      -> Await (\ i -> g i <<- p2) (gb <<- fb)

iterP emit tail f fb
    | null emit         = tail <<- Await f fb
    | otherwise         = case f (head emit) of
        (Await f2 fb2)  -> iterP (P.tail emit) tail f2 fb2
        p               -> (Emit emit tail) <<- p

(<<|) :: Process o' o -> Process i o' -> Process i o
Halt            <<| _               = Halt
(Emit os p2)    <<| p1              = Emit os (p2 <<| p1)
(Await f p2)    <<| (Emit os' p1)   = iter os' p1 f p2
  where
    iter os p' f p
        | null os       = Await f p <<| p'
        | otherwise     = case f (head os) of
            (Await f' p2')  -> iter (P.tail os) p f' p2'
            p               -> Emit os p'

apply :: Process i o -> [i] -> [o]
apply Halt          _       = []
apply (Await f p)   (x:xs)  = apply (f x) xs
apply (Await _ p)   s       = apply p s
apply (Emit os p)   s       = os ++ apply p s

emitAll :: [o] -> Process i o -> Process i o
emitAll os (Emit os' p)     = Emit (os ++ os') p
emitAll os p                = Emit os p

emit :: o -> Process i o
emit o = emitAll [o] Halt

appendP :: Process i o -> Process i o -> Process i o
appendP Halt        p   = p
appendP (Emit os p) p'  = emitAll os (appendP p p')
appendP (Await f p) p'  = Await (flip appendP p' . f) (appendP p p')

mapP :: (o' -> o) -> Process i o' -> Process i o
mapP _  Halt        = Halt
mapP f (Emit os p)  = emitAll (map f os) (mapP f p)
mapP f (Await g p)  = Await (mapP f . g) (mapP f p)

joinP :: Process i (Process i o) -> Process i o
joinP Halt               = Halt
joinP (Emit (o:os) p')   = appendP o (joinP (Emit os p'))
joinP (Await f p)        = Await (joinP . f) (joinP p)


-- apP p a
-- bindP p (flip mapP a)
-- bindP p (flip mapP a)
-- bindP Halt (flip mapP a)         = Halt
-- bindP (Emit [] p') (flip mapP a) = bindP p' (flip mapP a)
-- bindP (Emit (o':os') p') (flip mapP a) =
--          appendP (flip mapP a $ o') (bindP (emitAll os' p') (flip mapP a)

-- bindP :: Process i o' -> (o' -> Process i o) -> Process i o
-- bindP Halt                  _   = Halt
-- bindP (Emit [] p')          f   = bindP p' f
-- bindP (Emit (o':os') p')    f   = appendP (f o') (bindP (emitAll os' p') f)
-- 
-- instance Functor (Process i) where
--     fmap = mapP
-- 
-- instance Monad (Process i) where
--     return  = emit
--     (>>=)   = bindP
