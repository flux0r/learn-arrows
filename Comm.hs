{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Category
import Prelude hiding (id, (.))

newtype Comm0 i o = Comm0 {
    nextComm0 :: i -> (o, Comm0 i o)
}

unit :: Comm0 o o
unit = Comm0 $ \x -> (x, unit)

(+--+) :: Comm0 i' o -> Comm0 i i' -> Comm0 i o
c1' +--+ c2' = Comm0 $ \z ->
    let (y, c2) = nextComm0 c2' z
        (x, c1) = nextComm0 c1' y
    in  (x, c1 +--+ c2)

instance Category Comm0 where
    id  = unit
    (.) = (+--+)

testComm0Next x' c =
    let (x, c') = nextComm0 c $ x'
    in      print x
        >>  putStrLn "Press enter to continue"
        >>  getLine
        >>  testComm0Next x' c'

addOne :: Num a => Comm0 a a
addOne = Comm0 $ \x -> (x + 1, addOne)

countAt :: Num a => a -> Comm0 a a
countAt x = Comm0 $ \dx -> (x, countAt $ x + dx)


------------------------------------------------------------------------------
-- (>\\)
--  :: (Monad m)
--  => (b' -> p a' a x' x m b)
--  ->        p b' b x' x m c
--  ->        p a' a x' x m c
--
--  (\>\)
--  :: (Monad m, Proxy p)
--  => (b' -> p a' a x' x m b)
--  -> (c' -> p b' b x' x m c)
--  -> (c' -> p a' a x' x m c)

data Comm1 i o =
    More (i -> Comm1 i o)
  | End o (Comm1 i o)

comb :: Comm1 o' o -> Comm1 i o' -> Comm1 i o
comb (More f) (More g)  = More $ \i -> comb (More f) (g i)
comb (More f) (End x c) = comb (f x) c
comb (End x c1) c0      = End x (comb c1 c0)
