{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow

main = undefined

data Mealy i o = Mealy (i -> (o, Mealy i o))

(|.|) :: Mealy o' o -> Mealy i o' -> Mealy i o
(Mealy f) |.| (Mealy g) = Mealy $ \ i ->
    let (o', m) = g i
        (o, m') = f o'
    in  (o, m' |.| m)

instance Category Mealy where
    id  = Mealy (\ i -> (i, id))
    (.) = (|.|)

liftM :: (i -> o) -> Mealy i o
liftM f = Mealy $ \ i -> (f i, liftM f)

firstM :: Mealy i o -> Mealy (i, a) (o, a)
firstM (Mealy k) = Mealy $ \ (i, a) ->
    let (o, m) = k i
    in  ((o, a), firstM m)

instance Arrow Mealy where
    arr = liftM
    first = firstM

apply :: Mealy i o -> [i] -> [o]
apply _ []      = []
apply (Mealy k) (x:xs) = let (o, m) = k x in
    [o] ++ apply m xs
