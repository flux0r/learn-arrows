{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Category
import Prelude hiding (id, (.))

-- data Message i o =
--     Empty
--   | Msg o i
-- 
-- data CommNext i a o =
--     More (i -> Comm a i o) (a (Message i o))
--   | Message i o
-- 
-- newtype Comm a i o = Comm {
--     unComm :: a (CommNext i a o)
-- }

newtype Comm i o = Comm {
    nextComm :: i -> (o, Comm i o)
}

unit :: Comm o o
unit = Comm $ \x -> (x, unit)

(+--+) :: Comm i' o -> Comm i i' -> Comm i o
c1' +--+ c2' = Comm $ \z ->
    let (y, c2) = nextComm c2' z
        (x, c1) = nextComm c1' y
    in  (x, c1 +--+ c2)

instance Category Comm where
    id  = unit
    (.) = (+--+)

testCommNext x' c =
    let (x, c') = nextComm c $ x'
    in      print x
        >>  putStrLn "Press enter to continue"
        >>  getLine
        >>  testCommNext x' c'

addOne :: Num a => Comm a a
addOne = Comm $ \x -> (x + 1, addOne)

countAt :: Num a => a -> Comm a a
countAt x = Comm $ \dx -> (x, countAt $ x + dx)
