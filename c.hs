{-# LANGUAGE MultiParamTypeClasses #-}

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow

main = undefined

newtype M i o = M { runM :: i -> (o, M i o) }

instance Category M where
    id      = M $ \ i -> (i, id)
    f . g   = M $ \ i -> let (o', m') = runM g i
                             (o, m) = runM f o'
                         in  (o, m . m')

data S k i o r = Halt
             | Emit o r
             | Await (i -> r) (k i) r

instance Functor (S k i o) where
    fmap f Halt             = Halt
    fmap f (Emit o r)       = Emit o (f r)
    fmap f (Await k o r)    = Await (f . k) o (f r)

instance Arrow M where
    arr f = let m = M $ \ i -> (f i, m) in m
    first (M k) = M $ \ (x, y) ->
        let (o, m) = k x
        in  ((o, y), first m)
