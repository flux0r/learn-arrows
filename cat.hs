{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Prelude as P
import Prelude (($))
import Data.Category
import Data.Category.Functor

data Mealy i o = Mealy (i -> (o, Mealy i o))

instance Category Mealy where
    src m = Mealy (\i -> (i, src m))
    tgt m = Mealy (\o -> (o, tgt m))
    (Mealy f) . (Mealy g) = Mealy $ \x ->
        let (y, m)  = g x
            (z, m') = f y
        in  (z, m' . m)

instance Functor (Mealy i o) where
    type Dom (Mealy i o)    = (->)
    type Cod (Mealy i o)    = (->)
