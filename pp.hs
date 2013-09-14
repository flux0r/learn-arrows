{-# LANGUAGE ExistentialQuantification #-}

data Step f o r = Halt
                | Emit o r
                | forall i. Await (f i) (i -> r) r r
data M i o = M
    { res   :: o
    , next  :: i -> M i o
    }
newtype M' i o = M' { runM' :: i -> (o, M' i o) }

fmapS :: (r' -> r) -> Step f o r' -> Step f o r
fmapS _ Halt            = Halt
fmapS f (Emit os s)     = Emit os (f s)
fmapS f (Await r k s c) = Await r (f . k) (f s) (f c)

fmapM :: (o' -> o) -> M i o' -> M i o
fmapM f (M o k)     = M (f o) (fmapM f . k)

fmapM' :: (o' -> o) -> M' i o' -> M' i o
fmapM' f (M' k) = M' k'
  where k' = \i -> let (o, m) = k i in (f o, fmapM' f m)
