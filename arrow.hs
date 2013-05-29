class Arrow a where
    arr     :: (b -> c) -> a b c
    (>>>)   :: a b c -> a c d -> a b d
    first   :: a b c -> a (b, d) (c, d)

(-*-) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(f -*- g) (x, y) = (f x, g y)

ass :: ((a, b), c) -> (a, (b, c))
ass ((x, y), z) = (x, (y, z))

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

second :: (Arrow a) => a b c -> a (d, b) (d, c)
second f = arr swap >>> first f >>> arr swap

(***) :: (Arrow a) => a b c -> a b' c' -> a (b, b') (c, c')
f *** g = first f >>> second g

(&&&) :: (Arrow a) => a b c -> a b c' -> a b (c, c')
f &&& g = (arr $ \x -> (x, x)) >>> (f *** g)

instance Arrow (->) where
    arr f = f
    f >>> g = g . f
    first f = f -*- id
