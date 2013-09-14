data List a = Nil | Cons a (List a)

wrap :: a -> List a
wrap x = Cons x Nil

nil :: List a -> Bool
nil []  = True
nil _   = False

foldList :: (a -> b -> b) -> b -> List a -> b
foldList _ e []             = e
foldList f e (Cons x xs)    = f x (foldList f e xs)

-- Universal property of foldList.
--
--              h       == foldList f e
--      <==>    h xs    == case xs of
--                             Nil          -> e
--                             Cons y ys    -> f y (h ys) 
