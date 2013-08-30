data Moore i o = Moore o (i -> Moore i o)

data Mealy i o = Mealy (i -> (o, Mealy i o))

-- (<<-) :: Moore o' o -> Moore i o' -> Moore i o
-- (Moore o o'o) <<- (Moore o' io') = Moore o (\ i -> o'o o' <<- io' i)

(<<-) :: Mealy o' o -> Mealy i o' -> Mealy i o
Mealy f <<- Mealy g = Mealy $ \ i ->
    case g i of
    (o, g') -> case f o of
        (i', f') -> (i', f' <<- g')
