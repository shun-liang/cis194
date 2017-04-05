type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 pegA pegB pegC = [(pegA, pegC)]
hanoi n pegA pegB pegC = hanoi (n - 1) pegA pegC pegB ++ [(pegA, pegC)] ++ hanoi (n - 1) pegB pegA pegC
