toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (first : []) = [first]
doubleEveryOther (first : second : xs) = first : (2 * second) : doubleEveryOther xs

sumDigitsOfANum :: Integer -> Integer
sumDigitsOfANum 0 = 0
sumDigitsOfANum n
    | n < 0 = 0
    | otherwise = (sumDigitsOfANum (n `div` 10)) + (n `mod` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sumDigitsOfANum x + sumDigits xs
