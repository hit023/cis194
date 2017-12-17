{-# OPTIONS_GHC -Wall #-}
--Validate Credit Card Number
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (quot (fromIntegral n) 10)

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (quot (fromIntegral n) 10) ++ [n `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x
    | null x = []
    | length x `mod` 2==1 = head x : doubleEveryOther (tail x)
    | length x `mod` 2==0 = 2 * head x : doubleEveryOther (tail x)

sumDigits :: [Integer] -> Integer
sumDigits l
    | null l = 0
    | head l > 10 = ((head l) `mod` 10) + quot (head l) 10 + sumDigits (tail l)
    | otherwise = head l + sumDigits (tail l)

validate :: Integer -> Bool
validate n = (mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0


--Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 1 = [(a, b)]
    | otherwise = (hanoi (toInteger (fromIntegral n)-1) a c b) ++ [(a, b)] ++ (hanoi (toInteger (fromIntegral n)-1) c b a)
