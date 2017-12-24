{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -Wall #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

--copied this part :(
--Learn 'zipWith'.
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons t s) = t : streamToList s

instance Show a => Show (Stream a) where
    show a = foldr (\x acc -> (show x) ++ "," ++ acc) "..." (take 20 (streamToList a))

streamRepeat :: a -> Stream a
streamRepeat d = Cons d (streamRepeat d)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

getTrailing0 :: Integer -> Integer -> Integer
getTrailing0 0 c = c
getTrailing0 n c = if mod n 2==0 then getTrailing0 new (c+1) else c
    where
        new = floor (fromIntegral n/2)

ruler :: Stream Integer
ruler = streamMap (\x -> if (x `mod` 2)==1 then 0 else getTrailing0 x 0) natsNot0
    where
        Cons 0 natsNot0 = nats

{-[a b;c d]-}
data Matrix = Matrix {a :: Integer, b :: Integer, c :: Integer, d :: Integer}

{-[a b;c d] * [e f;g h]-}
instance Num Matrix where
    (*) first second = Matrix {a = (j*e + k*g),b = (j*f + k*h),c = (l*e + m*g),d = (l*f + m*h)}
        where j = a first
              k = b first
              l = c first
              m = d first
              e = a second
              f = b second
              g = c second
              h = d second

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = c fn
    where
        fn = f ^ n
            where
                f = Matrix {a = 1,b = 1,c = 1,d = 0}
