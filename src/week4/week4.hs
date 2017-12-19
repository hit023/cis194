{-# OPTIONS_GHC -Wall #-}

fun1' :: [Integer] -> Integer
fun1' ls = product (map (\x -> x-2) (filter even ls))

fun2' :: Integer -> Integer
fun2' n = sum (filter even (takeWhile (>1) (iterate (\x -> if mod x 2==0 then x `div` 2 else (3 * x)+1) n)))

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show,Eq,Ord)

getHeight :: Tree a -> Integer
getHeight Leaf = 0
getHeight (Node n _ _ _) = n

foldTree :: (Ord a) => [a] -> Tree a
foldTree ls = foldr insert Leaf ls
    where
        insert x Leaf = Node 0 Leaf x Leaf
        insert x (Node n l a r)
            | l > r = Node (getHeight r + 1) l a (insert x r)
            | otherwise = Node (getHeight l + 1) (insert x l) a r

isTrue :: Bool -> Integer
isTrue b = if b then 1 else 0

xor :: [Bool] -> Bool
xor ls = mod (foldr (+) 0 (map isTrue ls)) 2 == 1

map' :: (a->b) -> [a] -> [b]
map' f = foldr fun []
    where
        fun x acc = f x : acc

--Redo the below; nifty use of flip!
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

seiveSundaram :: Integer -> [Integer]
seiveSundaram n = [(2 * x) + 1 | x <- [1..n], let l = getNon n, x `notElem` l]

getNon :: Integer -> [Integer]
getNon n = [i+j+2*(i*j) | i <- [1..n], j <- [1..n], i <=j, i+j+2*(i*j) <= n]
