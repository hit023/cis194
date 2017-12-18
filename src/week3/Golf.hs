{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (intercalate)

isDiv :: Int -> Int -> Bool
isDiv p q = mod q p == 0

skipsHelp :: [a] -> Int -> [a]
skipsHelp ls n = map snd (filter (isDiv n.fst) (zip [1..] ls))

skipsAux :: [a] -> Int -> [[a]]
skipsAux _ 0 = []
skipsAux ls n = skipsAux ls (n-1) ++ [skipsHelp ls n]

skips :: [a] -> [[a]]
skips ls = skipsAux ls (length ls)

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_ , _] = []
localMaxima [_] = []
localMaxima (l : c : r : xs) = if c>r && c>l then c : localMaxima (c : r : xs) else localMaxima (c : r : xs)

collectCounts :: [Integer] -> [Integer]
collectCounts ls = map toInteger (map (\x -> length (filter (==x) ls)) [0..9])

fun :: [Integer] -> Integer -> String
fun ls n = map (\x -> if x>=n then '*' else ' ') ls

plot :: [Integer] -> String
plot ls = intercalate "\n" (map (\x -> fun ls x) [(maximum ls),(maximum ls - 1)..1]) ++ "\n==========\n0123456789\n"

histogram :: [Integer] -> String
histogram ls = plot (collectCounts ls)
