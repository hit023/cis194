{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Maybe
import ExprT
import Parser
import qualified StackVM as S
--use newtype when you want to declare different type instances of one type. Like here, we do for
--Integer.
newtype MinMax = MinMax Integer deriving (Eq,Show)
newtype Mod7 = Mod7 Integer deriving (Eq,Show)

eval :: ExprT -> Integer
eval ex = case ex of
    Lit n -> n
    Add ex1 ex2 -> (eval ex1) + (eval ex2)
    Mul ex1 ex2 -> (eval ex1) * (eval ex2)

evalStr :: String -> Maybe Integer
evalStr str = case p of
    Nothing -> Nothing
    Just x -> Just (eval x)
    where
        p = parseExp Lit Add Mul str

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit n = n
    add ex1 ex2 = ex1 + ex2
    mul ex1 ex2 = ex1 * ex2

instance Expr Bool where
    lit n = n>0
    add ex1 ex2 = ex1 || ex2
    mul ex1 ex2 = ex1 && ex2

instance Expr MinMax where
    lit = MinMax
    add f@(MinMax a) s@(MinMax b) = if a>=b then f else s
    mul f@(MinMax a) s@(MinMax b) = if a>=b then s else f

instance Expr Mod7 where
    lit n = Mod7 (mod n 7)
    add (Mod7 a) (Mod7 b) = lit (mod (a + b) 7)
    mul (Mod7 a) (Mod7 b) = lit (mod (a * b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr S.Program where
    lit n = [(S.PushI n)]
    add ex1 ex2 = ex1 ++ ex2 ++ [S.Add]
    mul ex1 ex2 = ex1 ++ ex2 ++ [S.Mul]

check :: String -> Either String S.StackVal
check = (S.stackVM).fromMaybe [].compile

compile :: String -> Maybe S.Program
compile = parseExp lit add mul
