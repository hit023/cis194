{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
import Sized
import Scrabble
import Data.Monoid
import StringBuffer
import Buffer
import Editor
import System.IO.Unsafe

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

tag :: Monoid m => JoinList m a -> m
tag jl = case jl of
    Empty -> mempty
    Single m a -> m
    Append k l r -> k

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l `mappend` tag r) l r

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 1 (Single _ a) = Just a
indexJ n (Append _ l r) = if leftsz >= n then indexJ n l else indexJ (n-leftsz) r
                            where
                                leftsz = getSize (size (tag l))
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n j@(Single _ _ ) = Empty
dropJ n j@(Append k l r) = if leftsz >= n then (+++) (dropJ n l) r else (+++) (dropJ leftsz l) (dropJ (n-leftsz) r)
                            where
                                leftsz = getSize (size (tag l))

--to cross-check the implementation
atz = foldr1 (+++) $ Single (Size 1) <$> ['a'..'z']
-- <$> stands for 'fmap' ; but for lists, it is simply 'map'.

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n j@(Single _ _) = if n>0 then j else Empty
takeJ n j@(Append k l r) = if leftsz >= n then takeJ n l else (+++) (takeJ leftsz l) (takeJ (n-leftsz) r)
                            where
                                leftsz = getSize.size.tag $ l

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

instance Buffer (JoinList (Size,Score) String) where
    toString b = unlines (jlToList b)
    fromString str = foldr1 (+++).map (\s -> (Single (Size 1,(scoreString s)) s)) $ lines str
    line = indexJ
    replaceLine n str b = case indexJ n b of
        Nothing -> b
        Just _ -> takeJ n b +++ fromString str +++ dropJ (n+1) b
    numLines = getSize.size.tag
    value b = getScore (snd (tag b))

main = runEditor editor $ (fromString "A\nChristmas\nCarol" :: JoinList (Score, Size) String)
