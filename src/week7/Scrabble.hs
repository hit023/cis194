module Scrabble where

newtype Score = Score Int
            deriving (Eq, Ord,Show)

instance Monoid Score where
    mempty = Score 0
    mappend (Score s1) (Score s2) = Score (s1+s2)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score ch
    | ch `elem` "aeilbnorstu" = Score 1
    | ch `elem` "dg"         = Score 2
    | ch `elem` "bcmp"       = Score 3
    | ch `elem` "fhvwy"      = Score 4
    | ch `elem` "k"          = Score 5
    | ch `elem` "jx"         = Score 8
    | ch `elem` "qz"         = Score 10
    | otherwise             = Score 0

scoreString :: String -> Score
scoreString = foldr1 mappend.map score
