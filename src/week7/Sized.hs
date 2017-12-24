{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Sized where

import Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
-- used in the case of pair of annotations : (size,score).
instance Sized a => Sized (a,b) where
  size = size . fst

instance Monoid Size where
  mempty  = Size 0
  mappend = (+)
