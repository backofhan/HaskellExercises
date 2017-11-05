module Lib where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

-- 20.5 Some basic derived operations
  -- Exercises: Library Functions
    -- 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

    --2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

    --3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (==a))

    --4
newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y    = Min m
    | otherwise = Min n

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getMin . foldMap (Min . Just)

    --5
newtype Max a = Max {getMax :: Maybe a}

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

  m `mappend` Max Nothing = m
  Max Nothing `mappend` n = n
  (Max m@(Just x)) `mappend` (Max n@(Just y))
    | x >= y    = Max m
    | otherwise = Max n

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax. foldMap (Max . Just)

    --6
null' :: (Foldable t) => t a -> Bool
null' = getAll . foldMap (const $ All False)

    --7
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const $ Sum 1)

    --8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

    --9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

    --10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x b -> f x <> b) mempty

-- 20.6 Chapter Exercises
  --1
data Constant a b = Constant a deriving (Show)

instance Foldable (Constant a) where
  foldr _ b _ = b
  --foldMap _ _ = mempty -- Also works

  --2
data Two a b = Two a b deriving (Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

  --3
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

  --4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = f b1 <> f b2

  --5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

  -- Thinking cap time
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)
