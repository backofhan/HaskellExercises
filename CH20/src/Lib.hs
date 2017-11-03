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
