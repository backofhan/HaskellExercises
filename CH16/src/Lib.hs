{-# LANGUAGE InstanceSigs #-}
module Lib where

import Test.QuickCheck
import Test.QuickCheck.Function

-- 16.4 Exercises: Be Kind
  -- 1 a: *
  -- 2 b: * -> *   T: * -> *
  -- 3 c: * -> * -> *

-- 16.7 Exercises: Heavy Lifting
  -- 1
-- a = (+1) $ read "[1]" :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]

  -- 2
-- b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap.fmap) (++ "lol") (Just ["Hi,", "Hello"])

  -- 3
-- c = (*2) (\x -> x - 2)
c = fmap (*2) (\x -> x - 2)
c' = (*2).(\x -> x - 2)

  -- 4
-- d = ((return '1' ++). show) (\x -> [x, 1..3])
d = ((return '1' ++). show).(\x -> [x, 1..3])
d' = fmap ((return '1' ++). show) (\x -> [x, 1..3])

  -- 5
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123" ++) show ioi
--     in (*3) changed
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read.("123" ++).show) ioi
    in fmap (*3) changed

-- 16.10 Exercises: Instances of Func
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g.f) x) == (fmap g . fmap f $ x)

  -- 1
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

  -- 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

  -- 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

  -- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

  -- 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

  -- 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  -- 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  -- 8
-- Can not implement because Trival has kind *
-- Only possible to implement functor for type which has kind * -> *
