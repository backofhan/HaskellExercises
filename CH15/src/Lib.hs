{-# LANGUAGE FlexibleInstances #-}
module Lib where

import Test.QuickCheck
import Data.Monoid hiding((<>))
import Data.Semigroup

-- 15.10 Exercise: Optional Monoid
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only $ a `mappend` b
  mappend Nada a = a
  mappend a Nada = a

-- 15.12 Exercise: Maybe Another Monoid
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend l@(First' (Only a)) _ = l
  mappend (First' Nada) r = r

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (3, Only <$> arbitrary)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a `mappend` (b `mappend` c)) == ((a `mappend` b) `mappend` c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a ) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty ) == a

-- 15.14 Chapter Exercises
  -- Smigroup exercises
    -- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

    -- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

    -- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

    -- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

    -- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

    -- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

    -- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

    -- 8
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  a@(Snd _) <> _ = a
  (Fst _) <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

    -- 9  TODO Test
newtype Combine a b =
  Combine { unCombine :: a -> b}

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine ((<>) <$> f <*> g)

    -- 10 TODO Test
newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp ((<>) <$> f <*> g)

    -- 11
data Validation' a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation' a b) where
  (Failure' a1) <> (Failure' a2) = Failure' (a1 <> a2)
  a@(Failure' _) <> _ = a
  _ <> b@(Failure' _) = b
  a@(Success' _) <> _ = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
   arbitrary = frequency [(1, Failure' <$> arbitrary), (1, Success' <$> arbitrary)]

    -- 12
newtype AccumulateRight a b = AccumulateRight (Validation' a b) deriving (Eq, Show)

instance (Semigroup b) => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success' b1)) <> (AccumulateRight (Success' b2)) = AccumulateRight $ Success' $ b1 <> b2
  v1@(AccumulateRight (Failure' _)) <> _ = v1
  _ <> v2@(AccumulateRight (Failure' _)) = v2

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = AccumulateRight <$> arbitrary

    -- 3
newtype AccumulateBoth a b = AccumulateBoth (Validation' a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Success' b1)) <> (AccumulateBoth (Success' b2)) = AccumulateBoth $ Success' $ b1 <> b2
  (AccumulateBoth v1) <> (AccumulateBoth v2) = AccumulateBoth $ v1 <> v2

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = AccumulateBoth <$> arbitrary
