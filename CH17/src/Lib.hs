{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib where

import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)

-- 17.5 Applicative in use
  -- Exercises: Lookups
    -- 1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

    -- 2
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tuple :: Maybe (Integer, Integer)
tuple = (,) <$> y <*> z

    -- 3
x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

    -- 4
xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

       -- This is tricky, finally the sum is trying to fold a tuple
       -- For tuple, just like snd. So the result is Just 5, not Just 11
       -- The reason could be found the Chapter of Foldable
summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

  -- Exercise: Identity Instance
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

  -- Exercise: Constant Instance
newtype Constant a b =
  Constant { getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a1) <*> (Constant a2) = Constant $ a1 <> a2

-- 17.8 ZipList Monoid
  -- List Applicative Exercise
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  fs <*> as = flatMap (`fmap` as) fs

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f la = concat' $ fmap f la

concat' :: List (List a) -> List a
concat' = fold append Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a->b->b)->b->List a->b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

instance (Eq a) => EqProp (List a) where (=-=) = eq

fromList :: [a] -> List a
fromList = foldr Cons Nil

-- As List is homogeneous with [a]
-- Here we construct a [a], then convert to a List. Put a limit of 200 to avoid extremely long list
-- Another alternative is using sized from QuickCheck which we will see in future chapters
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = fromList . take 200 <$> arbitrary

  -- ZipList Applicative Exercise
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' Nil) <*> _ = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons a as)) =
    ZipList' $ Cons (f a) xs
    where (ZipList' xs) = ZipList' fs <*> ZipList' as

repeat' :: a->List a
repeat' a = Cons a (repeat' a)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 as = Nil
take' n (Cons a as) = Cons a $ take' (n-1) as

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

  -- Exercise: Variations on Either
data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (Failure' e1) <*> (Failure' e2) = Failure' $ e1 <> e2
  (Failure' e) <*> _ = Failure' e
  _ <*> (Failure' e) = Failure' e
  (Success' f) <*> (Success' a) = Success' $ f a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, Failure' <$> arbitrary), (1, Success' <$> arbitrary)]

instance (Eq a, Eq e) => EqProp (Validation e a) where (=-=) = eq

-- 17.9 Chapter Exercises
  -- Applicative type verification. Here in REPL, let p1 = ....
    -- 1 []
p1 = pure :: a -> [] a
ap1 = (<*>) :: [] (a -> b) -> [] a -> [] b

    -- 2 IO
p2 = pure :: a -> IO a
ap2 = (<*>) :: IO (a -> b) -> IO a -> IO b

    -- 3 (,) a
p3 = pure :: (Monoid a) =>  b -> (,) a b
ap3 = (<*>) :: (Monoid c) => (,) c (a -> b) -> (,) c a -> (,) c b

    -- 4 (->) e
p4 = pure :: a -> (e -> a)
ap4 = (<*>) :: (e -> a -> b) -> (e -> a) -> (e -> b)

  -- Write applicative instance
    -- 1 Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f1 f2) <*> (Pair a1 a2) = Pair (f1 a1) (f2 a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

    -- 2 Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty
  (Two a1 f) <*> (Two a2 b) = Two (a1 <> a2) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

    -- 3 Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a1 b1 f) <*> (Three a2 b2 c) = Three (a1 <> a2) (b1 <> b2) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

    -- 4 Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a1 f1 f2) <*> (Three' a2 b1 b2) = Three' (a1 <> a2) (f1 b1) (f2 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

    -- 5 Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a1 b1 c1 f) <*> (Four a2 b2 c2 d) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

    -- 6 Four'
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a11 a12 a13 f) <*> (Four' a21 a22 a23 b) = Four' (a11 <> a21) (a12 <> a22) (a13 <> a23) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

  -- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
-- combos stops vowels stops
