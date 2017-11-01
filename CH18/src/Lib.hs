module Lib where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 18.2 Sorry - Monad is not a burrito
  -- The answer is the exercise
bind :: Monad m => (a -> m b) -> m a -> m b
bind f v = join $ fmap f v

-- 18.4 Examples of Monad use
  -- Short Exercise: Either Monad
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  _ <*> (First a) = First a
  (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b

-- 18.7 Chapter Exercises
  -- Write Monad instances and verify with QuickCheck
    -- 1 Nope
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>=  _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

    -- 2 PhhhbbtttEither
data PhEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance Applicative (PhEither b) where
  pure = Left'
  _ <*> (Right' b) = Right' b
  (Right' b) <*> _ = Right' b
  (Left' f) <*> (Left' a) = Left' (f a)

instance Monad (PhEither b) where
  return = pure
  (Right' b) >>= _ = Right' b
  (Left' a) >>= f = f a

instance (Eq a, Eq b) => EqProp (PhEither b a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhEither b a) where
  arbitrary = frequency [(1, Left' <$> arbitrary), (1, Right' <$> arbitrary)]

    -- 3 Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

    --4 List
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

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a->b->b)->b->List a->b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f la = concat' $ fmap f la

instance Monad List where
  return = pure
  as >>= f = concat' $ fmap f as

instance (Eq a) => EqProp (List a) where (=-=) = eq

fromList :: [a] -> List a
fromList = foldr Cons Nil

-- Yes, here we still use [] to map/generate a List
-- Another alternative will be seen in the future chapter
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = fromList <$> arbitrary

  -- Write functions using Monad & Functor
    -- 1
j :: Monad m => m (m a) -> m a
j mma = mma >>= id

    --2
l1 :: Monad m => (a->b)-> m a -> m b
l1 f ma = f <$> ma

    -- 3
l2 :: Monad m => (a->b->c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

    --4
a :: Monad m => m a -> m (a->b) -> m b
a ma mf = mf <*> ma

    --5
meh :: Monad m => [a] -> (a-> m b) -> m [b]
meh [] _ = return []
-- Both ways are working. Using bind (>>=) or do syntax
-- meh (x:xs) f =
--   f x >>= \b ->
--   meh xs f >>= \bs ->
--   return (b:bs)
meh (x:xs) f = do
  b <- f x
  bs <- meh xs f
  return (b:bs)

    --6
flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id
