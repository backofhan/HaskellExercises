{-# LANGUAGE InstanceSigs #-}
module Lib where

-- 25.4 Twinplicative
newtype Compose f g a = Compose { getCompose :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

  -- GOTCHA! Exercise time
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure  :: a -> Compose  f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose fga) = Compose $ (<*>) <$> f <*> fga

-- 25.6 Exercises: Compose Instances
  -- Compose Foldable
instance (Foldable f, Foldable g) => Foldable (Compose f g ) where
  foldMap ::(Monoid b) => (a -> b) -> Compose f g a -> b
  foldMap f (Compose fga) = (foldMap. foldMap) f fga

  -- Compose Traversable
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: (Applicative m) => (a -> m b) -> Compose f g a -> m (Compose f g b)
  traverse f (Compose fga)= Compose <$> (traverse.traverse) f fga

  -- And now for something completely different
class BiFunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

    -- 1
data Deux a b = Deux a b

instance BiFunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

    -- 2
data Const a b = Const a

instance BiFunctor Const where
  bimap f _ (Const a) = Const (f a)

    -- 3
data Drei a b c = Drei a b c

instance BiFunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

    -- 4
data SuperDrei a b c = SuperDrei a b

instance BiFunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

    -- 5
data SemiDrei a b c = SemiDrei a

instance BiFunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

    -- 6
data Quadriceps a b c d = Quadzzz a b c d

instance BiFunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

    -- 7
instance BiFunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)
