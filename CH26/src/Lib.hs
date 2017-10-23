{-# LANGUAGE InstanceSigs #-}
module Lib where

-- 26.3 EitherT
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap.fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . Right

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  emf <*> ema = EitherT $ (<*>) <$> runEitherT emf <*> runEitherT ema
