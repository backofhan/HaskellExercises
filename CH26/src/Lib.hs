{-# LANGUAGE InstanceSigs #-}
module Lib where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

-- 26.3 EitherT
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap.fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emf) <*> (EitherT ema) = EitherT $ (<*>) <$> emf <*> ema

instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT v) >>= f = EitherT $ do
    ema <- v
    case ema of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

swampEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swampEitherT (EitherT ema) = EitherT $ swapEither <$> ema

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT amb)= amb >>= either fa fb

-- 26.5 StateT
newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma)= StateT $ (fmap.fmap) g sma
                       where g (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s->return (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smf) <*> (StateT sma) = StateT $ \s-> do
    (f, s1) <- smf s
    (a, s2) <- sma s1
    return (f a, s2)

instance (Monad m) => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) ->StateT s m b
  (StateT sma) >>= f  = StateT $ \s->do
                                   (a, s1) <- sma s
                                   runStateT (f a) s1

-- 26.8 Lexically inner is structurally outer, Wrap It Up
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded =  undefined -- ??? (const (Right (Just 1)))
