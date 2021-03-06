{-# LANGUAGE InstanceSigs #-}
module Lib where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity
import Control.Monad
import Control.Applicative

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
  -- I have to inject 'return' to wrap value in IO monad. Any better solution?
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = (MaybeT . ExceptT . ReaderT) (const $ return (Right (Just 1)))

-- 26.9 MonadTrans
instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a
  lift = EitherT . fmap Right

instance MonadTrans (StateT r) where
  lift :: (Monad m) => m a -> StateT r m a
  lift ma = StateT $ \r -> let g a = (a, r) in fmap g ma

-- 26.10
  -- instance could not be hidden, we have to screate another MaybeT
newtype MaybeT' m a = MaybeT' {runMaybeT' :: m (Maybe a)}

instance Functor m => Functor (MaybeT' m) where
  fmap :: (a -> b) -> MaybeT' m a -> MaybeT' m b
  fmap f (MaybeT' mma) = MaybeT' $ (fmap.fmap) f mma

instance Applicative m => Applicative (MaybeT' m) where
  pure :: a -> MaybeT' m a
  pure = MaybeT' . pure . pure

  (<*>) :: MaybeT' m (a -> b) -> MaybeT' m a -> MaybeT' m b
  (MaybeT' mf) <*> (MaybeT' ma) = MaybeT' $ (<*>) <$> mf <*> ma

instance Monad m => Monad (MaybeT' m) where
  return :: a -> MaybeT' m a
  return = pure

  (>>=) :: MaybeT' m a -> (a -> MaybeT' m b) -> MaybeT' m b
  (MaybeT' mma) >>= f = MaybeT' $ do
    ma <- mma -- Maybe a
    case ma of
      Nothing -> return Nothing
      Just a -> runMaybeT' $ f a

  -- Finnally, it type checks. But how to verify?
instance (MonadIO m) => MonadIO (MaybeT' m) where
  liftIO :: IO a -> MaybeT' m a
  liftIO = MaybeT' . liftIO . fmap Just

-- Chapter Exercises
  -- 1
rDec :: Num a => Reader a a
rDec = reader $ \a -> a - 1

  -- 2
rDec' :: Num a => Reader a a
rDec' = reader $ flip (-) 1

  -- 3 & 4
rShow :: Show a => ReaderT a Identity String
rShow = reader show

  -- 5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> putStrLn ("Hi: " ++ show a) >> return (a + 1)

  -- 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> putStrLn ("Hi: " ++ show a) >> return (show a, a+1)

-- Fix the code (Original code are there following fixed ones)
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do  -- maybeExcite = do
  v <- getLine
  guard $ isValid v -- When invalid, this one throws exception rather than returning Nothing
                    -- Because it is in a IO (Maybe String), not a Maybe Monad
  return $ Just v -- return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite <|>  return Nothing -- excite <- maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
