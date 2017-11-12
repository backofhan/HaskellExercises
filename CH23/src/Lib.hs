{-# LANGUAGE InstanceSigs #-}
module Lib where

import System.Random
import qualified Data.DList as DL
import Control.Monad.Trans.State

-- 23.5 Throw down
  -- Exercises: Roll Your Own
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

    -- 1
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count +1) nextGen

    -- 2
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 0
  where go :: Int -> Int -> StdGen -> (Int, [Die])
        go sum count gen
          | sum >= n = (count, [])
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
                (c, dies) = go (sum + die) (count + 1) nextGen
            in (c, intToDie die : dies)

-- 23.6 Write State for yourself
newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi sa) = Moi  $ \s -> let (a, s') = sa s in (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi sf) <*> (Moi sa) = Moi $ \s ->
                              let (f, s') = sf s
                                  (a, s'') = sa s'
                              in (f a, s'')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi sa) >>= f = Moi $ \s ->
                        let (a, s') = sa s
                        in   runMoi (f a) s'

-- 23.7
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzList' :: [Integer] -> DL.DList String
fizzbuzzList' list = execState (mapM_ addResult' list) DL.empty

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

  -- Fizzbuzz Differently
fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fizzbuzzList [to, to-1 .. from]

-- 23.8 Chapter exercises
  -- 1  It is not possible to work with the State
  --    in the standard lib. But we do have our own state
  --    It is named Moi
get' :: Moi s s
get' = Moi $ \s -> (s, s)

  --2
put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

  --3
exec' :: Moi s a -> s -> s
exec' (Moi sa) s = let (_, s') = sa s in s'

  --4
eval' :: Moi s a -> s -> a
eval' (Moi sa) s = let (a, _) = sa s in a

  --5
modify'' :: (s -> s) -> Moi s ()
modify'' f = Moi $ \s->((), f s)
