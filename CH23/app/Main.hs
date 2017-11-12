module Main where

import Lib

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 23: State"
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  --mapM_ (putStrLn . fizzBuzz) [1..100]
  --mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
  --mapM_ putStrLn $ fizzbuzzList' [1..100]
  --mapM_ putStrLn $ fizzbuzzFromTo 1 100
