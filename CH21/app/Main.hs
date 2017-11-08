module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 21: Traversable."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- quickBatch (traversable (undefined:: Identity (Int, Int, [Int])))
  -- quickBatch (traversable (undefined :: Constant (String, String, String) (Char, String, String)))
  -- quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
  -- quickBatch (traversable (undefined :: List (Int, Int, [Int])))
  -- quickBatch (traversable (undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])))
  -- quickBatch (traversable (undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int])))
  -- quickBatch (traversable (undefined :: S (Three' Int) (Int, Int, [Int])))
  -- quickBatch (traversable (undefined :: Tree (Int, Int, [Int])))
