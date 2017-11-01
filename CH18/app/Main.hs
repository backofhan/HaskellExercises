module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 18: Monad."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- 18.7 Chapter Exercises
  --  quickBatch (monad (undefined :: Nope (String, String, Int)))
  --  quickBatch (monad (undefined :: PhEither (String, String, Int) (Char, Int, Int)))
  --  quickBatch (monad (undefined :: Identity (Char, Int, Int)))
  --  quickBatch (monad (undefined :: List (Char, Int, Int)))
