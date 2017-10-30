module Main where

import Lib
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 17: Applicative."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- 17.8 ZipList Monoid; Exercise: Fixer Upper
    -- 1
  -- const <$> Just "Hello" <*> pure "World"
    -- 2
  -- (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

  -- 17.8 ZipList Monoid; ZipList Applicative Exercise
  -- The code in the book doesn't work. We must use Cons
  -- let z = ZipList' $ Cons (+9) $ Cons (*2) $ Cons (+8) Nil
  -- let z' = ZipList' $ Cons 1 $ Cons 2 $ Cons 3 Nil
  -- z <*> z'
  -- Result> ZipList' (Cons 10 (Cons 4 (Cons 11 Nil)))
  -- let z' = ZipList' (repeat' 1)
  -- z <*> z'
  -- Result> ZipList' (Cons 10 (Cons 2 (Cons 9 Nil)))

  -- 17.8 ZipList Monoid; List Applicative Exercise
  -- quickBatch (applicative (undefined :: List (Product Int, String, Int)))
  -- 17.8 ZipList Monoid; ZipList Applicative Exercise
  -- quickBatch (applicative (undefined :: ZipList' (Product Int, Char, Int)))
  -- 17.8 ZipList Monoid; Exercise: Variations on Either
  -- quickBatch (applicative (undefined :: Validation (Product Int, String, Sum Int) (Sum Int, Char, Sum Int)))

  -- 17.9 Chapter Exercises
    -- Write applicative instance
  -- quickBatch (applicative (undefined :: Pair (String, String, Int)))
  -- quickBatch (applicative (undefined :: Two (Product Int, String, Sum Int) (Sum Int, Char, Sum Int)))
  -- quickBatch (applicative (undefined :: Three (Product Int, String, Sum Int) (Sum Int, All, Sum Int) (Sum Int, Any, Sum Int)))
  -- quickBatch (applicative (undefined :: Three' (Product Int, String, Sum Int) (Sum Int, All, Sum Int)))
  -- quickBatch (applicative (undefined :: Four (Product Int, String, Sum Int) (String, String, Sum Int) (Product Int, String, Sum Int) (Product Int, String, Sum Int)))
  -- quickBatch (applicative (undefined :: Four' (Product Int, String, Sum Int) (Char, Int, Sum Int)))
