module Main where

import Lib
import Test.QuickCheck
import Data.Monoid hiding((<>))
import Data.Semigroup

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 15: Monoid, Semigroup."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- quickCheck (monoidAssoc :: First' String -> First' String -> First' String -> Bool)
  -- quickCheck (monoidLeftIdentity :: First' String -> Bool)
  -- quickCheck (monoidRightIdentity :: First' String -> Bool)

  -- quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  -- quickCheck (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  -- quickCheck (semigroupAssoc :: Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool)
