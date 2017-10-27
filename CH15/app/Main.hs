module Main where

import Lib
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 15: Monoid, Semigroup."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- quickCheck (monoidAssoc :: First' String -> First' String -> First' String -> Bool)
  -- quickCheck (monoidLeftIdentity :: First' String -> Bool)
  -- quickCheck (monoidRightIdentity :: First' String -> Bool)
