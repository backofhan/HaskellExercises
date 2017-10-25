module Main where

import Lib
import Test.QuickCheck
import Test.QuickCheck.Function

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 16: Functor."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- Examples for playing around
  -- quickCheck $ \x -> functorIdentity (x :: Identity Int)
  -- quickCheck (functorCompose :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool)

  -- quickCheck $ \x -> functorIdentity (x :: Pair Char)
  -- quickCheck (functorCompose :: Pair Int -> Fun Int Char -> Fun Char String -> Bool)

  -- quickCheck $ \x -> functorIdentity (x :: Two Char Int)
  -- quickCheck (functorCompose :: Two Int Char -> Fun Char Int -> Fun Int String -> Bool)

  -- quickCheck $ \x -> functorIdentity (x :: Three Char Int String)
  -- quickCheck (functorCompose :: Three Int Char String -> Fun String Int -> Fun Int String -> Bool)

  -- quickCheck $ \x -> functorIdentity (x :: Three' Char Int)
  -- quickCheck (functorCompose :: Three' Int String -> Fun String String -> Fun String Char -> Bool)

  -- quickCheck $ \x -> functorIdentity (x :: Four Char Int Char String)
  -- quickCheck (functorCompose :: Four Int String Char Int -> Fun Int String -> Fun String Char -> Bool)

  -- quickCheck $ \x -> functorIdentity (x :: Four' Char Int)
  -- quickCheck (functorCompose :: Four' Char Int -> Fun Int String -> Fun String Char -> Bool)
