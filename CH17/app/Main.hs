module Main where

import Lib
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 17: Applicative."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- Exercise: Fixer Upper
    -- 1
  -- const <$> Just "Hello" <*> pure "World"
    -- 2
  -- (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
