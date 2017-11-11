module Main where

import Lib

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Maybe

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 22: Reader"
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."


  -- print $ sequenceA [Just 3, Just 2, Just 1]
  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs, ys]
  -- print $ summed <$> ((,) <$> xs <*> ys)
  -- print $ fmap summed ((,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ fmap bolt z
  --
  -- print $ sequenceA [(>3), (<8), even] 7
  -- print $ foldr (&&) True $ sequA 7
  -- print $ sequA $ fromMaybe 0 s'
  -- print $ bolt $ fromMaybe 0 ys
