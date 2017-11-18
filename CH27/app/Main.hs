module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 27: Non-strictness."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- Examples for playing around
  -- print $ take' 10 $ map' (+1) (repeat' 1)  -- This will not return
