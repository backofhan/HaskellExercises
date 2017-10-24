module Main where

import Lib
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

main :: IO ()
main = do
  putStrLn "Excercises from Chapter 26: Monad transformers."
  putStrLn "Some examples commented out in main function."
  putStrLn "Use 'stack ghci' to start ghci to play around. Enjoy."

  -- Examples for playing around
