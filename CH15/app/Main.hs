module Main where

import Lib
import Test.QuickCheck
import Test.QuickCheck.Function
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
  -- quickCheck (semigroupAssoc :: Three String (Sum Int) String -> Three String (Sum Int) String -> Three String (Sum Int) String -> Bool)
  -- quickCheck (semigroupAssoc :: Four String String String String -> Four String String String String -> Four String String String String -> Bool)
  -- quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  -- quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  -- quickCheck (semigroupAssoc :: Or Int Char -> Or Int Char -> Or Int Char -> Bool)

  -- quickCheck (combineAssoc :: Combine Int String -> Combine Int String -> Combine Int String -> Int -> Bool) -- Note: combineAssoc
  -- quickCheck (compAssoc :: Comp (Sum Int) -> Comp (Sum Int) -> Comp (Sum Int) -> Sum Int -> Bool) -- Note: compAssoc

  -- quickCheck (semigroupAssoc :: Validation' (Product Int) Char -> Validation' (Product Int) Char -> Validation' (Product Int) Char -> Bool)
  -- quickCheck (semigroupAssoc :: AccumulateRight (Product Int) String ...
  -- quickCheck (semigroupAssoc :: AccumulateBoth (Product Int) String ...



  -- quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  -- quickCheck (monoidRightIdentity :: Trivial -> Bool)
  -- quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  -- quickCheck (monoidRightIdentity :: Identy String -> Bool)
  -- quickCheck (monoidLeftIdentity :: Two (Sum Int) String -> Bool)
  -- quickCheck (monoidRightIdentity :: Two (Product Int) String -> Bool)
  -- quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  -- quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  -- quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  -- quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  -- quickCheck (combineLeftIdentity :: Combine Int String -> Int -> Bool)   -- combineLeftIdentity
  -- quickCheck (combineRightIdentity :: Combine String (Product Int) -> String -> Bool)   -- combineRightIdentity
  -- quickCheck (compLeftIdentity :: Comp (Sum Int) -> Sum Int -> Bool)     -- compLeftIdentity
  -- quickCheck (compRightIdentity :: Comp String -> String -> Bool)     -- compRightIdentity

  -- quickCheck (memLeftIdentity :: Fun Int (String, Int) -> Int -> Bool)
  -- quickCheck (memRightIdentity :: Fun Char ((Product Int), Char) -> Char -> Bool)
