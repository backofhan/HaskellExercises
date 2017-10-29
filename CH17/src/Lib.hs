module Lib where

import Data.List (elemIndex)

-- 17.5 Applicative in use
  -- Exercises: Lookups
    -- 1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

    -- 2
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tuple :: Maybe (Integer, Integer)
tuple = (,) <$> y <*> z

    -- 3
x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

    -- 4
xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

       -- This is tricky, finally the sum is trying to fold a tuple
       -- For tuple, just like snd. So the result is Just 5, not Just 11
       -- The reason could be found the Chapter of Foldable
summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')
