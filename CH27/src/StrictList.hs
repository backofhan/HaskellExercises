{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}
module StrictList where

-- 27.13 Chpater Excercises
  --Strict List
data List a =
    Nil
  | Cons a (List a)
  deriving (Show)

take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' x = xs where xs = Cons x xs
