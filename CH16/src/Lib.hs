{-# LANGUAGE InstanceSigs #-}
module Lib where


-- 16.4 Exercises: Be Kind
  -- 1 a: *
  -- 2 b: * -> *   T: * -> *
  -- 3 c: * -> * -> *

-- 16.7 Exercises: Heavy Lifting
  -- 1
-- a = (+1) $ read "[1]" :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]

  -- 2
-- b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap.fmap) (++ "lol") (Just ["Hi,", "Hello"])

  -- 3
-- c = (*2) (\x -> x - 2)
c = fmap (*2) (\x -> x - 2)
c' = (*2).(\x -> x - 2)

  -- 4
-- d = ((return '1' ++). show) (\x -> [x, 1..3])
d = ((return '1' ++). show).(\x -> [x, 1..3])
d' = fmap ((return '1' ++). show) (\x -> [x, 1..3])

  -- 5
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123" ++) show ioi
--     in (*3) changed
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read.("123" ++).show) ioi
    in fmap (*3) changed
