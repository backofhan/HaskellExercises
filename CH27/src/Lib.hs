{-# LANGUAGE InstanceSigs #-}

module Lib where

-- 27.13 Chpater Excercises
  -- What will :eprint output?
    -- 1 x = _
-- let x = 1

    -- 2 x = "1"
-- let x = ['1']

    -- 3 x = _
-- let x = [1]

    -- 4 x = 1
-- let x = 1 :: Int

    -- 5 x = _
-- let f = \x -> x
-- let x = f 1

    -- 6 x = _, x = 1
-- let f :: Int -> Int; f = \x -> x
-- let x = f 1

  -- Will printing this expression result in bottom?
    -- 1 snd (undefined, 1)  --NO

    -- 2 No (TYPO in y? should be 1? Or bottm out when defining)
-- let x = undefined
-- let y = x `seq` 1 in snd (x, y)

    -- 3 length $ [1..5] ++ undefined -- Yes

    -- 4 length $ [1..5] ++ [undefined] -- No, result 6

    -- 5 const 1 undefined --No

    -- 6 const 1 (undefined `seq` 1) --No

    -- 7 const undefined 1 --Yes

  -- Make the expression bottom
  x = undefined
  y = "blah"
  -- print (snd (x, x `seq` y))
  -- print (x `seq` snd (x, y))
  -- x `seq` print (snd (x, y))
