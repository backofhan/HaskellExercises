module Lib where

-- 3.4 Top-level versus local definitions
--    Exercises: Scope

--     1. YES
--     2. NO
--     3. NO
--     4. YES

-- 3.5 Types of concatenation functions
--    Exercises: Syntax Errors
--      1. NO
--         (++) [1,2,3] [4,5,6]  or  [1,2,3] ++ [4,5,6]
--      2. NO
--         "<3" ++ " Haskell"
--      3. YES

-- 3.8 Chapter Exercises
--    Reading Syntax
--      1.
--          a) YES
--          b) NO
--             (++) [1,2,3] [4,5,6]  or  [1,2,3] ++ [4,5,6]
--          c) YES
--          d) NO (Missing last ")
--             ["hello" ++ " world"]
--          e) NO
--             "hello" !! 4
--          f) YES
--          g) NO
--             take 4 "lovely"
--          h) YES
--      2.
--          a) -- d)
--          b) -- c)
--          c) -- e)
--          d) -- a)
--          e) -- b)
--    Building functions
--      1.
--          a) "Curry is awesome" ++ "!"
--          b) "Curry is awesome!" !! 4
--          c) drop 9 "Curry is awesome"
--      2.
funcA :: String -> String
funcA = (++ "!")

funcB :: String -> Char
funcB = (!! 4)

funcC :: String -> String
funcC = drop 9
--      3.
thirdLetter :: String -> Char
thirdLetter = (!! 2)
