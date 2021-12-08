{-|
Module      : HaskellExercises06.Exercises06
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 06 - McMaster CS 1JC3 2021
-}
module Exercises06 where

import Prelude hiding (zipWith,foldr,foldl,concat,concatMap,lookup)

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------
macid = "TODO"

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement the Higher Order Function zipWith that behaves like zip but instead of zipping a tuple,
-- uses a binary operator provided to it as an argument
-- E.x.  zipWith (*) [1,2,3] [0,0,0]
--      = [0,0,0]
-- NOTE QuickCheck test uses (+) as the function parameter
-----------------------------------------------------------------------------------------------------------
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith op xs ys = case (xs, ys) of 
                  (x:xs', y:ys') -> (x `op` y) : zipWith op xs' ys' 
                  _ -> [] 

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function foldr "folds" an operator to the right. For example
-- E.x.  foldr (/) 1 [18,27,3]
--      = 18 / (27 / (3 / 1))

--  a =  3/ 1 = 3 
--  b = 27 / 3 = 9   foldr (/) result [18]
-- c =  18/ b 
-- NOTE QuickCheck test uses (-) as the function parameter
-----------------------------------------------------------------------------------------------------------
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op v (x:xs) = error "implement"

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function foldl "folds" an operator to the left. For example
-- E.x.  foldl (/) 18 [9,2,1]
--      = ((18 / 9) / 2) / 1
  -- NOTE QuickCheck test uses (-) as the function parameter
-----------------------------------------------------------------------------------------------------------
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op v (x:xs) = error "implement"
-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function concat (which takes a list of lists, and combines them into
-- just a list) using foldl
-----------------------------------------------------------------------------------------------------------
concat :: [[a]] -> [a]
concat xss = error "implement"



-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function concatMap using foldr. concatMap takes a function that returns a list,
-- maps that function of a list then concat's the result.
-- E.x.  concatMap (replicate 2) [1,2,3]  replicate 2 1 = [1,1] replicate 2 2 = [2,2] replicate 2 3 = [3,3]
--     == [1,1,2,2,3,3]
  -- NOTE QuickCheck test uses (replicate 2) as the function parameter
  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  -- E.x.  foldr (/) 1 [18,27,3]
  --      = 18 / (27 / (3 / 1))

 -- concatMap (replicate 2) [1,2]
 -- foldr (++) [] [  [1,1] ++ concatMap (replicate 2) [2] ]
--  foldr (++) [] [ [1,1] ++ foldr (++) [] [ [2,2] ++ concatMap (replicate 2) []  ]  ] 
--  foldr (++) [] [ [1,1] ++ foldr (++) [] [ [2,2] ++ []  ]  ] 
--  foldr (++) [] [ [1,1] ++ foldr (++) [] [ [2,2] ]  ] 
--  foldr (++) [] [ [1,1,2,2]  ] 
--  [1,1,2,2] 
-----------------------------------------------------------------------------------------------------------
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f (x:xs)  = error "implement"
-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement the function lookup that takes a list of tuples, where the first element of the tuple
-- serves as a key and the second element a value (a list like this is also known as a dictionary), and
-- a key value, then looks up the first occuring element corresponding to that key. The return value is
-- wrapped in the Maybe type, so if the key doesn't occur anywhere in the list the function returns Nothing
-- E.x.  lookup 2 [(0,'a'),(1,'b')] == Nothing
--       lookup 2 [(0,'a'),(2,'b'),(2,'c')] == Just 'b' fst snd(tuple)
-----------------------------------------------------------------------------------------------------------
lookup :: Eq k => k -> [(k,v)] -> Maybe v
lookup k0 (d:ds) =  error "implement" 
