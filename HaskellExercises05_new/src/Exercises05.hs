{-|
Module      : HaskellExercises05.Exercises05
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 05 - McMaster CS 1JC3 2021
-}
module Exercises05 where

import Prelude hiding (take,drop,replicate,(!!),elem,and,or)

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
-- Implement the function split that takes a list and splits it in half and returns a tuple of the
-- two halves, WITHOUT USING TAKE / DROP
-- NOTE when the list is uneven, the second list is one element larger than the first
-- NOTE^2 when using take / drop, although convenient, you introduce redundant computation. A more
--        efficient implementation of this function can be done calling an auxilary function with
--        different parameters that recurses through the list directly

-- i.e. split [2,3,4,5,6] == ([2,3][4,5,6])
--      half = 5 `div` 2 = 2 
--      split' [] [2,3,4,5,6] 2 
--          = split' ([] ++ [2]) [3,4,5,6] 1
--          = split' ([2] ++ [3]) [4,5,6] 0
--          = split' ([2,3], [4,5,6]) 0 = ([2,3],[4,5,6])
-----------------------------------------------------------------------------------------------------------
split :: [a] -> ([a],[a])
split xs =

  let
    half = length xs `div` 2
    split' xs ys 0 = (xs,ys)
    split' xs (y:ys) n = split' (xs ++ [y]) ys (n-1) 
    split' xs [] n = (xs,[])
   in split' [] xs half



-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the function merge that takes two lists that (assuming both lists are already sorted) merges
-- them together in to a sorted list
-- merge [2,4,6][1,3,8]
--      = 1 : merge(2:[4,6])[3,8]
--      = 1: 2: merge([4,6])(3:[8])
--      = 1: 2: 3 : merge (4:[6])[8]
--      = 1:2:3:4:6:8

-----------------------------------------------------------------------------------------------------------

merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys) 
    | otherwise = y : merge (x:xs) ys
merge [] (y:ys) = y:ys 
merge (x:xs) [] = x:xs
merge [] [] = [] 
            

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the function mergeSort that sorts a list by recusively splitting a list, and merging
-- the sorted lists back together
-- NOTE singleton and empty lists are already sorted
-- (x,y)
-- mergeSort[4,2,5,6]
--     = xs' = [4,2]
--     = ys' = [5,6]
--     = sort1 = mergeSort [4,2]         sort2 = mergeSort[5,6]
--     =       xs'' = [4]                    xs'' = [5]
--     =       ys'' = [2]                    ys'' = [6]
--     =       sort1 = mergeSort [4]         sort1 = mergeSort [5] = [5]
--                   = [4]
--     =       sort2 = mergeSort [2]         sortt2 = mergeSort [5] = [6]
--     =       sort2 = [2]
--     =       merge [4] [2] == [2,4]        merge [5][6] == [5,6]           
--     = sort1 = [2,4]                sort2 = [5,6]
--     = merge[2,4][5,6] 
--     = [2,4,5,6] 
-----------------------------------------------------------------------------------------------------------
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = [] 
mergeSort [x] = [x] 
mergeSort xs = let 
    xs' = fst(split(xs))
    ys' = snd(split(xs))
    sort1 = mergeSort xs' 
    sort2 = mergeSort ys' 
    in merge sort1 sort2


-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the function sortProp that tests if a list is sorted or not
-- NOTE you can use this with QuickCheck to test your mergSort function by calling
--      quickCheck (sortProp . mergeSort)
-----------------------------------------------------------------------------------------------------------

sortProp :: (Ord a) => [a] -> Bool
sortProp (x0:x1:xs) = x0 <= x1 && sortProp (x1:xs)
sortProp _ = True

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function replicate that takes an Int n and a element and returns a list that
-- replicates that element n times

-- what where some issues you came accross when implementing this? 
-----------------------------------------------------------------------------------------------------------
replicate :: Int -> a -> [a]
replicate n x = if n==0 then [] else x : replicate (n-1) x

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function !! that selects the nth element of a list using recursion
-- NOTE throw an error when indexing out of bounds

-- [1,2,3,4,5] !! 2 
--    (!!) [2,3,4,5] 1
--     if  1 ==0 then 2 else (!!)[3,4,5] 0
--     if 0 == 0 then 3 
-----------------------------------------------------------------------------------------------------------
(!!) :: [a] -> Int -> a
(!!) (x:xs) n = if n == 0 then x else (!!) xs (n-1) 
(!!) [] n = error "index out of bounds!" 

-- Exercise G
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function elem that takes a value and a list and returns True if the value
-- is an element of the list
-----------------------------------------------------------------------------------------------------------
elem :: (Eq a) => a -> [a] -> Bool
elem e (x:xs) =  if x == e then True else elem e xs 
elem e [] = False
