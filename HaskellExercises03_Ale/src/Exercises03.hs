{-|
Module      : HaskellExercises03.Exercises03
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 03 - McMaster CS 1JC3 2021
-}
module Exercises03 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E. THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    OR DATA DECLERATIONS (I.E. data Tree a = ...)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------
macid = "TODO"

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement a function that computes the nth fibanacci number (see https://en.wikipedia.org/wiki/Fibonacci_number)
-- NOTE if the input is less than 0, simply return 0
-----------------------------------------------------------------------------------------------------------
fib :: (Integral a) => a -> a
fib n
  | n <= 0 = 0 
  | n == 1 = 1
  | otherwise = fib (n-1) + fib(n-2)
-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Using the given Tree data type, encode the following tree in Haskell
--                d
--              /   \
--             b     f
--            / \   / \
--           a   c e   g
-----------------------------------------------------------------------------------------------------------
data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
   deriving (Show,Eq)

exTree :: Tree Char
exTree = Node 'd'
         (Node 'b' (Leaf 'a') (Leaf 'c')) 
         (Node 'f' (Leaf 'e') (Leaf 'g'))

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement a function that multiples two numbers encoded using the following Nat data type
-- NOTE start by creating an function add :: Nat -> Nat -> Nat
-----------------------------------------------------------------------------------------------------------

two = Succ (Succ Zero)
three = Succ (Succ (Succ Zero))
four = Succ (Succ (Succ (Succ Zero)))

data Nat = Zero | Succ Nat
         deriving (Show,Eq)


add :: Nat -> Nat -> Nat 
add Zero n = n
add (Succ m) n = Succ (add m n)  -- (m+1) + n = (m+n) + 1 


{-
m == Succ Succ Zero == 2
n == Succ Succ Zero == 2 

add (Succ Succ Zero) (Succ Succ Zero)
    = Succ (add (Succ Succ Zero)(Succ Zero) ) 
    = Succ Succ (add (Succ Succ Zero) (Zero))
    = Succ Succ Succ Succ Zero === 4 
-}


mult :: Nat -> Nat -> Nat
mult Zero n     = Zero 
mult (Succ m) n = add n (mult m n) -- m is the number we are subtracting by one 
                                                                                --i.e. 3 + (3*3) 
                                                                                    -- 3 + 3 + (3*2) 
                                                                                    -- 3 + 3 + 3 + (3*1)
                                                                                -- i.e. 4 * 3 = 4 + 4 +4 
                                                                                    -- 4 + (4*2) 
                                                                                    -- 4 + 4 + (4*1) 
                                                                                    
{-
m == Succ Succ Zero == 2
n == Succ Succ Succ Zero == 3 

Mult (Succ Succ Zero) (Succ Succ Succ Zero)
    = add Succ Succ Succ Zero (mult (Succ Zero) (Succ Succ Succ Zero))
    = add Succ Succ Succ Zero add Succ Succ Succ Zero mult Zero Succ Succ Succ Zero 
    = (add Succ Succ Succ Zero) (add (Succ Succ Succ Zero) (Zero))

    ...
    = Succ Succ Succ Succ Succ Succ Zero 

    in other words: 
  Mult (2) (3)  == 3 + 3 
  = add 3 (mult 1 3)
  = add 3  add 3 (mult 0 3)
  = add 3 add 3  (Zero)
  = add 3 3
  = 6 
  ...
  = Succ Succ Succ Succ Succ Succ Zero 


      
-}
-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement a function that determines whether of not a given tree is a search tree (is a "sorted" tree)
-- NOTE: this means for every Node v t0 t1, v is >= all values in tree t0 and v is < all values in tree t1
--       start by defining functions that test if a value is less/greater than all the values in a tree
-----------------------------------------------------------------------------------------------------------
{-
           4 
        |      |
       2        5
    |   |
    1   3
[1,2,3,4,5]
-}
tree  = Node 4 (Node 2 (Leaf 1) (Leaf 3)) (Leaf 5) 

tree1  = Node 7 (Node 2 (Leaf 1) (Leaf 3)) (Leaf 5) 
tree2 = Node 1 (Node 1 (Leaf 0) (Leaf (-2))) (Leaf 2)
tree3 = Node (-16) (Leaf (-23)) (Node (-16) (Leaf 25) (Leaf 32))

tree4 = Node 1 (Leaf 0) (Leaf (-2))


--- WAY 2 

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node a t1 t2)= flatten t1 ++ [a] ++ flatten t2 


searchList :: Ord a => [a] -> Bool
searchList [a] = True  
searchList (x:xs) = (x <= (head xs)) && searchList xs

sortTree [] = True
sortTree [a] = True 
sortTree (x:y:xs) = if x<=y then sortTree(y:xs) else False 
 
isSearchTree :: Ord a => Tree a -> Bool
isSearchTree t1= sortTree (flatten t1)



-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement a function factors that takes an Int and returns a list off all its factors (i.e. all the
-- Int's bigger than 1 and less than that Int that are divisable without a remainder)
-- NOTE using list comprehension gives a pretty neat solution
-----------------------------------------------------------------------------------------------------------
factors :: Int -> [Int]
factors n = [x | x <- [2..(n-1)], (n `mod` x) == 0]


-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement a function pivot that takes a value and a list, then returns two lists in a tuple, with the
-- first list being all elements <= to the value, and the second list being all elements > the value
-- I.e.  pivot 3 [5,6,4,2,1,3]
--         = ([2,1,3],[5,6,4])
-- NOTE using list comprehensions gives a pretty neat solution
-----------------------------------------------------------------------------------------------------------
pivot :: Ord a => a -> [a] -> ([a],[a])
pivot v xs = (ss, ys) where 
            ss = [x | x<-xs, x<= v]
            ys = [y | y<-xs, y > v]

