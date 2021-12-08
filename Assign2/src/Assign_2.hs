{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2021
-}
module Assign_2 where

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

-- Name: Alessandra Guerinoni 
-- Date: Oct 31 2021 
--macid :: "Guerinoa" 

macid = "Guerinoa"

type Vector3D = (Double,Double,Double)

{- -----------------------------------------------------------------
 - getX
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
getX :: Vector3D -> Double
getX (x,y,z) = x 

{- -----------------------------------------------------------------
 - getY
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
getY :: Vector3D -> Double
getY (x,y,z) = y

{- -----------------------------------------------------------------
 - getZ
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
getZ :: Vector3D -> Double
getZ (x,y,z) = z

{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
scalarMult :: Double -> Vector3D -> Vector3D
scalarMult s v = (s*getX v, s*getY v, s*getZ v)

{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
add :: Vector3D -> Vector3D -> Vector3D
add v0 v1 = (getX v0 + getX v1, getY v0 + getY v1,getZ v0 + getZ v1)


{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}

v0 = (0.0,0.0,0.1)
v1 = (0.0,0.0,0.0)
innerProduct :: Vector3D -> Vector3D -> Double
innerProduct v0 v1 = 
      let 
        x1 = getX v0
        x2 = getX v1
        y1 = getY v0
        y2 = getY v1
        z1 = getZ v0
        z2 = getZ v1
       in x1*x2 + y1*y2 + z1*z2

{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
distance :: Vector3D -> Vector3D -> Double
distance v1 v2 = 
      let 
        p = add v1 (scalarMult (-1) v2)
        q = add v1 (scalarMult (-1) v2)
      in sqrt(innerProduct p q) 

{- ------------------------------------------------------------------------
 - maxDistance
 - ------------------------------------------------------------------------
 - Description:
 -   TODO add comments
 -- -- NOTE zip [0..] xs   creates a list of (index,element) tuples

 -}

-- using maximum 
maxDistance :: [Vector3D] -> Vector3D



maxDistance v = snd (maximum [(distance (0, 0, 0) x, x) | x <- v])

-- 


--using sorting alg
qsort :: Ord a => [(Int,a)] -> [(Int,a)]
qsort [] = [] 
qsort (p:xs) = 
    let 
     smaller = [x | x<-xs, snd(x) <= snd(p)]
     greater = [x | x<-xs , snd(x) > snd(p)] 
    in qsort smaller ++ [p] ++ qsort greater   --- sort my list of tuples from below from smallest to greatest 

maxDistance1 :: [Vector3D] -> Vector3D
maxDistance1 [] = (0,0,0)
maxDistance1 v = v !! fst (last $ qsort indDist) -- pull the last tuple from my list from qsort, take the element, and
                                                -- find that element in v given that index. 
            where 
                dist = map distFunc v  -- remember v is list of vectors 
                v0 = (0.0,0.0,0.0) 
                distFunc v = distance v v0  -- this v is different; just single vector 
                indDist = zip [0..] dist    -- [(0,dist1),(1,dist2)...] 
    
                











{-}

maxDistance :: [Vector3D] -> Vector3D
maxDistance [x] = x
maxDistance (x:xs)
  | distance (maxDistance xs) (0,0,0) > distance x (0,0,0) = maxDistance xs
  | otherwise = x



maxDistance :: [Vector3D] -> Vector3D
maxDistance [] = (0, 0, 0)
maxDistance list = head result_list
  where
    distance_list = [distance x (0, 0, 0) | x <- list]
    result_list = [x | x <- list, distance x (0, 0, 0) == maximum distance_list]


maxDistance :: [Vector3D] -> Vector3D
maxDistance [] = (0.0, 0.0, 0.0)
maxDistance (v : vs)
  | distance zero v > distance zero (maxDistance vs) = v
  | otherwise = maxDistance vs
  where
    zero = (0.0, 0.0, 0.0)

    -}
 {- -----------------------------------------------------------------------
 - Function: scalarMult
 - Test Case Number: 1
 - Input: 2 (1,2,3)
 - Expected Output: (x,y,z)
 - Actual Output: (a,b,c)
 ------------------------------------------------------------------------
 - Function: scalarMult
 - Test Case Number: 2
 - Input: 
 - Expected Output: 
 - Actual Output: 
 ------------------------------------------------------------------------
 - Function: scalarMult
 - Test Case Number: 3
 - Input: 
 - Expected Output: 
 - Actual Output: 
-} 

