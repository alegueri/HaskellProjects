{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : 1JC3-Assign2.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for 1JC3-Assign2 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Assign_2 as A2
import Data.Char
import Data.Fixed (mod')
import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable)
import Test.Hspec
import Test.QuickCheck.Property (property)

{- ----------------------------------------------------------------------------------------------------------------
 -  QuickCheck Properties
 - ----------------------------------------------------------------------------------------------------------------
 -}

-- | Existential type wrapper for QuickCheck propositions, allows @propList@ to essentially
--   act as a heterogeneous list that can hold any quickcheck propositions of any type
data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }

-- | Boolean implication
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y
infixr 4 ==>

{- -----------------------------------------------------------------
 - getXYZProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if getX,getY unwrap the correct values
 -    from (Double,Double,Double). Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck getXYZProp
 - -----------------------------------------------------------------
 - |   Input     | v = (x,y) :: (Double,Double,Double)                    |
 - -----------------------------------------------------------------
 - |   Output    | getX v == x && getY v == y && getZ == v         |
 - -----------------------------------------------------------------
 -}
getXYZProp :: (Double,Double,Double) -> Bool
getXYZProp v = v == (A2.getX v,A2.getY v,A2.getZ v)

{- -----------------------------------------------------------------
 - scalarMultProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if scalarMult correctly performs
 -    scalar multiplication. Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck scalarMultProp
 - -----------------------------------------------------------------
 - |   Input     | s :: Double                                     |
 - |             | v = (x,y,z) :: (Double,Double,Double)           |
 - -----------------------------------------------------------------
 - |   Output    | scalarMult s v == (s*x,s*y,z*z)                 |
 - -----------------------------------------------------------------
 -}
scalarMultProp :: Double -> (Double,Double,Double) -> Bool
scalarMultProp s v@(x,y,z) = A2.getX (A2.scalarMult s v) == s * x
                           && A2.getY (A2.scalarMult s v) == s * y
                           && A2.getZ (A2.scalarMult s v) == s * z

{- -----------------------------------------------------------------
 - addProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if add correctly performs vector
 -    addition. Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck addProp
 - -----------------------------------------------------------------
 - |   Input     | v0 = (x0,y0,z0) :: (Double,Double,Double)       |
 - |             | v1 = (x1,y1,z1) :: (Double,Double,Double)       |
 - -----------------------------------------------------------------
 - |   Output    | add v0 v1 == (x0+x1,y0+y1)                      |
 - -----------------------------------------------------------------
 -}
addProp :: (Double,Double,Double)  -> (Double,Double,Double)  -> Bool
addProp v0@(x0,y0,z0) v1@(x1,y1,z1) =
  let
    sum (x,y,z) = x + y + z
    tol = 1e-2
  in abs (sum (A2.add v0 v1) - sum (x0 + x1,y0 + y1,z0 + z1))
     <= tol

{- -----------------------------------------------------------------
 - innerProductProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if innerProdProp correctly performs
 -    the Euclidian Inner Product. Call this using Test.QuickCheck,
 -    i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck innerProdProp
 - -----------------------------------------------------------------
 - |  Input      | v0 = (x0,y0,z0) :: (Double,Double,Double)       |
 - |             | v1 = (x1,y1,z1) :: (Double,Double,Double)       |
 - -----------------------------------------------------------------
 - |  Output     | innertProduct v0 v1 == x0*x1 + y0*y1 + z0*z1    |
 - -----------------------------------------------------------------
 -}
innerProductProp :: (Double,Double,Double) -> (Double,Double,Double) -> Bool
innerProductProp v0@(x0,y0,z0) v1@(x1,y1,z1) =
  let
    tol = 1e-2
    i0 = A2.innerProduct v0 v1
    i1 = x0*x1 + y0*y1 + z0*z1
  in abs (i0 - i1) <= tol

{- -----------------------------------------------------------------
 - distanceProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if distance correctly computes
 -    the Euclidian distance. Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck distanceProp
 - -----------------------------------------------------------------
 - |  Input      | v0 = (x0,y0) :: (Double,Double,Double)          |
 - |             | v1 = (x1,y1) :: (Double,Double,Double)          |
 - -----------------------------------------------------------------
 - |  Output     | distance v1 v0 = sqrt((x1-x0)^2 + (y1-y0)^2)    |
 - -----------------------------------------------------------------
 -}
distanceProp :: (Double,Double,Double) -> (Double,Double,Double) -> Bool
distanceProp v1@(x0,y0,z0) v0@(x1,y1,z1) =
  let
    tol = 1e-4
  in abs ( A2.distance v1 v0
           - sqrt ((x1-x0)^2 + (y1-y0)^2 + (z1-z0)^2 )) <= tol

{- -----------------------------------------------------------------
 - maxDistanceProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if maxDistance correctly returns
 -    the point with the greatest Euclidian distance.
 -    Call this using Test.QuickCheck,  i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck maxDistanceProp
 - ----------------------------------------------------------------------
 - |  Input      | vs :: [(Double,Double,Double)]                       |
 - ----------------------------------------------------------------------
 - |  Output     | forall e in vs,                                      |
 - |             | distance (maxDistance vs) (0,0,0)                    |
 - |             |                               >= distance e (0,0,0)  |
 - ----------------------------------------------------------------------
 -}
maxDistanceProp :: [(Double,Double,Double)] -> Bool
maxDistanceProp vs =
  let
    maxDistance = A2.distance (0,0,0) $ A2.maxDistance vs
    distances = map (A2.distance (0,0,0)) vs
    tol = 1e-4
  in and [ maxDistance + tol >= d | d <- distances ]

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "scalarMult (and getX / getY / getZ)" $ do
    it "each component of scalarMult's return value should be scaled by s" $ property $ scalarMultProp
  describe "add v0 v1 = v2" $ do
    it "The sum of each element of v0 and v1 should be the sum of all elements of v2" $ property $ addProp
  describe "innerProduct" $ do
    it "should point-wise multiply each compenent and sum" $ property $ innerProductProp
  describe "distance" $ do
    it "should be the proper euclidean distance" $ property $ distanceProp
  describe "maxDistance" $ do
    it "should be the element with the maximum distance" $ property $ maxDistanceProp
