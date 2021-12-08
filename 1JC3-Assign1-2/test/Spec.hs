{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises01.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises01 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Assign_1 as Student
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

-------------------------------------------------------------------------------------------
-- * QuickCheck Tests

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
 - sinTaylorProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if Student.cosTaylor is accurate
 -    enough by comparing to built-in Prelude cos. Call this using
 -    Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck sinTaylorProp
 - -----------------------------------------------------------------
 - |   Input     |                                                 |
 - |  a'         | the value the taylor series is centered at      |
 - |  h'         | incremental value used to compute x = a' + h'   |
 - -----------------------------------------------------------------
 - |   Output    | True iff | cos(x) - cosTaylor(x) | <= tolerance |
 - -----------------------------------------------------------------
 -}
sinTaylorProp :: Double -> Double -> Bool
sinTaylorProp a' h' =
  let
    -- restrict -2*pi < a < 2*pi
    a =  (a' `mod'` (2*pi))
    -- restrict 0 < h < pi/8
    h = (abs h' `mod'` (pi/8))
    -- calculate a values using built-in cos
    cos_a  = cos a
    sin_a  = sin a
    -- use input a <= x < a + pi/4 for input
    x = a + h
    -- compare built-in cos to cosTaylor within a given tolerance
    sinRef = sin x
    sin_x  = Student.sinTaylor a cos_a sin_a x
    tol = 1e-4
    -- Passes test iff | cos(x) - cosTaylor(x) | <= tol
  in abs (sinRef - sin_x) <= tol

{- -------------------------------------------------------------------------
 - fmodProp
 - -------------------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if Student.fmod is accurate
 -    enough by comparing to mod' from the Data.Fixed  library.
 -    Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck fmodProp
 - -------------------------------------------------------------------------
 - |  Input      |                                                         |
 - |  x',y'      | Floating point inputs                                   |
 - -------------------------------------------------------------------------
 - |  Output     | True iff (x - rem) / y)  is approximately a whole number|
 - -------------------------------------------------------------------------
 -}
fmodProp :: Double -> Double -> Bool
fmodProp x y =
  let
    -- if r is the correct remainder, w should be very close to a whole number
    r = Student.fmod x y
    w = (x - r) / y
    -- if w is almost a whole number, then d ~ 0.0
    d = abs $ w - fromInteger (round w)
    -- we'll allow for this much floating point error
    tol = 1e-6
    -- passes test iff d <= tol, NOTE: y must be > 0 to avoid NaN
  in (abs y < tol) || (d <= tol)


{- -------------------------------------------------------------------------
 - sinApproxProp
 - -------------------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if Student.sinApprox is accurate
 -    enough by comparing to built-in sin from the Prelude.
 -    Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck sinApproxProp
 - -------------------------------------------------------------------------
 - |  Input      |                                                         |
 - |  x'         | Floating point input                                    |
 - -------------------------------------------------------------------------
 - |  Output     | True iff sinApprox(x) ~ sin(x)                          |
 - -------------------------------------------------------------------------
 -}
sinApproxProp :: Double -> Bool
sinApproxProp x =
  let
    -- get value of sin for Prelude cos and Student sinApprox
    sinRef = sin x
    sin_x  = Student.sinApprox x
    -- we'll allow for this much margin of error
    tol    = 1e-2
    -- passes test iff | sin(x) - sinApprox(x) | <= tol
  in abs (sinRef - sin_x) <= tol

{- -------------------------------------------------------------------------
 - cosApproxProp
 - -------------------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if Student.cosApprox is accurate
 -    enough by comparing to built-in cos from the Prelude.
 -    Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck cosApproxProp
 - -------------------------------------------------------------------------
 - |  Input      |                                                         |
 - |  x'         | Floating point input                                    |
 - -------------------------------------------------------------------------
 - |  Output     | True iff cosApprox(x) ~ cos(x)                          |
 - -------------------------------------------------------------------------
 -}
cosApproxProp :: Double -> Bool
cosApproxProp x =
  let
    -- get value of cos for Prelude cos and Student cosApprox
    cosRef = cos x
    cos_x  = Student.cosApprox x
    -- we'll allow for this much margin of error
    tol    = 1e-2
    -- passes test iff | cos(x) - cosApprox(x) | <= tol
  in abs (cosRef - cos_x) <= tol

{- -------------------------------------------------------------------------
 - tanApproxProp
 - -------------------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if Student.tanApprox is accurate
 -    enough by comparing to built-in tan from the Prelude.
 -    Call this utang Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck tanApproxProp
 - -------------------------------------------------------------------------
 - |  Input      |                                                         |
 - |  x'         | Floating point input                                    |
 - -------------------------------------------------------------------------
 - |  Output     | True iff tanApprox(x) ~ tan(x)                          |
 - -------------------------------------------------------------------------
 -}
tanApproxProp :: Double -> Bool
tanApproxProp x =
  let
    -- get value of tan for Prelude cos and Student tanApprox
    tanRef = tan x
    tan_x  = Student.tanApprox x
    -- we'll allow for this much margin of error
    tol    = 1e-2
    -- passes test iff | tan(x) - tanApprox(x) | <= tol
  in abs (tanRef - tan_x) <= tol

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "sinTaylor" $ do
    it "should approx. correspond to sin(x) when x-pi/8 <= a <= x+pi/8" $ property $ sinTaylorProp
  describe "fmod x y = r" $ do
    it "succeeds if " $ property $ fmodProp
  describe "sinApprox" $ do
    it "should correspond to Prelude.sin within a tolerance of 1e-2" $ property $ sinApproxProp
  describe "cosApprox" $ do
    it "should correspond to Prelude.cos within a tolerance of 1e-2" $ property $ cosApproxProp
  describe "tanApprox" $ do
    it "should correspond to Prelude.tan within a tolerance of 1e-2" $ property $ tanApproxProp
