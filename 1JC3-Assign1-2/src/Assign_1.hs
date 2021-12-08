{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 - McMaster CS 1JC3 2021
-}
module Assign_1 where

import Prelude hiding (sin,cos,tan)

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
macid :: String
macid = "Guerinoa"


{- -----------------------------------------------------------------
 - factorial
 - -----------------------------------------------------------------
 - Description:
 -    Computes the factorial of any Integer n
 - -----------------------------------------------------------------
 - |   Input     |                                                 |
 - |      n      | Integer input                                   |
 - -----------------------------------------------------------------
 - |   Output    |                                                 |
 - |      n <= 1 | 1                                               |
 - |      n >  1 | n * (n-1) ...  * 1   while (n-k) > 0            |
 - -----------------------------------------------------------------
 -}
factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- ------------------------------------------------------------------
 - sinTaylor
 - ------------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
sinTaylor :: Double -> Double -> Double -> Double -> Double
sinTaylor a cos_a sin_a x = sin_a * fact 0  +
                            cos_a * fact 1  -
                            sin_a * fact 2  -
                            cos_a * fact 3 +
                            sin_a * fact 4
                            where fact e = (1 / fromInteger(factorial e)) * (x-a)^e

{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description:
 -}
fmod :: Double -> Double -> Double
fmod x y =
  let
    -- z is the largest integer s.t. z*y <= x
    -- HINT use floating point division, then round down
    z = fromIntegral(floor(x / y))
  in x - z*y


{- --------------------------------------------------------------------------------------------------
 - sinApprox
 - --------------------------------------------------------------------------------------------------
 - Description:

 - --------------------------------------------------------------------------------------------------
 -}

sinApprox :: Double -> Double
sinApprox x
  | 0        <= funcMod && funcMod  < (pi/4)   = sinTaylor 0 1 0 funcMod
  | (pi/4)   <= funcMod && funcMod  < (3*pi/4) = sinTaylor (pi/2) 0 1 funcMod
  | (3*pi/4) <= funcMod && funcMod  < (5*pi/4) = sinTaylor pi (-1) 0 funcMod
  | (5*pi/4) <= funcMod && funcMod  < (7*pi/4) = sinTaylor (3*pi/2) 0 (-1) funcMod
  | (7*pi/4) <= funcMod && funcMod  < (2*pi)   = sinTaylor (2*pi) 1 0 funcMod
  | otherwise = error "Error here! "
  where  funcMod = fmod x (2*pi)
{- ---------------------------------------------------------------------
 - cosApprox
 - ---------------------------------------------------------------------

 -}
cosApprox :: Double -> Double
cosApprox x = - sinApprox (x - pi/2)

{- ---------------------------------------------------------------------
 - tanApprox
 - ---------------------------------------------------------------------
 - ---------------------------------------------------------------------
 -}
tanApprox :: Double -> Double
tanApprox x = sinApprox x / cosApprox x
