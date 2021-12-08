{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : HaskellExercises08.Exercises08
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 08 - McMaster CS 1JC3 2021
-}
module Exercises08 where

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

data Nat = Succ Nat
         | Zero
  deriving (Show,Eq)

data Signed a = Positive a
              | Negative a
  deriving (Show,Eq)

instance Num (Signed Nat) where
  (+) = addSNat
  (*) = multSNat
  abs = absSNat
  signum n = case n of
               (Positive _) -> Positive $ Succ Zero
               (Negative _) -> Negative $ Succ Zero
  fromInteger = intToSNat
  negate = negateSNat

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement the functions sNatToInt and intToSNat which converts a Signed Nat to an Int and vice versa
-- NOTE these two functions will be tested together
-----------------------------------------------------------------------------------------------------------
sNatToInt :: Signed Nat -> Integer
sNatToInt (Positive n) = case (n) of
                            Succ x -> sNatToInt(Positive x) + 1 
                            Zero -> 0 
sNatToInt (Negative n) = case (n) of
                          Succ x -> sNatToInt(Negative x) - 1 
                          Zero -> 0 


intToSNat :: Integer -> Signed Nat
intToSNat n
  | n == 0     = Negative Zero 
  | n < 0      = Negative $ Succ $ unwrapSign $ intToSNat $ -1 * ((abs n) - 1)
  | otherwise  = Positive $ Succ $ unwrapSign $ intToSNat $ -1 * ((abs n) - 1)
  where
    unwrapSign :: Signed Nat -> Nat
    unwrapSign (Positive n) = n
    unwrapSign (Negative n) = n

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the function addSNat which will add two signed natural numbers
-- DO NOT USE sNatToInt and intToSNat (which would be incredibly inefficient)
-----------------------------------------------------------------------------------------------------------

unwrapSign :: Signed Nat -> Nat
unwrapSign (Positive n) = n
unwrapSign (Negative n) = n

addSNat :: Signed Nat -> Signed Nat -> Signed Nat
addSNat (Positive x) (Positive y) =
  case (x,y) of
    (Succ n,m) -> Positive $ Succ $ unwrapSign $ addSNat (Positive n) (Positive m)
    (Zero,m) -> Positive m 
addSNat (Positive x) (Negative y) = 
  case (x,y) of
    (Succ n,Succ m) -> addSNat (Positive n) (Negative m)
    (Succ n,Zero)   -> Positive $ Succ n
    (Zero,m)        -> Negative m 
addSNat (Negative x) (Positive y) = addSNat (Positive y) (Negative x)
addSNat (Negative x) (Negative y) =
  case (x,y) of
    (Succ n,m) -> Negative $ Succ $ unwrapSign $ addSNat (Negative n) (Negative m)
    (Zero,m)   -> Negative m 

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the function multSNat that multiples two signed numbers
-- NOTE first define functions addNat and multNat :: Nat -> Nat -> Nat
-----------------------------------------------------------------------------------------------------------
multSNat :: Signed Nat -> Signed Nat -> Signed Nat
multSNat n0 n1 =
  let
    addNat (Succ n) m = addNat n (Succ m)
    addNat Zero m     =  m

    multNat (Succ n) m = addNat m (multNat n m)
    multNat Zero m     = Zero
  in case (n0,n1) of
      (Positive x, Positive y) -> Positive $ multNat x y 
      (Negative x, Negative y) -> Positive $ multNat x y 
      (Negative x, Positive y) -> Negative $ multNat x y 
      (Positive x, Negative y) -> Negative $ multNat x y 

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the function absSNat that returns the absolute value of a signed number
-----------------------------------------------------------------------------------------------------------
absSNat :: Signed Nat -> Signed Nat
absSNat n = case n of
            Positive x  -> Positive x
            Negative x  -> Positive x 

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the function negateSNat that returns the negation (i.e. flips the sign) of a signed number
-----------------------------------------------------------------------------------------------------------
negateSNat :: p -> a
negateSNat n = case n of
               Positive x  -> Negative x
               Negative x  -> Positive x 
