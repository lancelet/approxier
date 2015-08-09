{-|
Module      : Polynomial
Description : Roots of polynomial equations
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Computes the roots of polynomial equations. The root equations (particularly for quartics, which are
very tricky) are not necessarily sufficiently general for all purposes, so apply them cautiously.
-}
module Polynomial (
    quadratic
  , quartic
  ) where

import Data.Complex (Complex ((:+)), imagPart, realPart)

import Data.List    (sort)

import GHC.Float    (double2Float, float2Double)

-- | Computes the real roots of a quadratic equation.
--
--   For the quadratic equation:
--      a*t^2 + b*t + c = 0
--   Finds the real roots where t satisfies the equation. The smaller of the two roots is returned
--   first in the tuple.
quadratic :: (Float, Float, Float)  -- ^ coefficients (a, b, c)
          -> Maybe (Float, Float)   -- ^ roots (t1, t2)
quadratic (a, b, c) = if d <= 0.0 then Nothing else Just (t1, t2)
  where
    d  = (b * b) - (4.0 * a * c)
    t1 = min ta tb
    t2 = max ta tb
    ta = q / a
    tb = c / q
    q | b < 0     = -0.5 * (b - sqrt d)
      | otherwise = -0.5 * (b + sqrt d)

-- | Computes the real roots of a quartic (4th order) equation.
--
--   For the quartic equation:
--     c4*t^4 + c3*t^3 + c2*t^2 + c1*t + c0 = 0
--   Finds the real roots where t satisfies the equation. From zero to four roots are returned in a
--   list, sorted from smallest to largest.
quartic :: Float                                -- ^ imag threshold, below which a root is "real"
        -> (Float, Float, Float, Float, Float)  -- ^ coefficients (c4, c3, c2, c1, c0)
        -> [Float]                              -- ^ list of roots (zero to four elements)
quartic imagThreshold (c4, c3, c2, c1, c0) =
  let d                = float2Double
      (r1, r2, r3, r4) = quarticComplex (d c4, d c3, d c2, d c1, d c0)
      complexRoots     = [ r1, r2, r3, r4 ]
      isReal c         = (abs $ double2Float $ imagPart c) <= (abs imagThreshold)
      in sort $ map (double2Float . realPart) $ filter isReal complexRoots

----------------------------------------------------------------------------------------------------
-- PRIVATE FUNCTIONS

-- | Alias for double-precision complex number.
type CDouble = Complex Double

-- | Computes all four complex roots of a quartic equation.
--
--   Uses the Ferrari method. Reference for this implementation:
--     Szenasi S and Toth A (2015) Solving multiple quartic equations on the GPU using Ferrari's
--       method. SIAM 2015; IEEE 13th International Symposium on Applied Machine Intelligence and
--       Informatics.
--
--   Input parameters are the quartic coefficients, where:
--     c4*t^4 + c3*t^3 + c2*t^2 + c1*t + c0 = 0
--
--   The function operates on double-precision numbers for additional accuracy, since the quartic
--   analytic solution is reported to be rather numerically unstable.
quarticComplex :: (Double, Double, Double, Double, Double)  -- ^ coefficients (c4, c3, c2, c1, c0)
               -> (CDouble, CDouble, CDouble, CDouble)      -- ^ complex roots (t1, t2, t3, t4)
quarticComplex (c4, c3, c2, c1, c0) =
  let fr :: Double -> CDouble  -- fr creates a complex number from its real part
      fr d = d :+ 0
      a  = fr $ (-3)*c3**2 / (8*c4**2) + c2 / c4
      b  = fr $ c3**3 / (8*c4**3) - c3*c2 / (2*c4**2) + c1 / c4
      g  = fr $ (-3)*c3**4 / (256*c4**4) + c2*c3**2 / (16*c4**3) - c3*c1 / (4*c4**2) + c0 / c4
      z1 = fr $ (-c3) / (4*c4)
      p  = (-1)*a**2/12 - g
      q  = (-1)*a**3/108 + a*g/3 - b**2/8
      r  = (-1)*q/2 + sqrt(q**2/4 + p**3/27)
      u  = r ** (1/3)
      y  = if u == (fr 0)
           then (-5)*a/6 + u - q**(1/3)
           else (-5)*a/6 + u - p/(3*u)
      w = sqrt(a + 2*y)
      r1 = z1 + (  w  - sqrt( (-1) * (3*a + 2*y + 2*b/w) )) / 2
      r2 = z1 + (  w  + sqrt( (-1) * (3*a + 2*y + 2*b/w) )) / 2
      r3 = z1 + ((-w) - sqrt( (-1) * (3*a + 2*y - 2*b/w) )) / 2
      r4 = z1 + ((-w) + sqrt( (-1) * (3*a + 2*y - 2*b/w) )) / 2
  in (r1, r2, r3, r4)

