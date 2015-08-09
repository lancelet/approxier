{-|

-}
module Trace.Torus (
   torusTracePrim
   ) where

import GHC.Float    (double2Float, float2Double)

import Data.Complex (Complex ((:+)), imagPart, realPart)

import Data.List    (sort)

import VecMath      (XForm, cartesian3Tuple, n3, radians, toVector3, xform, (⋅))

import GPrim        (Torus (torusMajorRadius, torusMinorRadius, torusThetaMin, torusThetaMax,
                            torusPhiMax))

import Trace        (BoundingBox, Intersection (Intersection), Ray (Ray),
                     TracePrim (TracePrim, tpObj2World, tpObjBound, tpWorldBound, tpTrace,
                                tpTraceP),
                     quadricRayEpsilonFactor, rayAt, rayParamIsValid)

torusTracePrim :: XForm -> Torus -> TracePrim
torusTracePrim o2w tor =
  let objBound = torbox tor
  in TracePrim { tpObj2World  = o2w
               , tpObjBound   = objBound
               , tpWorldBound = \x -> xform x objBound
               , tpTrace      = tortrace  tor
               , tpTraceP     = tortracep tor
               }

----------------------------------------------------------------------------------------------------
-- PRIVATE TORUS FUNCTIONS

torbox :: Torus -> BoundingBox
torbox = undefined

tortrace :: Torus -> Ray -> Maybe Intersection
tortrace tor ray =
  let qp    = torqp tor ray
      roots = quartic qp
      chk   = istvalid tor ray
      ts    = filter chk roots
  in case ts of
     []    -> Nothing
     (t:_) -> let iP        = rayAt ray t
                  rr        = torusMajorRadius tor
                  (x, y, z) = cartesian3Tuple iP
                  phi       = atan2 y x
                  rxy       = sqrt(x*x + y*y) - rr
                  theta     = atan2 z rxy
                  iN        = n3 ((cos phi)*(cos theta)) ((sin phi)*(cos theta)) (sin theta)
                  is        = Intersection iP iN undefined t (2 * eps * t)
              in Just is

eps :: Float
eps = quadricRayEpsilonFactor

tortracep :: Torus -> Ray -> Bool
tortracep tor ray = maybe False (const True) $ tortrace tor ray

normAngle :: Float -> Float
normAngle x | x <      0 = normAngle $ x + 2*pi
            | x > (2*pi) = normAngle $ x - 2*pi  -- must be >2*pi, since 360 deg is used as max
            | otherwise  = x

istvalid :: Torus -> Ray -> Float -> Bool
istvalid tor ray t =
  let phiMax    = normAngle $ radians $ torusPhiMax tor
      thetaMin  = normAngle $ radians $ torusThetaMin tor
      thetaMax  = normAngle $ radians $ torusThetaMax tor
      thetaMax' = if (thetaMax < thetaMin) then thetaMax + 2*pi else thetaMax
      rr        = torusMajorRadius tor
      iP        = rayAt ray t
      (x, y, z) = cartesian3Tuple iP
      phi       = normAngle $ atan2 y x
      rxy       = sqrt(x*x + y*y) - rr
      theta     = normAngle $ atan2 z rxy
  in (rayParamIsValid ray t) && (phi < phiMax) && (theta >= thetaMin) && (theta <= thetaMax')

-- | Computes quartic coefficients for a torus - ray intersection.
--
--   Returns the quartic coefficients (c4, c3, c2, c1, c0) where:
--     c4*t^4 + c3*t^3 + c2*t^2 + c1*t + c0 = 0
torqp :: Torus -> Ray -> (Float, Float, Float, Float, Float)
torqp tor ray =
  let
    -- inputs
    rr          = torusMajorRadius tor
    r           = torusMinorRadius tor
    Ray p v _ _ = ray
    pv          = toVector3 p
    (px, py, _) = cartesian3Tuple p
    (vx, vy, _) = cartesian3Tuple v
    -- intermediate values
    r2  = r*r
    rr2 = rr*rr
    a   = (pv ⋅ pv) + rr2 - r2
    b   = 2 * (pv ⋅ v)
    c   = v ⋅ v
    d   = px*px + py*py
    e   = 2 * (px*vx + py*vy)
    h   = vx*vx + vy*vy
    -- quartic coefficients
    c4 = c*c
    c3 = 2*b*c
    c2 = 2*a*c + b*b - 4*rr2*h
    c1 = 2*a*b - 4*rr2*e
    c0 = a*a - 4*rr2*d
  in
    (c4, c3, c2, c1, c0)

type CDouble = Complex Double

-- | Computes the real roots of a quartic (zero to four real roots).
--
--   The roots are returned in ascending order.
quartic :: (Float, Float, Float, Float, Float)
        -> [Float]
quartic (c4, c3, c2, c1, c0) =
  let d                = float2Double
      (r1, r2, r3, r4) = quarticComplex (d c4, d c3, d c2, d c1, d c0)
      cmplxRoots       = [ r1, r2, r3, r4 ]
      isReal c         = (abs $ imagPart c) <= 1e-10
  in sort $ map (double2Float . realPart) $ filter isReal cmplxRoots

-- | Computes the four complex roots of a quartic using the Ferrari method.
--
--   Reference:
--     Szenasi S and Toth A (2015) Solving multiple quartic equations on the GPU using Ferrari's
--       method. SIAM 2015; IEEE 13th International Symposium on Applied Machine Intelligence and
--       Informatics.
--
--   Input parameters are the quartic coefficients (c4, c3, c2, c1, c0) where:
--       c4*t^4 + c3*t^3 + c2*t^2 + c1*t + c0 = 0
--
--   Operates on double-precision numbers for additional accuracy.
quarticComplex :: (Double, Double, Double, Double, Double)
               -> (CDouble, CDouble, CDouble, CDouble)
quarticComplex (c4, c3, c2, c1, c0) =
  let fr :: Double -> CDouble
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
  in if b == (fr 0)  -- if b is zero
     then let rt = sqrt(a**2 - 4*g)
              r1 = z1 + sqrt( ((-a) + rt) / 2 )
              r2 = z1 - sqrt( ((-a) + rt) / 2 )
              r3 = z1 + sqrt( ((-a) - rt) / 2 )
              r4 = z1 - sqrt( ((-a) - rt) / 2 )
          in (r1, r2, r3, r4)
     else let r1 = z1 + (  w  - sqrt( (-1) * (3*a + 2*y + 2*b/w) )) / 2
              r2 = z1 + (  w  + sqrt( (-1) * (3*a + 2*y + 2*b/w) )) / 2
              r3 = z1 + ((-w) - sqrt( (-1) * (3*a + 2*y - 2*b/w) )) / 2
              r4 = z1 + ((-w) + sqrt( (-1) * (3*a + 2*y - 2*b/w) )) / 2
          in (r1, r2, r3, r4)
