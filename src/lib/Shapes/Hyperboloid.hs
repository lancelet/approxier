{-|
Module      : Shapes.Hyperboloid
Description : Hyperboloid
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Geometric descriptions of a hyperboloid.

TODO: Compute bounding box.
TODO: Compute u,v coordinates correctly.
-}
module Shapes.Hyperboloid (
    Hyperboloid (
        Hyperboloid
      , hyperboloidPoint1
      , hyperboloidPoint2
      , hyperboloidPhiMax
    )
  , traceableHyperboloid
  , boundedHyperboloid
  ) where

import Bound      (BoundingBox, BoundingBoxed, mkBoundingBoxed)

import Polynomial (quadratic)

import Trace      (Intersection (Intersection), Ray, Traceable, mkTraceable, quadricRayEpsilonFactor,
                   rayAt, rayParamIsValid, rayPoint, rayVector)

import VecMath    (Point3, UVCoord (UVCoord), Vector3, cartesian3Tuple, normAngle1,
                   offsetPointAlongVector, radians, toNormal3, toVector3, v3, zcomp3, (⨯))

----------------------------------------------------------------------------------------------------
-- HYPERBOLOID

data Hyperboloid = Hyperboloid
  { hyperboloidPoint1 :: !Point3
  , hyperboloidPoint2 :: !Point3
  , hyperboloidPhiMax :: !Float
  } deriving (Show)

traceableHyperboloid :: Hyperboloid -> Traceable
traceableHyperboloid = mkTraceable . hypTrace

boundedHyperboloid :: Hyperboloid -> BoundingBoxed
boundedHyperboloid = mkBoundingBoxed . hypBound

----------------------------------------------------------------------------------------------------
-- PRIVATE HYPERBOLOID FUNCTIONS

hypBound :: Hyperboloid -> BoundingBox
hypBound = undefined

hypTrace :: Hyperboloid -> Ray -> Maybe Intersection
hypTrace hyp ray =
  let ts = quadratic $ hypqp hyp ray
  in fmap fst ts >>= intersection hyp ray

-- | Hyperboloid quadratic equation parameters.
--
--   These are the coefficients (c2, c1, c0) in the equation:
--     c2*t^2 + c1*t + c0 = 0
--   Which when solved for t gives the intersection points of the ray with the hyperboloid.
hypqp :: Hyperboloid -> Ray -> (Float, Float, Float)
hypqp hyp ray =
  let (a, c)       = implicitParams hyp
      (px, py, pz) = (cartesian3Tuple . rayPoint)  ray
      (vx, vy, vz) = (cartesian3Tuple . rayVector) ray
      aa           = a*(vx*vx + vy*vy) - c*vz*vz
      bb           = 2.0 * (a*(px*vx + py*vy) - c*pz*vz)
      cc           = a*(px*px + py*py) - c*pz*pz - 1.0
  in (aa, bb, cc)

-- | Hyperboloid implicit parameters (a, c).
--   These are the parameters in the hyperboloid implicit equation:
--      a*x^2 + a*y^2 - c*z^2 = 0
implicitParams :: Hyperboloid -> (Float, Float)
implicitParams hyp
  | (abs . zcomp3 . hyperboloidPoint1) hyp < eps = (implicitParams . slidep1p2) hyp
  | (abs . adenom) hyp < eps                     = (implicitParams . slidep2)   hyp
  | otherwise                                    = (a, c)
  where
    (x1, y1, z1) = (cartesian3Tuple . hyperboloidPoint1) hyp
    ( _,  _, z2) = (cartesian3Tuple . hyperboloidPoint2) hyp
    a            = (1 - (z2*z2) / (z1*z1)) / (adenom hyp)
    c            = (a*x1*x1 + a*y1*y1 - 1) / (z1*z1)

-- | Denominator in the expression for the hyperboloid parameter a.
adenom :: Hyperboloid -> Float
adenom hyp =
  let (x1, y1, z1) = cartesian3Tuple $ hyperboloidPoint1 hyp
      (x2, y2, z2) = cartesian3Tuple $ hyperboloidPoint2 hyp
  in x2*x2 + y2*y2 - (z2*z2) / (z1*z1) * (x1*x1 + y1*y1)

-- | Slides both points of a hyperboloid along the p1-p2 vector.
slidep1p2 :: Hyperboloid -> Hyperboloid
slidep1p2 hyp =
  let ofs  = offsetPointAlongVector (hypvector hyp) 1.0
      pt1  = hyperboloidPoint1 hyp
      pt2  = hyperboloidPoint2 hyp
      pt1' = ofs pt1
      pt2' = ofs pt2
  in hyp { hyperboloidPoint1 = pt1'
         , hyperboloidPoint2 = pt2' }

-- | Slides just point p2 along the p1-p2 vector.
slidep2 :: Hyperboloid -> Hyperboloid
slidep2 hyp =
  let p2  = hyperboloidPoint2 hyp
      p2' = offsetPointAlongVector (hypvector hyp) 1.0 p2
  in hyp { hyperboloidPoint2 = p2' }

-- |Returns the p1-p2 vector of a hyperboloid.
hypvector :: Hyperboloid -> Vector3
hypvector hyp =
  let v1 = toVector3 $ hyperboloidPoint1 hyp
      v2 = toVector3 $ hyperboloidPoint2 hyp
  in v2 - v1

eps :: Float
eps = quadricRayEpsilonFactor

intersection :: Hyperboloid -> Ray -> Float -> Maybe Intersection
intersection hyp ray t =
  let
    p               = rayAt ray t
    (x, y, z)       = cartesian3Tuple p
    p1              = (toVector3 . hyperboloidPoint1) hyp
    p2              = (toVector3 . hyperboloidPoint2) hyp
    (dpx, dpy, dpz) = cartesian3Tuple (p2 - p1)
    phi             = normAngle1 $ atan2 y x
    c               = cos phi
    s               = sin phi
    dpduu           = v3 (-y) x 0
    dpdv            = v3 (dpx*c - dpy*s) (dpx*s + dpy*c) (dpz)
    n               = toNormal3 $ dpduu ⨯ dpdv
    u               = undefined -- TODO
    v               = undefined -- TODO
  in
    if paramsValid hyp ray z phi t
    then Just $ Intersection p n (UVCoord u v) ray t (t*eps)
    else Nothing

paramsValid :: Hyperboloid
            -> Ray
            -> Float         -- ^ z
            -> Float         -- ^ phi radians
            -> Float         -- ^ t
            -> Bool
paramsValid hyp ray z phi t =
  let z1     = (zcomp3 . hyperboloidPoint1) hyp
      z2     = (zcomp3 . hyperboloidPoint2) hyp
      phimax = (radians . hyperboloidPhiMax) hyp
      zmin   = min z1 z2
      zmax   = max z1 z2
  in ( rayParamIsValid ray t
    && z   >= zmin
    && z   <= zmax
    && phi <= phimax
     )
