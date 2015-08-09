{-|

-}
module Trace.Hyperboloid (
  hyperboloidTracePrim
  ) where

import VecMath (Cartesian3Tuple(cartesian3Tuple), v3, toVector3, toNormal3,
                offsetPointAlongVector, zcomp, XForm, xform, (⨯),
                degrees, Vector3)

import GPrim (Hyperboloid(hyperboloidPoint1, hyperboloidPoint2, hyperboloidPhiMax))

import Trace (Ray(Ray), quadratic, rayAt,
              Intersection(Intersection), rayParamIsValid, quadricRayEpsilonFactor, BoundingBox,
              TracePrim(TracePrim, tpObj2World, tpObjBound, tpWorldBound, tpTrace, tpTraceP))

hyperboloidTracePrim :: XForm -> Hyperboloid -> TracePrim
hyperboloidTracePrim o2w hyp =
  let objBound = hypbbox hyp
  in TracePrim { tpObj2World  = o2w
               , tpObjBound   = objBound
               , tpWorldBound = \x -> xform x objBound
               , tpTrace      = hyptrace  hyp
               , tpTraceP     = hyptracep hyp
               }

----------------------------------------------------------------------------------------------------
-- PRIVATE HYPERBOLOID FUNCTIONS

hypbbox :: Hyperboloid -> BoundingBox
hypbbox = undefined

hyptrace :: Hyperboloid -> Ray -> Maybe Intersection
hyptrace hyp ray =
  let isectFn = isect hyp ray
  in takeFirstIntersection $ mapMaybeTuple isectFn $ quadratic $ hypqp hyp ray

hyptracep :: Hyperboloid -> Ray -> Bool
hyptracep hyp ray = maybe False (const True) $ hyptrace hyp ray

mapMaybeTuple :: (a -> Maybe b) -> Maybe (a, a) -> (Maybe b, Maybe b)
mapMaybeTuple f (Just (x, y)) = (f x, f y)
mapMaybeTuple _       Nothing = (Nothing, Nothing)

takeFirstIntersection :: (Maybe Intersection, Maybe Intersection) -> Maybe Intersection
takeFirstIntersection (l@(Just _),          _) = l
takeFirstIntersection (         _, r@(Just _)) = r
takeFirstIntersection                        _ = Nothing

isect :: Hyperboloid -> Ray -> Float -> Maybe Intersection
isect hyp ray t =
  let p1              = hyperboloidPoint1 hyp
      p2              = hyperboloidPoint2 hyp
      (_, _, z1)      = cartesian3Tuple p1
      (_, _, z2)      = cartesian3Tuple p2
      phiMax          = hyperboloidPhiMax hyp
      dp              = (toVector3 p2) - (toVector3 p1)
      (dpx, dpy, dpz) = cartesian3Tuple dp
      iP              = rayAt ray t
      (x, y, z)       = cartesian3Tuple iP
      phi'            = atan2 y x
      phi             = if phi' < 0.0 then phi' + 2*pi else phi'
      phid            = degrees phi
      c               = cos phi
      s               = sin phi
      dpduu           = v3 (-y) x 0
      dpdv            = v3 (dpx*c - dpy*s) (dpx*s + dpy*c) (dpz)
      iN              = toNormal3 $ dpduu ⨯ dpdv
      zMin            = min z1 z2
      zMax            = max z1 z2
      isValid         = (rayParamIsValid ray t) && (z >= zMin) && (z <= zMax) && (phid < phiMax)
      is              = Intersection iP iN t (eps * t)
  in if isValid then Just is else Nothing

-- |Finds the quadratic parameters of a hyperboloid.
hypqp :: Hyperboloid -> Ray -> (Float, Float, Float)
hypqp hyp ray =
  let (a, c)       = hypip hyp
      Ray p v _ _  = ray
      (px, py, pz) = cartesian3Tuple $ p
      (vx, vy, vz) = cartesian3Tuple $ v
      aa           = a*(vx*vx + vy*vy) - c*vz*vz
      bb           = 2.0 * (a*(px*vx + py*vy) - c*pz*vz)
      cc           = a*(px*px + py*py) - c*pz*pz - 1.0
  in (aa, bb, cc)

-- |Finds the implicit parameters of a hyperboloid (a, c).
hypip :: Hyperboloid -> (Float, Float)
hypip hyp | (abs $ zcomp $ hyperboloidPoint1 hyp) < eps = hypip $ slidep1p2 hyp
          | (abs $ adenom hyp) < eps                    = hypip $ slidep2   hyp
          | otherwise =
              let (x1, y1, z1) = cartesian3Tuple $ hyperboloidPoint1 hyp
                  ( _,  _, z2) = cartesian3Tuple $ hyperboloidPoint2 hyp
                  a            = (1.0 - (z2*z2) / (z1*z1)) / (adenom hyp)
                  c            = (a*x1*x1 + a*y1*y1 - 1.0) / (z1*z1)
              in (a,c)

-- |Shortened alias for quadricRayEpsilonFactor.
eps :: Float
eps = quadricRayEpsilonFactor

-- |Denominator in the expression for hyperboloid parameter a.
-- This cannot be zero.
adenom :: Hyperboloid -> Float
adenom hyp =
  let (x1, y1, z1) = cartesian3Tuple $ hyperboloidPoint1 hyp
      (x2, y2, z2) = cartesian3Tuple $ hyperboloidPoint2 hyp
  in x2*x2 + y2*y2 - (z2*z2) / (z1*z1) * (x1*x1 + y1*y1)

-- |Slides both points of a hyperboloid along the p1-p2 vector.
slidep1p2 :: Hyperboloid -> Hyperboloid
slidep1p2 hyp =
  let ofs  = offsetPointAlongVector (hypv hyp) 1.0
      pt1  = hyperboloidPoint1 hyp
      pt2  = hyperboloidPoint2 hyp
      pt1' = ofs pt1
      pt2' = ofs pt2
  in hyp { hyperboloidPoint1 = pt1'
         , hyperboloidPoint2 = pt2' }

-- |Slides just point p2 along the p1-p2 vector.
slidep2 :: Hyperboloid -> Hyperboloid
slidep2 hyp =
  let p2  = hyperboloidPoint2 hyp
      p2' = offsetPointAlongVector (hypv hyp) 1.0 p2
  in hyp { hyperboloidPoint2 = p2' }

-- |Returns the p1-p2 vector of a hyperboloid.
hypv :: Hyperboloid -> Vector3
hypv hyp =
  let v1 = toVector3 $ hyperboloidPoint1 hyp
      v2 = toVector3 $ hyperboloidPoint2 hyp
  in v2 - v1
