{-|
Module      : Shapes.Sphere
Description : Sphere
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Geometric descriptions of a sphere.

TODO: compute v param correctly
-}
module Shapes.Sphere (
    Sphere(
        Sphere
      , sphereRadius
      , sphereZMin
      , sphereZMax
      , spherePhiMax
    )
  , traceableSphere
  , boundedSphere
  ) where

import Bound      (BoundingBox (BoundingBox), BoundingBoxed, mkBoundingBoxed)

import Polynomial (quadratic)

import Trace      (Intersection (Intersection), Ray, Traceable, mkTraceable,
                   quadricRayEpsilonFactor, rayAt, rayParamIsValid, rayPoint, rayVector)

import VecMath    (UVCoord (UVCoord), cartesian3Tuple, normAngle1, radians, toNormal3, toVector3,
                   (⋅))

----------------------------------------------------------------------------------------------------
-- SPHERE

data Sphere = Sphere
  { sphereRadius :: !Float
  , sphereZMin   :: !Float
  , sphereZMax   :: !Float
  , spherePhiMax :: !Float
  } deriving (Show)

traceableSphere :: Sphere -> Traceable
traceableSphere = mkTraceable . sphereTrace

boundedSphere :: Sphere -> BoundingBoxed
boundedSphere = mkBoundingBoxed . sphereBound

----------------------------------------------------------------------------------------------------
-- PRIVATE SPHERE FUNCTIONS

sphereBound :: Sphere -> BoundingBox
sphereBound Sphere { sphereRadius = r } = BoundingBox (-r) (-r) (-r) r r r

sphereTrace :: Sphere -> Ray -> Maybe Intersection
sphereTrace sphere @ Sphere { sphereRadius = r } ray =
  let
    -- inputs
    p  = (toVector3 . rayPoint) ray
    v  = rayVector              ray
    -- ray parametric value at intersection points
    ts = quadratic (v ⋅ v, 2*(p ⋅ v), p ⋅ p - r*r)
  in fmap fst ts >>= intersection sphere ray

intersection :: Sphere -> Ray -> Float -> Maybe Intersection
intersection sphere ray t =
  let p         = rayAt ray t
      n         = toNormal3 p
      (x, y, z) = cartesian3Tuple p
      phi       = normAngle1 $ atan2 y x
      phimax    = (radians . spherePhiMax) sphere
      u         = phi / phimax
      v         = undefined -- TODO: compute v parameter correctly
      eps       = t * quadricRayEpsilonFactor
  in
    if paramsValid sphere ray z phi t
    then Just $ Intersection p n (UVCoord u v) ray t eps
    else Nothing

paramsValid :: Sphere
            -> Ray
            -> Float    -- ^ z
            -> Float    -- ^ phi radians
            -> Float    -- ^ t
            -> Bool
paramsValid (Sphere _ zmin zmax phimaxd) ray z phi t =
     ( rayParamIsValid ray t
    && z   >= zmin
    && z   <= zmax
    && phi <= radians phimaxd
     )
