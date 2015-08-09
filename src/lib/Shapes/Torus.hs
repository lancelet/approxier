{-|
Module      : Shapes.Torus
Description : Torus
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Geometric descriptions of a torus.

TODO: bounding box
TODO: u,v coordinates
-}
module Shapes.Torus (
    Torus (
        Torus
      , torusMajorRadius
      , torusMinorRadius
      , torusThetaMin
      , torusThetaMax
      , torusPhiMax
    )
  , traceableTorus
  , boundedTorus
  ) where

import Bound         (BoundingBox, BoundingBoxed, mkBoundingBoxed)

import Control.Monad (msum)

import Polynomial    (quartic)

import Trace         (Intersection (Intersection), Ray, Traceable, mkTraceable,
                      quadricRayEpsilonFactor, rayAt, rayParamIsValid, rayPoint, rayVector)

import VecMath       (UVCoord (UVCoord), cartesian3Tuple, n3, normAngle1, radians, toVector3, (⋅))

----------------------------------------------------------------------------------------------------
-- TRACEABLE TORUS

-- |Torus.
data Torus = Torus
  { torusMajorRadius :: !Float
  , torusMinorRadius :: !Float
  , torusThetaMin    :: !Float
  , torusThetaMax    :: !Float
  , torusPhiMax      :: !Float
  } deriving (Show)

traceableTorus :: Torus -> Traceable
traceableTorus = mkTraceable . torTrace

boundedTorus :: Torus -> BoundingBoxed
boundedTorus = mkBoundingBoxed . torBound

----------------------------------------------------------------------------------------------------
-- PRIVATE TORUS FUNCTIONS

torBound :: Torus -> BoundingBox
torBound = undefined

torTrace :: Torus -> Ray -> Maybe Intersection
torTrace tor ray =
  let ts = quartic quadricRayEpsilonFactor $ torqp tor ray
  in msum $ map (intersection tor ray) ts

intersection :: Torus -> Ray -> Float -> Maybe Intersection
intersection tor ray t =
  let p         = rayAt ray t
      (x, y, z) = cartesian3Tuple p
      rR        = torusMajorRadius tor
      rxy       = sqrt(x*x + y*y) - rR
      phi       = normAngle1 $ atan2 y x
      theta     = normAngle1 $ atan2 z rxy
      n         = n3 ((cos theta)*(cos phi)) ((cos theta)*(sin phi)) (sin theta)
      u         = undefined
      v         = undefined
      eps       = 2 * t * quadricRayEpsilonFactor
  in
    if paramsValid tor ray phi theta t
    then Just $ Intersection p n (UVCoord u v) ray t eps
    else Nothing

paramsValid :: Torus
            -> Ray
            -> Float  -- ^ phi radians
            -> Float  -- ^ theta radians
            -> Float  -- ^ t
            -> Bool
paramsValid tor ray phi theta t =
  let phimax    = (normAngle1 . radians . torusPhiMax)   tor
      thetamin  = (normAngle1 . radians . torusThetaMin) tor
      thetamax  = (normAngle1 . radians . torusThetaMax) tor
      thetamax' = if (thetamax < thetamin) then thetamax + 2*pi else thetamax
  in  ( rayParamIsValid ray t
     && phi   <= phimax
     && theta >= thetamin
     && theta <= thetamax'
      )

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
    p           = rayPoint ray
    v           = rayVector ray
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
