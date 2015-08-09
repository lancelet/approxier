{-|

-}
module Trace (
    Ray(
        Ray
      , rayPoint
      , rayVector
      , rayMinParam
      , rayMaxParam
    )
  , rayAt
  , rayParamIsValid
  , Intersection(
      Intersection
    , isectP
    , isectN
    , isectUV
    , isectRay
    , isectRayParam
    , isectRayEpsilon
    )
  , Traceable(
        Traceable
      , trace
      , traceHit
    )
  , mkTraceable
  , quadricRayEpsilonFactor
  ) where

import VecMath (Normal3, Point3, Transform (xform), UVCoord, Vector3, toPoint3, toVector3, (.*),
                (.*))

----------------------------------------------------------------------------------------------------
-- RAY STUFF

-- |Ray.
data Ray = Ray {
    rayPoint    :: !Point3   -- ^ starting point for the ray
  , rayVector   :: !Vector3  -- ^ vector in the direction of the ray
  , rayMinParam :: !Float    -- ^ minimum parametric value (units of ray vector length)
  , rayMaxParam :: !Float    -- ^ maximum parametric value (units of ray vector length)
  } deriving (Show)

instance Transform Ray where
  xform x (Ray p v tmin tmax) = Ray (xform x p) (xform x v) tmin tmax

-- |Evaluates a ray at a given parametric coordinate.
rayAt :: Ray -> Float -> Point3
rayAt (Ray p v _ _) t =
  let pvec = toVector3 p
  in toPoint3 $ pvec + (v .* t)

-- |Checks if a given ray parameter (t) falls inside the allowed range.
rayParamIsValid :: Ray -> Float -> Bool
rayParamIsValid (Ray _ _ tmin tmax) t = (t >= tmin) && (t <= tmax)

----------------------------------------------------------------------------------------------------
-- INTERSECTION WITH A PRIMITIVE

-- |Intersection between a ray and a primitive.
data Intersection = Intersection {
    isectP          :: Point3
  , isectN          :: Normal3
  , isectUV         :: UVCoord
  , isectRay        :: Ray
  , isectRayParam   :: Float
  , isectRayEpsilon :: Float
  }

instance Transform Intersection where
  xform x (Intersection p n uv ray rp re) =
    Intersection (xform x p) (xform x n) uv (xform x ray) rp re

----------------------------------------------------------------------------------------------------
-- TRACEABLE THING

-- | Traceable.
--
--   Something is traceable if a ray can be fired against it and some intersection data found.
--   Traceable things exist in their own coordinate system (because they know of no other!). Thus,
--   rays and intersections are all implicitly in "object space" for a traceable.
data Traceable = Traceable {
    -- | Traces a ray against an object.
    trace    :: Ray -> Maybe Intersection

    -- | Checks if a ray strikes an object
  , traceHit :: Ray -> Bool
  }

-- | Default implementation of a 'traceHit' function, using an existing 'trace' function.
defaultTraceHit :: (Ray -> Maybe Intersection) -> Ray -> Bool
defaultTraceHit t r = maybe False (const True) (t r)

-- | Creates a traceable from only a ray-intersection function, providing a default implementation
--   of traceHit.
mkTraceable :: (Ray -> Maybe Intersection) -> Traceable
mkTraceable f = Traceable {
    trace    = f
  , traceHit = defaultTraceHit f
  }

----------------------------------------------------------------------------------------------------
-- TOP-LEVEL STUFF

-- |Factor used to scale ray intersections for quadrics.
quadricRayEpsilonFactor :: Float
quadricRayEpsilonFactor = 5.0e-4
