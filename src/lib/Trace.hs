{-|

-}
module Trace (
    Ray(Ray)
  , rayAt
  , RayParametricRange(RayParametricRange)
  , rayParamIsValid
  , Intersection(
      Intersection
    , isectP
    , isectN
    , isectRayParam
    , isectRayEpsilon
    )
  , BoundingBox(BoundingBox)
  , TracePrim(
      TracePrim
    , tpObj2World
    , tpObjBound
    , tpWorldBound
    , tpTrace
    , tpTraceP
    )
  , trace
  , brainDeadTraceGroup
  , nullTracePrim
  , quadratic
  , quadricRayEpsilonFactor
  , sphereTracePrim
  ) where

import VecMath    (Normal3, Point3, Transformable (xform), Vector3, XForm, p3, xcomp, xformId,
                   ycomp, zcomp, xformInv, lengthSquared, toVector3, toNormal3, (⋅), (.*), toPoint3,
                   degrees, xformCompose, (.*))
import GPrim      (Sphere(sphereRadius, sphereZMin, sphereZMax, spherePhiMax))
import Data.Foldable (asum)
import Data.List  (foldl', minimumBy)
import Data.Maybe (catMaybes)


-- |Ray.
data Ray = Ray {-# UNPACK #-} !Point3 !Vector3 deriving (Show)

instance Transformable Ray where
  xform x (Ray p v) = Ray (xform x p) (xform x v)

-- |Evaluates a ray at a given parametric coordinate.
rayAt :: Ray -> Float -> Point3
rayAt (Ray p v) t =
  let pvec = toVector3 p
  in toPoint3 $ pvec + (v .* t)

-- |Parametric range for a ray.
data RayParametricRange = RayParametricRange {-# UNPACK #-} !Float !Float deriving (Show)

-- |Checks if a given ray parameter (t) falls inside the allowed range.
rayParamIsValid :: RayParametricRange -> Float -> Bool
rayParamIsValid (RayParametricRange tmin tmax) t = (t >= tmin) && (t <= tmax)

----------------------------------------------------------------------------------------------------
-- INTERSECTION WITH A PRIMITIVE

-- |Intersection between a ray and a primitive.
data Intersection = Intersection {
    isectP          :: Point3
  , isectN          :: Normal3
  , isectRayParam   :: Float
  , isectRayEpsilon :: Float
  }

instance Transformable Intersection where
  xform x (Intersection p n rp re) = Intersection (xform x p) (xform x n) rp re

-- |Intersections can be ordered according to the ray parameter.
intersectionOrdering :: Intersection -> Intersection -> Ordering
intersectionOrdering ia ib = compare (isectRayParam ia) (isectRayParam ib)

-- |Returns the first of a possible set of intersections.
firstIntersection :: [ Maybe Intersection ] -> Maybe Intersection
firstIntersection mis =
  let
    is = catMaybes mis
  in case is of
    []  -> Nothing
    isl -> Just $ minimumBy intersectionOrdering isl

----------------------------------------------------------------------------------------------------
-- BOUNDING BOXES

-- |Bounding box for primitives.
data BoundingBox = BoundingBox
                   !Float  -- ^ min X
                   !Float  -- ^ min Y
                   !Float  -- ^ min Z
                   !Float  -- ^ max X
                   !Float  -- ^ max Y
                   !Float  -- ^ max Z
                   deriving (Show)

-- |Takes the union of two bounding boxes.
bboxUnion :: BoundingBox -> BoundingBox -> BoundingBox
bboxUnion a b =
  let
    BoundingBox aXmin aYmin aZmin aXmax aYmax aZmax = a
    BoundingBox bXmin bYmin bZmin bXmax bYmax bZmax = b
    xmin = min aXmin bXmin
    ymin = min aYmin bYmin
    zmin = min aZmin bZmin
    xmax = max aXmax bXmax
    ymax = max aYmax bYmax
    zmax = max aZmax bZmax
  in BoundingBox xmin ymin zmin xmax ymax zmax

-- |Returns the 8 corners of a bounding box.
bboxCorners :: BoundingBox -> [ Point3 ]
bboxCorners b =
  let
    BoundingBox xmin ymin zmin xmax ymax zmax = b
    pt1 = p3 xmin ymin zmin
    pt2 = p3 xmin ymax zmin
    pt3 = p3 xmax ymin zmin
    pt4 = p3 xmax ymax zmin
    pt5 = p3 xmin ymin zmax
    pt6 = p3 xmin ymax zmax
    pt7 = p3 xmax ymin zmax
    pt8 = p3 xmax ymax zmax
  in [ pt1, pt2, pt3, pt4, pt5, pt6, pt7, pt8 ]

-- |Returns a bounding box that precisely bounds a list of points.
boundPts :: [ Point3 ] -> BoundingBox
boundPts pts =
  let
    xs   = map xcomp pts
    ys   = map ycomp pts
    zs   = map zcomp pts
    minx = minimum xs
    miny = minimum ys
    minz = minimum zs
    maxx = maximum xs
    maxy = maximum ys
    maxz = maximum zs
  in BoundingBox minx miny minz maxx maxy maxz

-- |Bounding boxes are transformable by transforming their corner points.
instance Transformable BoundingBox where
  xform x = boundPts . (map (xform x)) . bboxCorners

----------------------------------------------------------------------------------------------------
-- TRACEABLE PRIMITIVE

-- |A traceable primitive.
data TracePrim = TracePrim
  { tpObj2World  :: XForm
  , tpObjBound   :: BoundingBox
  , tpWorldBound :: XForm -> BoundingBox
  , tpTrace      :: RayParametricRange -> Ray -> Maybe Intersection
  , tpTraceP     :: RayParametricRange -> Ray -> Bool
  }

instance Show TracePrim where
  show (TracePrim ow ob _ _ _) = "TracePrim " ++ (show ow) ++ " " ++ (show ob) ++ " "

-- |Trace a primitive in world coordinates.
trace :: TracePrim -> RayParametricRange -> Ray -> Maybe Intersection
trace tp rpr ray =
  let
    o2w = xform $ tpObj2World tp
    w2o = xform $ xformInv $ tpObj2World tp
    tpt = tpTrace tp
  in fmap o2w $ tpt rpr $ w2o ray

-- |Find the world-space bounding box of a primitive.
worldBoundingBox :: TracePrim -> BoundingBox
worldBoundingBox tp = tpWorldBound tp $ tpObj2World tp

-- |A trace primitive that is completely invisible.
nullTracePrim :: TracePrim
nullTracePrim = TracePrim
                { tpObj2World  = xformId
                , tpObjBound   = BoundingBox 0 0 0 0 0 0
                , tpWorldBound = const $ BoundingBox 0 0 0 0 0 0
                , tpTrace      = noTrace
                , tpTraceP     = noTraceP
                }
  where
    noTrace  _ _ = Nothing
    noTraceP _ _ = False

instance Transformable TracePrim where
  xform x tp =
    let newTp = tp { tpObj2World = xformCompose (tpObj2World tp) x }
    in newTp

----------------------------------------------------------------------------------------------------
-- SCENE (GROUP OF TRACEABLE PRIMITIVES)

-- |Simplest possible implementation of a group of traceprims that can constitute a scene.
-- This implementation just tracks over the list of TracePrims without any spatial indexing.
brainDeadTraceGroup :: [ TracePrim ] -> TracePrim
brainDeadTraceGroup [] = nullTracePrim
brainDeadTraceGroup prims@(p:ps) = groupTracePrim
  where
    groupTracePrim = TracePrim
                     { tpObj2World  = xformId
                     , tpObjBound   = objBound
                     , tpWorldBound = worldBound
                     , tpTrace      = tracetp
                     , tpTraceP     = tracetpP
                     }
    objBound = foldl' bboxUnion (worldBoundingBox p) (map worldBoundingBox ps)
    worldBound x = xform x objBound
    tracetp rpr ray  = firstIntersection $ map (\prim -> (trace prim) rpr ray) prims
    tracetpP _ _ = undefined

----------------------------------------------------------------------------------------------------
-- TRACEABLE QUADRICS

-- |Factor used to scale ray intersections for quadrics.
quadricRayEpsilonFactor :: Float
quadricRayEpsilonFactor = 5.0e-4

-- |Makes a traceable primitive from a transformed sphere.
sphereTracePrim :: XForm -> Sphere -> TracePrim
sphereTracePrim obj2World sphere =
  let
    objBound = sphereObjBoundingBox sphere
  in TracePrim {
      tpObj2World  = obj2World
    , tpObjBound   = objBound
    , tpWorldBound = \x -> xform x objBound
    , tpTrace      = sphereTrace sphere
    , tpTraceP     = sphereTraceP sphere
    }

-- |Bounding box of a sphere in object space.
sphereObjBoundingBox :: Sphere -> BoundingBox
sphereObjBoundingBox sphere =
  let r = sphereRadius sphere
  in  BoundingBox (-r) (-r) (-r) r r r

-- |Traces a sphere. Ray is in object space.
sphereTrace :: Sphere -> RayParametricRange -> Ray -> Maybe Intersection
sphereTrace sphere rp ray@(Ray p v) =
  let
    radius = sphereRadius sphere
    zMin   = sphereZMin   sphere
    zMax   = sphereZMax   sphere
    phiMax = spherePhiMax sphere
    pvec   = toVector3 p
    qp     = (lengthSquared v, 2.0 * (pvec ⋅ v), lengthSquared pvec - (radius * radius))
    chk t = if isValid then Just isect else Nothing
      where
        isp     = rayAt ray t
        isn     = toNormal3 ((toVector3 isp) .* (-1))  -- temporary testing
        phi'    = atan2 (ycomp isp) (xcomp isp)
        phi     = if (phi' < 0.0) then phi' + 2.0 * pi else phi'
        z       = zcomp isp
        isValid = (rayParamIsValid rp t) && (z >= zMin) && (z <= zMax) && (degrees phi <= phiMax)
        isect   = Intersection isp isn t (quadricRayEpsilonFactor * t)
    f (t1, t2) = asum $ (map chk) [ t1, t2 ]  -- take the first
  in (quadratic qp) >>= f

-- |Determines whether a ray hits a sphere. Ray is in object space.
sphereTraceP :: Sphere -> RayParametricRange -> Ray -> Bool
sphereTraceP sphere rp ray = maybe False (const True) (sphereTrace sphere rp ray)
    
-- |Solves the real roots of the quadratic equation: a * t^2 + b * t + c = 0 for t.
-- The smaller of the two roots is returned first in the tuple.
quadratic :: (Float, Float, Float) -> Maybe (Float, Float)
quadratic (a, b, c) = if d <= 0.0 then Nothing else Just (t1, t2)
  where
    d  = (b * b) - (4.0 * a * c)
    t1 = min ta tb
    t2 = max ta tb
    ta = q / a
    tb = c / q
    q | b < 0     = -0.5 * (b - sqrt d)
      | otherwise = -0.5 * (b + sqrt d)

