{-|
Module      : Shapes.Disk
Description : Disk
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Geometric descriptions of a disk.
-}

module Shapes.Disk (
    Disk (
        Disk
      , diskHeight
      , diskRadiusInner
      , diskRadiusOuter
      , diskPhiMax
    )
  , traceableDisk
  , boundedDisk
  ) where

import Bound   (BoundingBox(BoundingBox), BoundingBoxed, mkBoundingBoxed)

import Trace   (Intersection (Intersection), Ray, Traceable, mkTraceable, quadricRayEpsilonFactor,
                rayAt, rayMaxParam, rayParamIsValid, rayPoint, rayVector)

import VecMath (UVCoord (UVCoord), cartesian3Tuple, n3, normAngle1, radians, zcomp3)

----------------------------------------------------------------------------------------------------
-- DISK

data Disk = Disk
  { diskHeight      :: !Float
  , diskRadiusInner :: !Float
  , diskRadiusOuter :: !Float
  , diskPhiMax      :: !Float
  } deriving (Show)

traceableDisk :: Disk -> Traceable
traceableDisk = mkTraceable . diskTrace

boundedDisk :: Disk -> BoundingBoxed
boundedDisk = mkBoundingBoxed . diskBound

----------------------------------------------------------------------------------------------------
-- PRIVATE DISK FUNCTIONS

diskBound :: Disk -> BoundingBox
diskBound (Disk h _ r _) =
  let e = 1e-3 * r  -- fudge factor to give the otherwise flat disk bounding box a non-zero height
  in BoundingBox (-r) (-r) (h-e) r r (h+e)

diskTrace :: Disk -> Ray -> Maybe Intersection
diskTrace disk ray =
  let h     = diskHeight disk
      pz    = (zcomp3 . rayPoint) ray
      vz    = (zcomp3 . rayVector) ray
      t     = (h - pz) / vz
  in intersection disk ray t

intersection :: Disk -> Ray -> Float -> Maybe Intersection
intersection disk ray t =
  let p         = rayAt ray t
      n         = n3 0 0 1
      (x, y, _) = cartesian3Tuple p
      r         = sqrt(x*x + y*y)
      rmin      = diskRadiusInner disk
      rmax      = diskRadiusOuter disk
      phi       = normAngle1 $ atan2 y x
      phimax    = (radians . diskPhiMax) disk
      u         = phi / phimax
      v         = (r - rmin) / (rmax - rmin)
      eps       = t * quadricRayEpsilonFactor
  in
    if paramsValid disk ray r phi t
    then Just $ Intersection p n (UVCoord u v) ray t eps
    else Nothing

paramsValid :: Disk
            -> Ray
            -> Float  -- ^ radius
            -> Float  -- ^ phi radians
            -> Float  -- ^ t
            -> Bool
paramsValid disk ray r phi t =
  let rmin   = diskRadiusInner disk
      rmax   = diskRadiusOuter disk
      phimax = (radians . diskPhiMax) disk
      vzmag  = (abs . zcomp3 . rayVector) ray
      pz     = (zcomp3 . rayPoint)  ray
      h      = diskHeight disk
      tmax   = rayMaxParam ray
      vzmin  = abs $ (h - pz) / (2*tmax)
  in ( vzmag > vzmin
    && rayParamIsValid ray t
    && r   >= rmin
    && r   <= rmax
    && phi <= phimax
     )
