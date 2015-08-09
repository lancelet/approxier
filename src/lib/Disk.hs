{-|

-}
module Disk (
  diskTracePrim
  ) where

import VecMath (Cartesian3Tuple (cartesian3Tuple), XForm, degrees, n3, xform)

import GPrim   (Disk (diskHeight, diskRadiusInner, diskRadiusOuter, diskPhiMax))

import Trace   (BoundingBox, Intersection (Intersection), Ray (Ray),
                TracePrim (TracePrim, tpObj2World, tpObjBound, tpWorldBound, tpTrace, tpTraceP),
                quadricRayEpsilonFactor, rayAt, rayParamIsValid)

diskTracePrim :: XForm -> Disk -> TracePrim
diskTracePrim o2w dsk =
  let objBound = diskbbox dsk
  in TracePrim { tpObj2World  = o2w
               , tpObjBound   = objBound
               , tpWorldBound = \x -> xform x objBound
               , tpTrace      = disktrace  dsk
               , tpTraceP     = disktracep dsk
               }


----------------------------------------------------------------------------------------------------
-- PRIVATE DISK FUNCTIONS

diskbbox :: Disk -> BoundingBox
diskbbox = undefined

disktrace :: Disk -> Ray -> Maybe Intersection
disktrace dsk ray =
  let Ray p v _ tmax            = ray
      (_, _, pz)                = cartesian3Tuple p
      (_, _, vz)                = cartesian3Tuple v
      h                         = diskHeight      dsk
      ri                        = diskRadiusInner dsk
      ro                        = diskRadiusOuter dsk
      phiMax                    = diskPhiMax      dsk
      vzmin                     = abs $ ((h - pz) / (2.0 * tmax))
      t                         = (h - pz) / vz
      iP                        = rayAt ray t
      iN                        = n3 0 0 1
      (x, y, _)                 = cartesian3Tuple iP
      r                         = sqrt (x*x + y*y)
      phi'                      = atan2 y x
      phi                       = if phi' < 0.0 then phi' + 2*pi else phi'
      phid                      = degrees phi
      isValid = (abs vz > vzmin) && (rayParamIsValid ray t) && (r >= ri) && (r <= ro) && (phid <= phiMax)
      is                        = Intersection iP iN t (eps * t)
  in if isValid then Just is else Nothing

eps :: Float
eps = quadricRayEpsilonFactor

disktracep :: Disk -> Ray -> Bool
disktracep dsk ray = maybe False (const True) $ disktrace dsk ray
