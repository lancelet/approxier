{-|

-}
module Chess (
    pawn
  ) where

import GPrim (Sphere(Sphere), Hyperboloid(Hyperboloid), Cylinder(Cylinder), Disk(Disk),
              Torus(Torus))
import VecMath (Point3, p3, scale, translate, Transformable(xform), xformId)
import Trace (TracePrim, brainDeadTraceGroup, sphereTracePrim, nullTracePrim)
import Hyperboloid (hyperboloidTracePrim)
import Cylinder (cylinderTracePrim)
import Disk (diskTracePrim)
import Torus (torusTracePrim)

-- |A pawn, 50 units high, oriented along the z-axis.
pawn :: TracePrim
pawn =
  let s = (50.0 / 51.0)
  in xform (scale s s s) $ brainDeadTraceGroup
       [ -- felt base
         dsk 0 0 13.8
       , dz 0.4 $ tor 13.8 0.4 (-90) 0
       , cyl 14.2 0.4 1.4
       , dz 1.4 $ tor 13.8 0.4 0 90
       , dsk 1.8 0 13.8
         -- pawn
       , dsk 1.8 0 13.14
       , dz 2.2 $ tor 13.14 0.4 (-90) (-72.9)
       , hyp (p3 13.2575 0 1.8177) (p3 14.3588 0 2.1566)
       , dz 2.3477 $ tor 14.3 0.2 (-72.9) 0
       , cyl 14.5 2.3477 2.6122
       , dz 2.6122 $ tor 14.3 0.2 0 47.39
       , hyp (p3 14.4354 0 2.7594) (p3 12.264 0 4.758)
       , dz 4.905 $ tor 12.4 0.2 (-245.82) (-132.77)
       , dz 7.237 $ tor 11.551 2.282 (-70.36) 50.58
       , hyp (p3 13 0 9) (p3 10.343 0 11.184)
       , dz 14.661 $ tor 13.2 4.5 (-180) (-128.42)
       , cyl 8.7 14.661 14.7
       , dz 14.7 $ tor 8.4 0.3 0 90
       , dsk 15 8.269 8.4 -- , hyp (p3 8.4 0 15) (p3 8.269 0 15)  -- TODO: Degenerate
       , dz 16.4 $ tor 8.269 1.4 (-148.8) (-90)
       , dz 25.471 $ tor 23.257 18.919 (-199.76) (-148.82)  -- main body torus
       , dz 31.8 $ tor 5.64 0.2 160.11 90
       , dsk 32 5.64 6.9 -- , hyp (p3 5.64 0 32) (p3 6.9 0 32)   -- TODO: Degenerate
       , dz 32.6 $ tor 6.9 0.6 (-90) 0
       , cyl 7.5 32.6 32.86
       , dz 32.86 $ tor 6.9 0.6 0 59.12
       , hyp (p3 7.208 0 33.3749) (p3 3.617 0 35.52)
       , dz 35.692 $ tor 3.719 0.2 (-243.18) (-120.85)
       , dz 43 $ sph 8 (-7.13) 8
       ]

dsk :: Float -> Float -> Float -> TracePrim
dsk h ri ro = diskTracePrim xformId $ Disk h ri ro 360

dz :: Float -> TracePrim -> TracePrim
dz z = xform (translate 0 0 z)

cyl :: Float -> Float -> Float -> TracePrim
cyl r zMin zMax = cylinderTracePrim xformId $ Cylinder r zMin zMax 360

tor :: Float -> Float -> Float -> Float -> TracePrim
tor minor major thetamin thetamax = torusTracePrim xformId $ Torus minor major thetamin thetamax 360

hyp :: Point3 -> Point3 -> TracePrim
hyp p1 p2 = hyperboloidTracePrim xformId $ Hyperboloid p1 p2 360

sph :: Float -> Float -> Float -> TracePrim
sph r zMin zMax = sphereTracePrim xformId $ Sphere r zMin zMax 360
