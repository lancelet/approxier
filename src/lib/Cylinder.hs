{-|

-}

module Cylinder (
  cylinderTracePrim
  ) where

import Hyperboloid (hyperboloidTracePrim)

import GPrim (Cylinder(Cylinder), Hyperboloid(Hyperboloid))

import VecMath (XForm, p3)

import Trace (TracePrim)

cylinderTracePrim :: XForm -> Cylinder -> TracePrim
cylinderTracePrim obj2World (Cylinder radius zMin zMax phiMax) =
  let
    pt1 = p3 radius 0 zMin
    pt2 = p3 radius 0 zMax
  in hyperboloidTracePrim obj2World $ Hyperboloid pt1 pt2 phiMax

