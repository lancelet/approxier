{-|
Module      : Shapes.Cylinder
Description : Cylinder
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Geometric descriptions of a cylinder.
-}
module Shapes.Cylinder (
    Cylinder (
        Cylinder
      , cylinderRadius
      , cylinderZMin
      , cylinderZMax
      , cylinderPhiMax
    )
  , traceableCylinder
  , boundedCylinder
  ) where

import Bound              (BoundingBoxed)

import Shapes.Hyperboloid (Hyperboloid (Hyperboloid, hyperboloidPoint1, hyperboloidPoint2,
                                        hyperboloidPhiMax),
                           traceableHyperboloid, boundedHyperboloid)

import Trace              (Traceable)

import VecMath            (p3)

----------------------------------------------------------------------------------------------------
-- CYLINDER

data Cylinder = Cylinder
  { cylinderRadius :: !Float
  , cylinderZMin   :: !Float
  , cylinderZMax   :: !Float
  , cylinderPhiMax :: !Float
  } deriving (Show)

traceableCylinder :: Cylinder -> Traceable
traceableCylinder = traceableHyperboloid . cylinderToHyperboloid

boundedCylinder :: Cylinder -> BoundingBoxed
boundedCylinder = boundedHyperboloid . cylinderToHyperboloid

----------------------------------------------------------------------------------------------------
-- PRIVATE CYLINDER FUNCTIONS

cylinderToHyperboloid :: Cylinder -> Hyperboloid
cylinderToHyperboloid c =
  let r      = cylinderRadius c
      zmin   = cylinderZMin   c
      zmax   = cylinderZMax   c
      p1     = p3 r 0 zmin
      p2     = p3 r 0 zmax
      phimax = cylinderPhiMax c
  in Hyperboloid { hyperboloidPoint1 = p1
                 , hyperboloidPoint2 = p2
                 , hyperboloidPhiMax = phimax }
