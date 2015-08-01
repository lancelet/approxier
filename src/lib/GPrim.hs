{-|
Module      : GPrim
Description : Geometric primitives
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Geometric primitive definitions.
-}
module GPrim (
  -- * Quadric surface primitives.
    Sphere (
      Sphere
    , sphereRadius
    , sphereZMin
    , sphereZMax
    , spherePhiMax
    )
  , Cone (
      Cone
    , coneHeight
    , coneRadius
    , conePhiMax
    )
  , Cylinder(
      Cylinder
    , cylinderRadius
    , cylinderZMin
    , cylinderZMax
    , cylinderPhiMax
    )
  , Hyperboloid(
      Hyperboloid
    , hyperboloidPoint1
    , hyperboloidPoint2
    , hyperboloidPhiMax
    )
  , Paraboloid(
      Paraboloid
    , paraboloidRMax
    , paraboloidZMin
    , paraboloidZMax
    , paraboloidPhiMax
    )
  , Disk (
      Disk
    , diskHeight
    , diskRadius
    , diskPhiMax
    )
  , Torus (
      Torus
    , torusMajorRadius
    , torusMinorRadius
    , torusThetaMin
    , torusThetaMax
    , torusPhiMax
    )
  ) where

import VecMath (Point3)

----------------------------------------------------------------------------------------------------
-- QUADRICS

-- |Sphere.
data Sphere = Sphere
  { sphereRadius :: !Float
  , sphereZMin   :: !Float
  , sphereZMax   :: !Float
  , spherePhiMax :: !Float
  } deriving (Show)

-- |Cone.
data Cone = Cone
  { coneHeight :: !Float
  , coneRadius :: !Float
  , conePhiMax :: !Float
  } deriving (Show)

-- |Cylinder.
data Cylinder = Cylinder
  { cylinderRadius :: !Float
  , cylinderZMin   :: !Float
  , cylinderZMax   :: !Float
  , cylinderPhiMax :: !Float
  } deriving (Show)

-- |Hyperboloid.
data Hyperboloid = Hyperboloid
  { hyperboloidPoint1 :: !Point3
  , hyperboloidPoint2 :: !Point3
  , hyperboloidPhiMax :: !Float
  } deriving (Show)

-- |Paraboloid.
data Paraboloid = Paraboloid
  { paraboloidRMax   :: !Float
  , paraboloidZMin   :: !Float
  , paraboloidZMax   :: !Float
  , paraboloidPhiMax :: !Float
  } deriving (Show)

-- |Disk.
data Disk = Disk
  { diskHeight :: !Float
  , diskRadius :: !Float
  , diskPhiMax :: !Float
  } deriving (Show)

-- |Torus.
data Torus = Torus
  { torusMajorRadius :: !Float
  , torusMinorRadius :: !Float
  , torusThetaMin    :: !Float
  , torusThetaMax    :: !Float
  , torusPhiMax      :: !Float
  } deriving (Show)
