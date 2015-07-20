{-|
Module      : VecMath
Description : 3D vectors, normals and points
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

This module contains basic types for 3D vectors, normals and points, as well as the operations which
are commonly used on them.

3D vectors, normals and points are unpacked, strict structures. Each contains 3 'Float' values,
referring to Cartesian x, y, z coordinates. Vectors, normals and points all have the same structure,
but are distinguished from each other because they behave differently under common types of
transformation and have slightly different semantic meanings. It is thus convenient to separate them
at the type level to avoid mistakenly attempting operations on the wrong type.  However, it is
sometimes appropriate to force a conversion to a different type using one of the supplied conversion
functions.

Smart constructors are used for the vector, normal and point because the normal vector should always
have unit length. Employing smart constructors for all of them ensures a more uniform interface. To
access x, y and z components, a typeclass called 'cartesian3Tuple' is provided, which gives access
to individual components as well as a way to retrieve them as a '(Float, Float, Float)' tuple.
-}
module VecMath (
  -- * Classes
    Cartesian3Tuple(
      xcomp
    , ycomp
    , zcomp
    , cartesian3Tuple
    )
  -- * Types
  , Vector3
  , Normal3
  , Point3
  -- * Constructor functions
  , v3
  , p3
  , n3
  -- * Conversions
  , p3v3
  , n3v3
  -- * Vector3 arithmetic
  , (.*)
  , (./)
  , (.+)
  , (.-)
  -- * Vector3 operations
  , lengthSquared
  , vectorLength
  , normalize
  , dot
  , cross
  , (⋅)
  , (⨯)
  -- * Point3 operations
  , offsetPoint
  ) where

import Control.Exception (assert)

-- |3D vector.
data Vector3 = Vector3 {-# UNPACK #-} !Float !Float !Float deriving (Show)
-- |3D normal.
data Normal3 = Normal3 {-# UNPACK #-} !Float !Float !Float deriving (Show)
-- |3D point.
data Point3  = Point3  {-# UNPACK #-} !Float !Float !Float deriving (Show)

-- |Provides a uniform way to access elements of a vector, normal or point.
class Cartesian3Tuple a where
  xcomp :: a -> Float
  ycomp :: a -> Float
  zcomp :: a -> Float

  cartesian3Tuple :: a -> (Float, Float, Float)
  cartesian3Tuple q = (xcomp q, ycomp q, zcomp q)

instance Cartesian3Tuple Vector3 where
  xcomp v = let (Vector3 x _ _) = v in x
  ycomp v = let (Vector3 _ y _) = v in y
  zcomp v = let (Vector3 _ _ z) = v in z

instance Cartesian3Tuple Normal3 where
  xcomp n = let (Normal3 x _ _) = n in x
  ycomp n = let (Normal3 _ y _) = n in y
  zcomp n = let (Normal3 _ _ z) = n in z

instance Cartesian3Tuple Point3 where
  xcomp p = let (Point3 x _ _) = p in x
  ycomp p = let (Point3 _ y _) = p in y
  zcomp p = let (Point3 _ _ z) = p in z

-- |Constructs a vector.
v3 :: Float -> Float -> Float -> Vector3
v3 x y z = Vector3 x y z

-- |Constructs a point.
p3 :: Float -> Float -> Float -> Point3
p3 x y z = Point3 x y z

-- |Constructs a normal (guaranteed to be unit length by construction).
n3 :: Float -> Float -> Float -> Normal3
n3 x y z =
    let l = sqrt ((x * x) + (y * y) + (z * z))
    in assert (l >= 0.0) (Normal3 (x / l) (y / l) (z / l))

-- |Explicitly converts a point to a vector.
p3v3 :: Point3 -> Vector3
p3v3 (Point3 x y z) = v3 x y z

-- |Explicitly converts a normal to a vector.
n3v3 :: Normal3 -> Vector3
n3v3 (Normal3 x y z) = v3 x y z

-- |Returns the squared length of a vector.
lengthSquared :: Vector3 -> Float
lengthSquared (Vector3 x y z) = (x * x) + (y * y) + (z * z)

-- |Returns the length of a vector.
vectorLength :: Vector3 -> Float
vectorLength = sqrt . lengthSquared

-- |Multiplies a vector by a scalar.
(.*) :: Vector3 -> Float -> Vector3
(.*) (Vector3 x y z) c = v3 (x * c) (y * c) (z * c)

-- |Divides a vector by a scalar.
(./) :: Vector3 -> Float -> Vector3
(./) (Vector3 x y z) c = v3 (x / c) (y / c) (z / c)

-- |Normalizes a vector, creating a normal.
normalize :: Vector3 -> Normal3
normalize (Vector3 x y z) = n3 x y z

-- |Vector dot product.
dot :: Vector3 -> Vector3 -> Float
dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

-- |Vector dot product (symbolic alias).
(⋅) :: Vector3 -> Vector3 -> Float
(⋅) = dot

-- |Vector cross product.
cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    let x = (y1 * z2) - (y2 * z1)
        y = (x2 * z1) - (x1 * z2)
        z = (x1 * y2) - (x2 * y1)
    in Vector3 x y z

-- |Vector cross product (symbolic alias).
(⨯) :: Vector3 -> Vector3 -> Vector3
(⨯) = cross

-- |Adds two vectors.
(.+) :: Vector3 -> Vector3 -> Vector3
(.+) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)

-- |Subtracts a vector from another.
(.-) :: Vector3 -> Vector3 -> Vector3
(.-) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

-- |Offsets a point by a given vector.
offsetPoint :: Vector3 -> Point3 -> Point3
offsetPoint (Vector3 vx vy vz) (Point3 px py pz) = Point3 (px + vx) (py + vy) (pz + vz)
