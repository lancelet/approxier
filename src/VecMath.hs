{-|
Module      : VecMath
Description : Operations on 3D vectors, normals and points
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
      x
    , y
    , z
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

data Vector3 = Vector3 {-# UNPACK #-} !Float !Float !Float deriving (Show) -- ^ 3D vector.
data Normal3 = Normal3 {-# UNPACK #-} !Float !Float !Float deriving (Show) -- ^ 3D normal.
data Point3  = Point3  {-# UNPACK #-} !Float !Float !Float deriving (Show) -- ^ 3D point.

-- |Provides a uniform way to access elements of a vector, normal or point.
class Cartesian3Tuple a where
  x :: a -> Float
  y :: a -> Float
  z :: a -> Float

  cartesian3Tuple :: a -> (Float, Float, Float)
  cartesian3Tuple q = (x q, y q, z q)

instance Cartesian3Tuple Vector3 where
  x v = let (Vector3 xx _ _) = v in xx
  y v = let (Vector3 _ yy _) = v in yy
  z v = let (Vector3 _ _ zz) = v in zz

instance Cartesian3Tuple Normal3 where
  x n = let (Normal3 xx _ _) = n in xx
  y n = let (Normal3 _ yy _) = n in yy
  z n = let (Normal3 _ _ zz) = n in zz

instance Cartesian3Tuple Point3 where
  x p = let (Point3 xx _ _) = p in xx
  y p = let (Point3 _ yy _) = p in yy
  z p = let (Point3 _ _ zz) = p in zz

-- |Constructs a vector.
v3 :: Float -> Float -> Float -> Vector3
v3 xx yy zz = Vector3 xx yy zz

-- |Constructs a point.
p3 :: Float -> Float -> Float -> Point3
p3 xx yy zz = Point3 xx yy zz

-- |Constructs a normal (guaranteed to be unit length by construction).
n3 :: Float -> Float -> Float -> Normal3
n3 xx yy zz =
    let l = sqrt ((xx * xx) + (yy * yy) + (zz * zz))
    in assert (l >= 0.0) (Normal3 (xx / l) (yy / l) (zz / l))

-- |Explicitly converts a point to a vector.
p3v3 :: Point3 -> Vector3
p3v3 (Point3 xx yy zz) = v3 xx yy zz

-- |Explicitly converts a normal to a vector.
n3v3 :: Normal3 -> Vector3
n3v3 (Normal3 xx yy zz) = v3 xx yy zz

-- |Returns the squared length of a vector.
lengthSquared :: Vector3 -> Float
lengthSquared (Vector3 xx yy zz) = (xx * xx) + (yy * yy) + (zz * zz)

-- |Returns the length of a vector.
vectorLength :: Vector3 -> Float
vectorLength = sqrt . lengthSquared

-- |Multiplies a vector by a scalar.
(.*) :: Vector3 -> Float -> Vector3
(.*) (Vector3 xx yy zz) c = v3 (xx * c) (yy * c) (zz * c)

-- |Divides a vector by a scalar.
(./) :: Vector3 -> Float -> Vector3
(./) (Vector3 xx yy zz) c = v3 (xx / c) (yy / c) (zz / c)

-- |Normalizes a vector, creating a normal.
normalize :: Vector3 -> Normal3
normalize (Vector3 xx yy zz) = n3 xx yy zz

-- |Vector dot product.
dot :: Vector3 -> Vector3 -> Float
dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

-- |Vector dot product (symbolic alias).
(⋅) :: Vector3 -> Vector3 -> Float
(⋅) = dot

-- |Vector cross product.
cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    let xx = (y1 * z2) - (y2 * z1)
        yy = (x2 * z1) - (x1 * z2)
        zz = (x1 * y2) - (x2 * y1)
    in Vector3 xx yy zz

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
