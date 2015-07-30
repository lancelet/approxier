{-|
Module      : VecMath
Description : 3D vectors, normals and points
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

3D vector mathematics and transformations.

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
to individual components and explicit transformations between them.

A rudimentary system is defined for tracking which coordinate space some object is defined in. This
is implemented using the 'InSpace csys a' phantom type wrapper, which declares type 'a' to be in
coordinate system 'csys'. Common coordinate systems are defined, along with aliases for vectors,
points and normals in those systems.

Transformations are defined between coordinate systems, and the transformations also track 'from'
and 'to' coordinate systems. This is a rough system intended to prevent very broad kinds of error.
It has obvious limitations; for example: it's easy to transform a point from world space into the
space of the wrong object.
-}
module VecMath (
  -- * Classes
  Cartesian3Tuple(
    xcomp
  , ycomp
  , zcomp
  , cartesian3Tuple
  , toVector3
  , toNormal3
  , toPoint3
  )
  -- * Basic vector math types
  , Vector3
  , Normal3
  , Point3
  -- * Constructor functions
  , v3
  , p3
  , n3
  -- * Vector3 arithmetic
  , (.*)
  , (./)
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
  , offsetPointAlongVector
  -- * Transformations between spaces
  , XForm
  , xformInv
  , xformCompose
  , xformId
  , translate
  , scale
  , rotate
  , Transformable(xform)
  -- * Rays
  , Ray(Ray)
  ) where

import Control.Exception (assert)

----------------------------------------------------------------------------------------------------
-- BASIC VECTOR OPERATIONS

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

  toVector3 :: a -> Vector3
  toVector3 q = v3 (xcomp q) (ycomp q) (zcomp q)

  toNormal3 :: a -> Normal3
  toNormal3 q = n3 (xcomp q) (ycomp q) (zcomp q)

  toPoint3 :: a -> Point3
  toPoint3 q = p3 (xcomp q) (ycomp q) (zcomp q)

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

-- |Vector3 pretends to have a Num instance.
instance Num Vector3 where
  (+) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
  (-) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)
  negate (Vector3 x y z) = Vector3 (-x) (-y) (-z)
  (*)         = error "Multiplication is NOT defined for Vector3"
  abs         = error "abs is NOT defined for Vector3"
  signum      = error "signum is NOT defined for Vector3"
  fromInteger = error "fromInteger is NOT defined for Vector3"

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

-- |Offsets a point by a given vector.
offsetPoint :: Vector3 -> Point3 -> Point3
offsetPoint (Vector3 vx vy vz) (Point3 px py pz) = Point3 (px + vx) (py + vy) (pz + vz)

-- |Offsets a point by a given factor along a direction vector.
offsetPointAlongVector :: Vector3 -> Float -> Point3 -> Point3
offsetPointAlongVector v d p = toPoint3 ((toVector3 p) + (v .* d))

----------------------------------------------------------------------------------------------------
-- TRANSFORMATIONS BETWEEN COORDINATE SPACES

-- |Transformation between coordinate spaces.
data XForm = XForm
             AMatrix  -- ^ transformation from -> to
             AMatrix  -- ^ inverse transformation to -> from

-- |Invert a transformation.
xformInv :: XForm -> XForm
xformInv (XForm x x') = XForm x' x

-- |Compose transformations.
xformCompose :: XForm  -- ^ transformation A
             -> XForm  -- ^ transformation B
             -> XForm  -- ^ transformation that is equal to applying A and then B
xformCompose (XForm x1 x1') (XForm x2 x2') = XForm (x2 * x1) (x1' * x2')

-- |Identity affine transformation.
xformId :: XForm
xformId =
  let m = AMatrix  1 0 0 0  0 1 0 0  0 0 1 0
  in XForm m m

-- |Translation.
translate :: Float -> Float -> Float -> XForm
translate tx ty tz =
  let
    m  = AMatrix  1 0 0   tx   0 1 0   ty   0 0 1   tz
    m' = AMatrix  1 0 0 (-tx)  0 1 0 (-ty)  0 0 1 (-tz)
  in XForm m m'

-- |Scale.
scale :: Float -> Float -> Float -> XForm
scale sx sy sz =
  let
    m  = AMatrix sx 0 0 0  0 sy 0 0  0 0 sz 0
    m' = AMatrix (1.0 / sx) 0 0 0  0 (1.0 / sy) 0 0  0 0 (1.0/sz) 0
  in XForm m m'

-- |Axis-angle rotation.
-- The angle is expressed in degrees.
rotate :: Float -> Vector3 -> XForm
rotate degAngle v =
  let
    angle = degAngle * pi / 180.0
    Normal3 ax ay az = toNormal3 v
    s = sin angle
    c = cos angle
    m11 = ax * ax + (1.0 - ax * ax) * c
    m12 = ax * ay * (1.0 - c) - az * s
    m13 = ax * az * (1.0 - c) + ay * s
    m21 = ax * ay * (1.0 - c) + az * s
    m22 = ay * ay + (1.0 - ay * ay) * c
    m23 = ay * az * (1.0 - c) - ax * s
    m31 = ax * az * (1.0 - c) - ay * s
    m32 = ay * az * (1.0 - c) + ax * s
    m33 = az * az + (1.0 - az * az) * c
    m  = AMatrix  m11 m12 m13 0  m21 m22 m23 0  m31 m32 m33 0
    m' = AMatrix  m11 m21 m31 0  m12 m22 m32 0  m13 m23 m33 0
  in XForm m m'

-- |Transform a vector.
xformVector3 :: XForm -> Vector3 -> Vector3
xformVector3 (XForm m _) (Vector3 x y z) =
  let HVector x' y' z' _ = affineMul m (HVector x y z 0)
  in Vector3 x' y' z'

-- |Transform a point.
xformPoint3 :: XForm -> Point3 -> Point3
xformPoint3 (XForm m _) (Point3 x y z) =
  let HVector x' y' z' _ = affineMul m (HVector x y z 1)
  in Point3 x' y' z'

-- |Transform a normal.
xformNormal3 :: XForm -> Normal3 -> Normal3
xformNormal3 (XForm _ m') (Normal3 x y z) =
  let HVector x' y' z' _ = affineTransposeMul m' (HVector x y z 0)
  in n3 x' y' z'

-- |Transforms an object of type 'a' between coordinate spaces.
class Transformable a where
  xform :: XForm -> a -> a

instance Transformable Vector3 where xform = xformVector3
instance Transformable Point3  where xform = xformPoint3
instance Transformable Normal3 where xform = xformNormal3

----------------------------------------------------------------------------------------------------
-- HOMOGENEOUS COORDINATE VECTORS AND AFFINE MATRICES

-- |Vector or point in homogeneous coordinates.
data HVector = HVector {-# UNPACK #-} !Float !Float !Float !Float

-- |Affine transformation matrix.
-- This is a 4x4 homogeneous transformation matrix, but we explicitly assume that the bottom row
-- just always contains the values [0,0,0,1]. (Saves writing them out.)
-- This can represent all possible affine transformations, but NOT projective transformations.
data AMatrix = AMatrix {-# UNPACK #-} !Float !Float !Float !Float
                                      !Float !Float !Float !Float
                                      !Float !Float !Float !Float

instance Num AMatrix where
  (*) a b =
    let
      AMatrix a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 = a
      AMatrix b11 b12 b13 b14 b21 b22 b23 b24 b31 b32 b33 b34 = b
      x11 = (a11 * b11) + (a12 * b21) + (a13 * b31)
      x12 = (a11 * b12) + (a12 * b22) + (a13 * b32)
      x13 = (a11 * b13) + (a12 * b23) + (a13 * b33)
      x14 = (a11 * b14) + (a12 * b24) + (a13 * b34) + a14
      x21 = (a21 * b11) + (a22 * b21) + (a23 * b31)
      x22 = (a21 * b12) + (a22 * b22) + (a23 * b32)
      x23 = (a21 * b13) + (a22 * b23) + (a23 * b33)
      x24 = (a21 * b14) + (a22 * b24) + (a23 * b34) + a24
      x31 = (a31 * b11) + (a32 * b21) + (a33 * b31)
      x32 = (a31 * b12) + (a32 * b22) + (a33 * b32)
      x33 = (a31 * b13) + (a32 * b23) + (a33 * b33)
      x34 = (a31 * b14) + (a32 * b24) + (a33 * b34) + a34
    in
      AMatrix x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34
  (+)         = error "(+) is not defined for AMatrix"
  (-)         = error "(-) is not defined for AMatrix"
  abs         = error "abs is not defined for AMatrix"
  signum      = error "signum is not defined for AMatrix"
  fromInteger = error "fromInteger is not defined for AMatrix"
  negate      = error "negate is not defined for AMatrix"

-- |Multiplies an affine matrix by a homogeneous vector.
affineMul :: AMatrix -> HVector -> HVector
affineMul m v =
  let
    AMatrix m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 = m
    HVector x y z w = v
    x' = (m11 * x) + (m12 * y) + (m13 * z) + (m14 * w)
    y' = (m21 * x) + (m22 * y) + (m23 * z) + (m24 * w)
    z' = (m31 * x) + (m32 * y) + (m33 * z) + (m34 * w)
  in HVector x' y' z' w

-- |Multiplies the transpose of an affine matrix by a homogeneous vector.
-- This operation ONLY uses the rotational component of the matrix, and assumes the
-- translation component is zero. (It is used in transforming normals, so we can
-- safely disregard the translations.)
affineTransposeMul :: AMatrix -> HVector -> HVector
affineTransposeMul m v =
  let
    AMatrix m11 m21 m31 _ m12 m22 m32 _ m13 m23 m33 _ = m
    HVector x y z w = v
    x' = (m11 * x) + (m12 * y) + (m13 * z)
    y' = (m21 * x) + (m22 * y) + (m23 * z)
    z' = (m31 * x) + (m32 * y) + (m33 * z)
  in HVector x' y' z' w

----------------------------------------------------------------------------------------------------
-- RAYS

-- |Ray.
data Ray = Ray Point3 Vector3 deriving (Show)

-- |Transform a ray.
xformRay :: XForm -> Ray -> Ray
xformRay x (Ray p v) = Ray (xformPoint3 x p) (xformVector3 x v)

instance Transformable Ray where xform = xformRay
