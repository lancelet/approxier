{-|
Module      : VecMath
Description : 3D vectors, normals and points
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Vector mathematics and transformations.
-}
module VecMath (
  -- * Classes
    Cartesian3Tuple(
      xcomp3
    , ycomp3
    , zcomp3
    , cartesian3Tuple
    , toVector3
    , toNormal3
    , toPoint3
    )
  -- * Basic 3D vector math types
  , Vector3
  , Normal3
  , Point3
  -- * Point / Vector constructor functions
  , v3
  , p3
  , n3
  -- * Rectangle (2D)
  , Rectangle
  , rectangle
  , rectangleContains
  , rectangleUnion
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
  -- * UV coordinates
  , UVCoord(UVCoord)
  -- * Transformations between spaces
  , XForm
  , xformInv
  , xformCompose
  , xformId
  , translate
  , scale
  , rotate
  , Transform(xform)
  , inSpace
  , inSpace'
  -- * Utilities
  , degrees
  , radians
  , clamp
  , normAngle1
  ) where

import Control.Exception (assert)

----------------------------------------------------------------------------------------------------
-- BASIC 3D VECTOR OPERATIONS

-- | 3D vector.
data Vector3 = Vector3 {-# UNPACK #-} !Float !Float !Float deriving (Show)
-- | 3D normal.
data Normal3 = Normal3 {-# UNPACK #-} !Float !Float !Float deriving (Show)
-- | 3D point.
data Point3  = Point3  {-# UNPACK #-} !Float !Float !Float deriving (Show)

-- | Provides a uniform way to access elements of a 3D vector, normal or point.
class Cartesian3Tuple a where
  xcomp3 :: a -> Float
  ycomp3 :: a -> Float
  zcomp3 :: a -> Float

  cartesian3Tuple :: a -> (Float, Float, Float)
  cartesian3Tuple q = (xcomp3 q, ycomp3 q, zcomp3 q)

  toVector3 :: a -> Vector3
  toVector3 q = v3 (xcomp3 q) (ycomp3 q) (zcomp3 q)

  toNormal3 :: a -> Normal3
  toNormal3 q = n3 (xcomp3 q) (ycomp3 q) (zcomp3 q)

  toPoint3 :: a -> Point3
  toPoint3 q = p3 (xcomp3 q) (ycomp3 q) (zcomp3 q)

instance Cartesian3Tuple Vector3 where
  xcomp3 v = let (Vector3 x _ _) = v in x
  ycomp3 v = let (Vector3 _ y _) = v in y
  zcomp3 v = let (Vector3 _ _ z) = v in z

instance Cartesian3Tuple Normal3 where
  xcomp3 n = let (Normal3 x _ _) = n in x
  ycomp3 n = let (Normal3 _ y _) = n in y
  zcomp3 n = let (Normal3 _ _ z) = n in z

instance Cartesian3Tuple Point3 where
  xcomp3 p = let (Point3 x _ _) = p in x
  ycomp3 p = let (Point3 _ y _) = p in y
  zcomp3 p = let (Point3 _ _ z) = p in z

-- | Vector3 pretends to have a Num instance.
--
--   Insert grumbles about Haskell numeric typeclasses.
instance Num Vector3 where
  (+) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
  (-) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)
  negate (Vector3 x y z) = Vector3 (-x) (-y) (-z)
  (*)         = error "Multiplication is NOT defined for Vector3"
  abs         = error "abs is NOT defined for Vector3"
  signum      = error "signum is NOT defined for Vector3"
  fromInteger = error "fromInteger is NOT defined for Vector3"

-- | Constructs a vector.
v3 :: Float -> Float -> Float -> Vector3
v3 x y z = Vector3 x y z

-- | Constructs a point.
p3 :: Float -> Float -> Float -> Point3
p3 x y z = Point3 x y z

-- | Constructs a normal (guaranteed to be unit length by construction).
n3 :: Float -> Float -> Float -> Normal3
n3 x y z =
    let l = sqrt ((x * x) + (y * y) + (z * z))
    in assert (l >= 0.0) (Normal3 (x / l) (y / l) (z / l))

-- | Returns the squared length of a vector.
lengthSquared :: Vector3 -> Float
lengthSquared (Vector3 x y z) = (x * x) + (y * y) + (z * z)

-- | Returns the length of a vector.
vectorLength :: Vector3 -> Float
vectorLength = sqrt . lengthSquared

-- | Multiplies a vector by a scalar.
(.*) :: Vector3 -> Float -> Vector3
(.*) (Vector3 x y z) c = v3 (x * c) (y * c) (z * c)

-- | Divides a vector by a scalar.
(./) :: Vector3 -> Float -> Vector3
(./) (Vector3 x y z) c = v3 (x / c) (y / c) (z / c)

-- | Normalizes a vector, creating a normal.
normalize :: Vector3 -> Vector3
normalize (Vector3 x y z) = toVector3 $ n3 x y z

-- | Vector dot product.
dot :: Vector3 -> Vector3 -> Float
dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

-- | Vector dot product (symbolic alias).
(⋅) :: Vector3 -> Vector3 -> Float
(⋅) = dot

-- | Vector cross product.
cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    let x = (y1 * z2) - (y2 * z1)
        y = (x2 * z1) - (x1 * z2)
        z = (x1 * y2) - (x2 * y1)
    in Vector3 x y z

-- | Vector cross product (symbolic alias).
(⨯) :: Vector3 -> Vector3 -> Vector3
(⨯) = cross

-- | Offsets a point by a given vector.
offsetPoint :: Vector3 -> Point3 -> Point3
offsetPoint (Vector3 vx vy vz) (Point3 px py pz) = Point3 (px + vx) (py + vy) (pz + vz)

-- | Offsets a point by a given factor along a direction vector.
offsetPointAlongVector :: Vector3 -> Float -> Point3 -> Point3
offsetPointAlongVector v d p = toPoint3 ((toVector3 p) + (v .* d))

----------------------------------------------------------------------------------------------------
-- 2D RECTANGLE

data Rectangle a = Rectangle
                   !a  -- ^ x 
                   !a  -- ^ y
                   !a  -- ^ width
                   !a  -- ^ height

-- | Creates a rectangle.
--
--   The width and height of the rectangle are normalized so that they are always positive.
rectangle :: (Num a, Ord a)
          => a -- ^ x coordinate
          -> a -- ^ y coordinate
          -> a -- ^ width
          -> a -- ^ height
          -> Rectangle a
rectangle x y w h
  | w < 0     = rectangle (x-w) y (-w) h
  | h < 0     = rectangle x (y-h) w (-h)
  | otherwise = Rectangle x y w h

-- | Checks if a rectangle contains a point.
rectangleContains :: (Ord a)
                  => Rectangle a
                  -> a -- ^ x coordinate
                  -> a -- ^ y coordinate
                  -> Bool
rectangleContains (Rectangle x y w h) px py =
  (px >= x) && (px < w) && (py >= y) && (py < h)

-- | Finds a rectangle which completely bounds two existing rectangles.
rectangleUnion :: (Num a, Ord a)
               => Rectangle a
               -> Rectangle a
               -> Rectangle a
rectangleUnion (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
  let x = min x1 x2
      y = max y1 y2
      xmax = max (x1+w1) (x2+w2)
      ymax = max (y1+h1) (y2+h2)
      w = xmax - x
      h = ymax - y
  in Rectangle x y w h

----------------------------------------------------------------------------------------------------
-- UV COORDINATES

-- | UV parametric coordinates.
data UVCoord = UVCoord {-# UNPACK #-} !Float !Float deriving (Show)

----------------------------------------------------------------------------------------------------
-- TRANSFORMATIONS BETWEEN COORDINATE SPACES

-- | Transformation between coordinate spaces.
data XForm = XForm
             AMatrix  -- ^ transformation from -> to
             AMatrix  -- ^ inverse transformation to -> from
             deriving (Show)

-- | Invert a transformation.
xformInv :: XForm -> XForm
xformInv (XForm x x') = XForm x' x

-- | Compose transformations.
xformCompose :: XForm  -- ^ transformation A
             -> XForm  -- ^ transformation B
             -> XForm  -- ^ transformation that is equal to applying A and then B
xformCompose (XForm x1 x1') (XForm x2 x2') = XForm (x2 * x1) (x1' * x2')

-- | Monoid instance for XForm.
instance Monoid XForm where
  mempty  = xformId
  mappend = xformCompose

-- | Identity affine transformation.
xformId :: XForm
xformId =
  let m = AMatrix  1 0 0 0  0 1 0 0  0 0 1 0
  in XForm m m

-- | Translation.
translate :: Float -> Float -> Float -> XForm
translate tx ty tz =
  let
    m  = AMatrix  1 0 0   tx   0 1 0   ty   0 0 1   tz
    m' = AMatrix  1 0 0 (-tx)  0 1 0 (-ty)  0 0 1 (-tz)
  in XForm m m'

-- | Scale.
scale :: Float -> Float -> Float -> XForm
scale sx sy sz =
  let
    m  = AMatrix sx 0 0 0  0 sy 0 0  0 0 sz 0
    m' = AMatrix (1.0 / sx) 0 0 0  0 (1.0 / sy) 0 0  0 0 (1.0/sz) 0
  in XForm m m'

-- | Axis-angle rotation.
-- 
--   The angle is expressed in degrees.
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

-- | Transform a vector.
xformVector3 :: XForm -> Vector3 -> Vector3
xformVector3 (XForm m _) (Vector3 x y z) =
  let HVector x' y' z' _ = affineMul m (HVector x y z 0)
  in Vector3 x' y' z'

-- | Transform a point.
xformPoint3 :: XForm -> Point3 -> Point3
xformPoint3 (XForm m _) (Point3 x y z) =
  let HVector x' y' z' _ = affineMul m (HVector x y z 1)
  in Point3 x' y' z'

-- | Transform a normal.
xformNormal3 :: XForm -> Normal3 -> Normal3
xformNormal3 (XForm _ m') (Normal3 x y z) =
  let HVector x' y' z' _ = affineTransposeMul m' (HVector x y z 0)
  in n3 x' y' z'

-- | Transforms an object of type 'a' between coordinate spaces.
class Transform a where
  xform :: XForm -> a -> a

instance Transform Vector3 where xform = xformVector3
instance Transform Point3  where xform = xformPoint3
instance Transform Normal3 where xform = xformNormal3

instance (Transform a) => Transform (Maybe a) where
  xform x = maybe Nothing (Just . xform x)

-- | Performs a function in a given space and then transforms back.
--
--   Transforms a value using the given transformation, then computes a function, and finally
--   transforms the result back using the inverse of the transformation.
inSpace :: (Transform a, Transform b)
        => XForm
        -> (a -> b)
        -> a
        -> b
inSpace x f =
  let fwd = xform x
      bwd = (xform . xformInv) x
  in bwd . f . fwd

-- | Performs a function in a given space, but does NOT transform the result back.
inSpace' :: (Transform a)
         => XForm
         -> (a -> b)
         -> a
         -> b
inSpace' x f =
  let fwd = xform x
  in f . fwd

----------------------------------------------------------------------------------------------------
-- HOMOGENEOUS COORDINATE VECTORS AND AFFINE MATRICES

-- | Vector or point in homogeneous coordinates.
data HVector = HVector {-# UNPACK #-} !Float !Float !Float !Float

-- | Affine transformation matrix.
--
--   This is a 4x4 homogeneous transformation matrix, but we explicitly assume that the bottom row
--   just always contains the values [0,0,0,1]. (Saves writing them out.)
--   This can represent all possible affine transformations, but NOT projective transformations.
data AMatrix = AMatrix {-# UNPACK #-} !Float !Float !Float !Float
                                      !Float !Float !Float !Float
                                      !Float !Float !Float !Float
                                      deriving (Show)

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

-- | Multiplies an affine matrix by a homogeneous vector.
affineMul :: AMatrix -> HVector -> HVector
affineMul m v =
  let
    AMatrix m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 = m
    HVector x y z w = v
    x' = (m11 * x) + (m12 * y) + (m13 * z) + (m14 * w)
    y' = (m21 * x) + (m22 * y) + (m23 * z) + (m24 * w)
    z' = (m31 * x) + (m32 * y) + (m33 * z) + (m34 * w)
  in HVector x' y' z' w

-- | Multiplies the transpose of an affine matrix by a homogeneous vector.
--
--   This operation ONLY uses the rotational component of the matrix, and assumes the
--   translation component is zero. (It is used in transforming normals, so we can
--   safely disregard the translations.)
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
-- UTILITIES

-- | Converts radians to degrees.
degrees :: Float -> Float
degrees r = 180.0 * r / pi

-- | Converts degrees to radians.
radians :: Float -> Float
radians d = pi * d / 180.0

-- | Clamps a floating-point value to a given range.
clamp :: Float  -- ^ minimum allowed value
      -> Float  -- ^ maximum allowed value
      -> Float  -- ^ input value
      -> Float  -- ^ output value after having been clamped
clamp minx maxx x
  | x < minx  = minx
  | x > maxx  = maxx
  | otherwise = x

-- | Normalizes an angle to the range [0, 2*pi).
--
-- NOTE: normalizing angles is very tricky; double-check you have the right logic.
normAngle1 :: Float -> Float
normAngle1 x | x < 0       = normAngle1 (x + 2*pi)
             | x >= (2*pi) = normAngle1 (x - 2*pi)
             | otherwise   = x
