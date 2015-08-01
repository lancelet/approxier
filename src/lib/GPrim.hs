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
  -- * Types
    Sphere (
      Sphere
    , sphereRadius
    , sphereZMin
    , sphereZMax
    , spherePhiMax
    )
  ) where

-- |Sphere.
data Sphere = Sphere
  { sphereRadius :: !Float  -- ^ radius
  , sphereZMin   :: !Float  -- ^ minimum z value
  , sphereZMax   :: !Float  -- ^ maximum z value
  , spherePhiMax :: !Float  -- ^ angle about z-axis, measured from zero at +x (degrees)
  } deriving (Show)
