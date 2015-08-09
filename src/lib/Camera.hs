{-|

-}
module Camera (
      NDC (
          NDC
      )
    , Camera (
          cameraProjection
        , cameraTransform
      )
    , perspectiveCamera
  ) where

import Trace   (Ray (Ray, rayPoint, rayVector, rayMinParam, rayMaxParam))

import VecMath (normalize, p3, radians, v3, XForm, xformCompose, xform, Transform)

-- | Normalized device coordinate.
--
--   NDCs run from 0 to 1 along x and y. (0,0) is the top left of the frame, (1,1) is the bottom
--   right of the frame.
data NDC = NDC !Float !Float deriving (Show)

instance Num NDC where
  (+) (NDC x1 y1) (NDC x2 y2) = NDC (x1 + x2) (y1 + y2)
  (-) (NDC x1 y1) (NDC x2 y2) = NDC (x1 - x2) (y1 - y2)
  negate (NDC x y) = NDC (-x) (-y)
  (*)         = error "(*) undefined for NDC"
  abs         = error "abs undefined for NDC"
  signum      = error "signum undefined for NDC"
  fromInteger = error "fromInteger undefined for NDC"

-- | Camera.
data Camera = Camera {
    -- | Camera projection.
    --
    --   Takes an NDC coordinate in the camera's imaging system and computes a ray that should be
    --   traced into the scene, corresponding to that coordinate.
    cameraProjection :: NDC -> Ray

    -- | Transforms a camera.
  , cameraTransform  :: XForm -> Camera
  }

instance Transform Camera where
  xform = flip cameraTransform

----------------------------------------------------------------------------------------------------
-- PERSPECTIVE CAMERA

perspectiveCamera :: XForm  -- ^ camera to world
                  -> Float  -- ^ x fov (degrees)
                  -> Float  -- ^ hither
                  -> Float  -- ^ yon
                  -> Float  -- ^ image aspect ratio, defined as width / height
                  -> Camera
perspectiveCamera c2w fovx hither yon aspect =
  Camera { cameraProjection = perspectiveCameraProjection c2w fovx hither yon aspect
         , cameraTransform  = perspectiveCameraTransform c2w fovx hither yon aspect }

----------------------------------------------------------------------------------------------------
-- PRIVATE FUNCTIONS

perspectiveCameraProjection :: XForm          -- ^ camera to world
                            -> Float          -- ^ x fov (degrees)
                            -> Float          -- ^ hither
                            -> Float          -- ^ yon
                            -> Float          -- ^ image aspect ratio: width / height
                            -> (NDC -> Ray)
perspectiveCameraProjection c2w fovx hither yon aspect (NDC nx ny) =
  let x   = 2*nx - 1
      y   = 2*ny - 1
      f2x = fovx / 2
      f2y = f2x / aspect
      hwx = hither * tan (radians f2x)
      hwy = hither * tan (radians f2y)
  in xform c2w Ray { rayPoint    = p3 0 0 0
                   , rayVector   = normalize $ v3 (x * hwx) (y * hwy) hither
                   , rayMinParam = hither
                   , rayMaxParam = yon    }

perspectiveCameraTransform :: XForm               -- ^ camera to world
                           -> Float               -- ^ x fov (degrees)
                           -> Float               -- ^ hither
                           -> Float               -- ^ yon
                           -> Float               -- ^ image aspect ratio: width / height
                           -> (XForm -> Camera)
perspectiveCameraTransform c2w fovx hither yon aspect x =
  let x' = xformCompose c2w x
  in perspectiveCamera x' fovx hither yon aspect
