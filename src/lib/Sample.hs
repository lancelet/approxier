{-|

-}
module Sample (
    simpleSample
  ) where

import Camera (Camera, NDC (NDC), cameraProjection)
import Raster (Color, Raster, avgColor, black, fromRowMajorPixelList)
import Scene  (SceneObject, SimpleShader, soShade)

-- | Samples a scene uniformly and produces a raster.
simpleSample :: SimpleShader  -- ^ default shader to be used if no other has been assigned
             -> (Int, Int)    -- ^ (width, height) of the raster to produce
             -> (Int, Int)    -- ^ number of x and y samples
             -> Camera        -- ^ camera to use for projection
             -> SceneObject   -- ^ scene to trace
             -> Raster
simpleSample defaultShader (w,h) nSamples camera scene =
  let genNDC x y = uniformPixelNDCSamples (w,h) nSamples (x,y)
      ndcss = [ genNDC x y
              | y <- [0 .. (h-1)]
              , x <- [0 .. (w-1)]
              ]
      pixels = map (sampleNDCs defaultShader camera scene) ndcss
  in fromRowMajorPixelList (w,h) pixels

----------------------------------------------------------------------------------------------------
-- PRIVATE

-- | Given a set of NDC coordinates, find a single color that is the average result of sampling them
--   in a scene.
sampleNDCs :: SimpleShader
           -> Camera
           -> SceneObject
           -> [NDC]
           -> Color
sampleNDCs defaultShader camera scene ndcs =
  let camProj    = cameraProjection camera
      shade      = soShade scene defaultShader scene
      forceColor = maybe black id
  in avgColor $ map (forceColor . shade . camProj) ndcs

-- | Computes the NDC samples for a pixel.
uniformPixelNDCSamples :: (Int, Int)  -- ^ (width, height) of raster
                       -> (Int, Int)  -- ^ number of x and y samples
                       -> (Int, Int)  -- ^ (x,y) coordinates of pixel (zero based)
                       -> [NDC]       -- list of uniformly-distributed NDC samples for this pixel
uniformPixelNDCSamples (w, h) (nx, ny) (x, y) =
  let i   = fromIntegral
      ofs = (+) $ NDC ((i x) / (i w)) ((i y) / (i h))
  in map ofs $ uniformPixelNDCTemplate (w, h) (nx, ny)

-- | Distributes NDC samples across the space of a pixel at the origin.
uniformPixelNDCTemplate :: (Int, Int)  -- ^ (width, height) of raster
                        -> (Int, Int)  -- ^ number of x and y samples
                        -> [NDC]       -- ^ NDC samples for a pixel at the origin
uniformPixelNDCTemplate (w, h) (nx, ny) =
  let i     = fromIntegral
      fx x  = (i x) / (i w) / (i nx)
      fy y  = (i y) / (i h) / (i ny)
      f x y = NDC (fx x) (fy y)
  in [ f x y
     | y <- [1 .. ny]
     , x <- [1 .. nx]
     ]
