{-|

-}
module Image (
  Image
  ) where

import VecMath (UVCoord)

import Raster (Color)


-- | An image takes a UVCoord and returns a Color.
--
--   Images are conceptually continuous.
type Image = UVCoord -> Color
