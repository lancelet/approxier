{-|

-}
module Raster where

import Data.Array (Array, (!))

import GHC.Arr (Ix(range, unsafeIndex, inRange, unsafeRangeSize))

import Data.Word (Word8)

import Codec.Picture (DynamicImage(ImageRGB8), PixelRGB8(PixelRGB8), generateImage,
                      savePngImage)

import VecMath   (clamp)

----------------------------------------------------------------------------------------------------
-- FLOATING-POINT COLOR

-- Eventually, color will be handled in a separate module; quick-and-dirty for now

-- | Floating-point trinary color for within the renderer.
data Color = Color {-# UNPACK #-} !Float !Float !Float deriving (Show)

black, white :: Color
black = Color 0 0 0
white = Color 1 1 1

-- | Scale all components of a color by a scalar.
mulColor :: Float -> Color -> Color
mulColor x (Color r g b) = Color (x*r) (x*g) (x*b)

----------------------------------------------------------------------------------------------------
-- COLOR FOR INPUT / OUTPUT

-- | Word8 trinary color for I/O purposes.
data ImageColor = ImageColor {-# UNPACK #-} !Word8 !Word8 !Word8 deriving (Show)

-- | Convert a color from an I/O image to an internal renderer color.
imageColorToColor :: ImageColor -> Color
imageColorToColor (ImageColor r g b) =
  let f x = fromIntegral x / 255
  in Color (f r) (f g) (f b)

-- | Convert an internal renderer color to a color for an I/O image.
colorToImageColor :: Color -> ImageColor
colorToImageColor (Color r g b) =
  let f x = fromInteger $ round $ clamp 0 255 (x * 255)
  in ImageColor (f r) (f g) (f b)

----------------------------------------------------------------------------------------------------
-- RASTER

data Raster = Raster { rasterWidth  :: !Int
                     , rasterHeight :: !Int
                     , rasterPixels :: Array RasterPxCoord Color
                     }

data RasterPxCoord = RasterPxCoord {-# UNPACK #-} !Int !Int deriving (Eq, Ord, Show)

data RasterFloatCoord = RasterFloatCoord {-# UNPACK #-} !Float !Float deriving (Show)

instance Ix RasterPxCoord where
  {-# INLINE range #-}
  range (RasterPxCoord l1 l2, RasterPxCoord u1 u2) =
    [ RasterPxCoord i1 i2 | i1 <- range (l1, u1), i2 <- range(l2, u2) ]

  {-# INLINE unsafeIndex #-}
  unsafeIndex (RasterPxCoord l1 l2, RasterPxCoord u1 u2) (RasterPxCoord i1 i2) =
      unsafeIndex     (l1, u1) i1
    * unsafeRangeSize (l2, u2)
    + unsafeIndex     (l2, u2) i2

  {-# INLINE inRange #-}
  inRange (RasterPxCoord l1 l2, RasterPxCoord u1 u2) (RasterPxCoord i1 i2) =
      inRange (l1, u1) i1
   && inRange (l2, u2) i2

----------------------------------------------------------------------------------------------------
-- LOADING / SAVING RASTERS VIA JUICYPIXELS

saveRasterPng :: Raster -> FilePath -> IO ()
saveRasterPng raster filePath = savePngImage filePath $ rasterToJuicyPixelsImage raster

rasterToJuicyPixelsImage :: Raster -> DynamicImage
rasterToJuicyPixelsImage (Raster w h pixels) =
  let p (ImageColor r g b) = PixelRGB8 r g b
      f x y                = p $ colorToImageColor $ pixels ! (RasterPxCoord x y)
  in ImageRGB8 $ generateImage f w h
