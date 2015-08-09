{-|

-}
module Raster2 (
    Raster (
        Raster
      , rasterWidth
      , rasterHeight
      , rasterPixels
    )
  , generateRasterRowMajor
  , rasterToList
  , RasterRgn
  , generateRasterRgnRowMajor
  , rasterRgnToRaster
  ) where

import Prelude              hiding (length)

import Control.Monad        (msum)

import Data.Vector.Storable (Storable, Vector, fromList, generate, length, (!))

----------------------------------------------------------------------------------------------------

data Raster a = Raster
  { rasterWidth  :: !Int
  , rasterHeight :: !Int
  , rasterPixels :: Vector a
  }

-- | Creates a raster from a generating function.
generateRaster :: Storable a
               => Int               -- ^ width
               -> Int               -- ^ height
               -> ((Int, Int) -> a) -- ^ function for a pixel
               -> Raster a
generateRaster w h f =
  let g i = f(i `quot`  w, i `mod` w)
  in Raster { rasterWidth  = w
            , rasterHeight = h
            , rasterPixels = generate (w*h) g }

-- | Creates a raster from an array of pixels in row-major order.
generateRasterRowMajor :: Storable a
                       => Int              -- ^ width
                       -> Int              -- ^ height
                       -> [a]              -- ^ pixels, stored in a row-major list
                       -> Maybe (Raster a) -- ^ produced raster
generateRasterRowMajor w h px
  | length pxv == (w * h) = Just r
  | otherwise             = Nothing
  where
    pxv = fromList px
    r = Raster { rasterWidth  = w
               , rasterHeight = h
               , rasterPixels = pxv }

rasterToList :: Raster a -> [a]
rasterToList = undefined

----------------------------------------------------------------------------------------------------

data RasterRgn a = RasterRgn
  { rasterRgnOfsX     :: !Int
  , rasterRgnOfsY     :: !Int
  , rasterRgnWidth    :: !Int
  , rasterRgnHeight   :: !Int
  , rasterRgnGetPixel :: (Int, Int) -> Maybe a
  }

instance Monoid (RasterRgn a) where
  mempty  = emptyRasterRgn
  mappend = addRasterRgn

emptyRasterRgn :: RasterRgn a
emptyRasterRgn = RasterRgn 0 0 0 0 (const Nothing)

addRasterRgn :: RasterRgn a -> RasterRgn a -> RasterRgn a
addRasterRgn ra rb =
  let (RasterRgn ox1 oy1 w1 h1 px1) = ra
      (RasterRgn ox2 oy2 w2 h2 px2) = rb
      ofsx     = min ox1 ox2
      ofsy     = min oy1 oy2
      maxx     = max (ox1 + w1) (ox2 + w2)
      maxy     = max (oy1 + h1) (oy2 + h2)
      w        = maxx - ofsx
      h        = maxy - ofsy
      px (x,y) = msum [ px1 (x,y), px2 (x,y) ]
  in RasterRgn ofsx ofsy w h px

generateRasterRgnRowMajor :: Storable a
                          => Int -- ^ offset x
                          -> Int -- ^ offset y
                          -> Int -- ^ width
                          -> Int -- ^ height
                          -> [a] -- ^ pixels, stored in a row-major list
                          -> Maybe (RasterRgn a)
generateRasterRgnRowMajor ofsx ofsy w h px
  | length pxv == (w * h) = Just r
  | otherwise             = Nothing
  where
    pxv = fromList px
    xInRange x = (x >= ofsx) && (x < (ofsx + w))
    yInRange y = (y >= ofsy) && (y < (ofsy + h))
    getPixel (x, y) | (xInRange x) && (yInRange y) = Just $ pxv ! (x - ofsx + (y - ofsy)*w)
                    | otherwise                    = Nothing
    r = RasterRgn { rasterRgnOfsX     = ofsx
                  , rasterRgnOfsY     = ofsy
                  , rasterRgnWidth    = w
                  , rasterRgnHeight   = h
                  , rasterRgnGetPixel = getPixel }

rasterRgnToRaster :: Storable a
                  => Int         -- ^ width of final image
                  -> Int         -- ^ height of final image
                  -> a           -- ^ default pixel value (if otherwise empty)
                  -> RasterRgn a -- ^ raster region to convert
                  -> Raster a    -- ^ produced raster
rasterRgnToRaster w h dflt rgn =
  let f (x, y) = maybe dflt id $ (rasterRgnGetPixel rgn) (x, y)
  in generateRaster w h f
