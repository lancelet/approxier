-- |

module Main where

import           Data.Word (Word8)

import           VecMath   (Cartesian3Tuple (xcomp, ycomp, zcomp, cartesian3Tuple),
                            Normal3, Point3, Vector3, dot, n3v3, normalize, p3,
                            p3v3, v3)

main :: IO ()
main = writeFile "test.ppm" testRender

-- | Ray.
data Ray =
    Ray Point3
        Normal3
    deriving (Show)

-- | Evaluates a ray at a parametric distance.
-- rayAt = P3 + N3 * t
rayAt :: Ray -> Float -> Point3
rayAt (Ray p n) t = p3 x' y' z'
  where
    x' = (xcomp p) + t * (xcomp n)
    y' = (ycomp p) + t * (ycomp  n)
    z' = (zcomp p) + t * (zcomp n)

-- | Color.
data Color =
    Color Float
          Float
          Float
    deriving (Show)

-- | The color black.
black :: Color
black = Color 0.0 0.0 0.0

-- | The color white.
white :: Color
white = Color 1.0 1.0 1.0

-- | The color green.
green :: Color
green = Color 0.0 1.0 0.0

-- | Scales all components of a color value by a scalar.
mulColor :: Float -> Color -> Color
mulColor c (Color r g b) = Color (c * r) (c * g) (c * b)

-- | Color for raster output.
data ImageColor =
    ImageColor Word8
               Word8
               Word8
    deriving (Show)

-- | Simple color 2 image color transformation (0 -> 1) -> (0 -> 255).
color2ImageColor :: Color -> ImageColor
color2ImageColor (Color r g b) = ImageColor (f r) (f g) (f b)
  where
    f :: Float -> Word8
    f c = fromInteger $ round (clamp 0.0 255.0 (c * 255.0))

-- | Clamps a Float to some range.
clamp :: Float -> Float -> Float -> Float
clamp minc maxc c
  | c < minc  = minc
  | c > maxc  = maxc
  | otherwise = c

-- | Sphere.
data Sphere = Sphere
    { sphereRadius :: Float
    , spherePhiMax :: Float
    , spherezMin   :: Float
    , spherezMax   :: Float
    } deriving (Show)

-- | Intersection with a primitive.
data Intersection = Intersection
    { intersectionP :: Point3
    , intersectionN :: Normal3
    } deriving (Show)

-- | Trace a ray against a primitive.
data TracePrim = TracePrim
    { tracePrimRay :: Ray -> Maybe Intersection
    }

-- | Converts degrees to radians.
radians :: Float -> Float
radians d = d * pi / 180.0

-- | Converts radians to degrees.
degrees :: Float -> Float
degrees r = r * 180.0 / pi

-- | Solves the real roots of the quadratic equation.
-- a * t^2 + b * t + c = 0
-- The smaller of the two roots is returned first in the tuple.
quadratic :: Float -> Float -> Float -> Maybe (Float, Float)
quadratic a b c = if (discrim <= 0.0)
                     then Nothing
                     else Just (t1, t2)
   where
     discrim = (b * b) - (4.0 * a * c)
     t1 = min ta tb
     t2 = max ta tb
     ta = q / a
     tb = c / q
     q | b < 0     = -0.5 * (b - sqrt discrim)
       | otherwise = -0.5 * (b + sqrt discrim)

-- | Traceable sphere.
sphereTracePrim :: Sphere -> TracePrim
sphereTracePrim (Sphere r phiMax zMin zMax) = TracePrim trace
  where
    trace :: Ray -> Maybe Intersection
    trace ray = case quadratic a b c of
                 Nothing       -> Nothing
                 Just (t1, t2) -> (checkIntersection t1) `mOr` (checkIntersection t2)
                   where

                     mOr :: Maybe a -> Maybe a -> Maybe a
                     mOr (Just x1)         _ = Just x1
                     mOr   Nothing (Just x2) = Just x2
                     mOr         _         _ = Nothing

                     checkIntersection :: Float -> Maybe Intersection
                     checkIntersection t = if isValid
                                              then Just (Intersection isectp isectn)
                                              else Nothing
                       where
                         isValid = ((zcomp isectp) >= zMin && (zcomp isectp) <= zMax && (degrees phi) <= phiMax)
                         isectp = rayAt ray t
                         isectn = normalize (p3v3 isectp)
                         phi = if phi' < 0.0
                                  then phi' + 2.0 * pi
                                  else phi'
                         phi' = atan2 (ycomp isectp) (xcomp isectp)

      where
        a = nx * nx + ny * ny + nz * nz
        b = 2.0 * (nx * px + ny * py + nz * pz)
        c = px * px + py * py + pz * pz - r * r
        (px, py, pz) = cartesian3Tuple p
        (nx, ny, nz) = cartesian3Tuple n
        Ray p n = ray

-- | Computes the color of a surface as a simple dot product between the incoming ray direction
--   and the surface normal.
dotShade
    :: Color         -- ^ raw color of the surface (color to be modulated)
    -> Ray           -- ^ incoming intersection ray
    -> Intersection  -- ^ intersection of the ray and the surface
    -> Color         -- ^ computed `dotShade` color
dotShade inColor (Ray _ nr) (Intersection _ ni) = c
  where
    c = mulColor (abs dp) inColor
    dp = dot (n3v3 nr) (n3v3 ni)

-- | Simple camera model.
data Camera = Camera
    { cameraFov    :: Float
    , cameraHither :: Float
    }

-- | Raster parameters.
data RasterParams = RasterParams
    { rasterParamsWidth  :: Int
    , rasterParamsHeight :: Int
    }

-- | Raster.
data Raster = Raster
    { rasterWidth  :: Int
    , rasterHeight :: Int
    , rasterPixels :: [Color]
    }

-- | Pixel coordinate in the raster.
data RasterCoord = RasterCoord
    { rasterCoordX :: Float
    , rasterCoordY :: Float
    }

-- | Normalize device coordinates (-1 to 1).
data NDC = NDC
    { ndcX :: Float
    , ndcY :: Float
    }

-- | Convert raster coordinates to NDC coordinates.
rasterToNDC :: RasterParams -> RasterCoord -> NDC
rasterToNDC (RasterParams w h) (RasterCoord rx ry) = NDC ndcx ndcy
  where
    ndcx = 2.0 * rx / (fromIntegral w) - 1.0
    ndcy = 2.0 * ry / (fromIntegral h) - 1.0

-- | Computes a ray for the given raster coordinates.
--   The ray starts at the hither plane.
rayForRasterCoord :: Camera -> RasterParams -> RasterCoord -> Ray
rayForRasterCoord camera rasterParams rasterCoord = Ray p n
  where
    n = normalize (p3v3 p)
    p = p3 rayx rayy hither
    rayx = ndcx * hwx
    rayy = ndcy * hwy
    hwx = hither * tan (radians f2X)
    hwy = hither * tan (radians f2Y)
    f2X = fovX / 2.0
    f2Y = fovY / 2.0
    fovY = ((fromIntegral h) / (fromIntegral w)) * fovX
    Camera fovX hither = camera
    NDC ndcx ndcy = rasterToNDC rasterParams rasterCoord
    RasterParams w h = rasterParams

-- | Transformation.
data XForm = XForm
  { xformPt     :: Point3 -> Point3
  , xformVec    :: Vector3 -> Vector3
  , xformPtInv  :: Point3 -> Point3
  , xformVecInv :: Vector3 -> Vector3
  }

-- | Identity transformation.
idXForm :: XForm
idXForm = XForm id id id id

-- | Invert a transformation.
invXForm :: XForm -> XForm
invXForm (XForm p v p' v') = XForm p' v' p v

-- | Translate
translate :: Vector3 -> XForm
translate v = XForm xfp id xfpInv id
  where
    xfp p = p3 ((xcomp p) + tx) ((ycomp p) + ty) ((zcomp p) + tz)
    xfpInv p = p3 ((xcomp p) - tx) ((ycomp p) - ty) ((zcomp p) - tz)
    (tx, ty, tz) = cartesian3Tuple v

-- | Transform a normal vector.
xformNormal :: XForm -> Normal3 -> Normal3
xformNormal (XForm _ xfv _ _) n = normalize (xfv (n3v3 n))

-- | Transforms a ray.
xformRay :: XForm -> Ray -> Ray
xformRay xf@(XForm xfp _ _ _) (Ray p n) = Ray (xfp p) (xformNormal xf n)

-- | Transforms a TracePrim.
xformTracePrim :: XForm -> TracePrim -> TracePrim
xformTracePrim xf (TracePrim tpr) = TracePrim tpr'
  where
    tpr' ray = tpr (xformRay xf ray)

-- | Trace a scene in one color.
dotShadeTrace
    :: Color           -- ^ underlying color to shade everything in the scene
    -> RasterParams    -- ^ parameters of the raster to create
    -> Camera          -- ^ camera for the scene
    -> XForm           -- ^ world-to-camera transformation
    -> TracePrim       -- ^ primitive (entire scene) to trace
    -> Raster          -- ^ raster output
dotShadeTrace color rasterParams camera camera2world tracePrim = raster
  where
    raster = Raster w h pixels
    RasterParams w h = rasterParams
    pixels = map shadeTuple (zip worldRays intersections)

    shadeTuple :: (Ray, Maybe Intersection) -> Color
    shadeTuple (ray, maybeIntersection) =
      case maybeIntersection of
        Just isect -> dotShade color ray isect
        Nothing    -> black

    intersections :: [Maybe Intersection]
    intersections = map (tracePrimRay tracePrim) worldRays

    worldRays = map (xformRay camera2world) camRays
    camRays = map (rayForRasterCoord camera rasterParams) rasterCoords
    rasterCoords = map (\(rx,ry) -> RasterCoord rx ry) rawRasterCoords
    rawRasterCoords = [((fromIntegral i) - 0.5, (fromIntegral j) - 0.5) | j <- [1..h], i <- [1..w]]

-- | Splits a list into constant-size sub-lists.
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

-- | Converts a Raster to a PPM bitmap.
rasterToPPM :: (Color -> ImageColor) -> Raster -> String
rasterToPPM f (Raster w h pixels) = ppm
  where
    ppm = unlines ([ "P3"
                   , (show w) ++ " " ++ (show h)
                   , "255"
                   ] ++ rows)

    rows :: [String]
    rows = map unwords (splitEvery w ppmColors)

    ppmColors :: [String]
    ppmColors = map color2ppm imageColors

    color2ppm :: ImageColor -> String
    color2ppm (ImageColor r g b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b)

    imageColors :: [ImageColor]
    imageColors = map f pixels

-- | Test scene.
testScene :: TracePrim
testScene = sphereTracePrim (Sphere 1.0 300.0 (-0.8) (0.8))

-- | Test camera.
testCamera :: Camera
testCamera = Camera 45.0 0.01

-- | Test camera transformation.
testCameraXForm :: XForm
testCameraXForm = translate (v3 0.0 0.0 (-5.0))

-- | Test raster parameters.
testRasterParams :: RasterParams
testRasterParams = RasterParams 3840 2160

-- | Test rendering the scene (as PPM bitmap).
testRender :: String
testRender = rasterToPPM color2ImageColor raster
  where
    raster = dotShadeTrace white testRasterParams testCamera testCameraXForm testScene
