-- |

module Main where

import           Data.Word (Word8)

import           VecMath   (Cartesian3Tuple (xcomp, ycomp, zcomp, cartesian3Tuple, toVector3, toNormal3),
                            Normal3, Point3, dot,
                            offsetPointAlongVector, p3, Ray(..), WorldSpace, ObjectSpace, CameraSpace,
                            XForm, Transformable(xform), translate, InSpace(..), xformInv,
                            rotate, v3)

main :: IO ()
main =
  let
    actions :: [IO ()]
    actions = map (\frame -> writeFile (fileName frame) (testRender (fromIntegral frame))) [0 .. 359]
  in
    sequence_ actions

fileName :: Int -> String
fileName frame = (numToStr frame) ++ ".ppm"

numToStr :: Int -> String
numToStr x
  | x < 10    = "000" ++ (show x)
  | x < 100   = "00"  ++ (show x)
  | x < 1000  = "0"   ++ (show x)
  | x < 9999  = (show x)
  | otherwise = error "only handles numbers < 9999"

-- | Evaluates a ray at a parametric distance.
-- rayAt = P3 + N3 * t
rayAt :: Ray -> Float -> Point3
rayAt (Ray p v) t = offsetPointAlongVector v t p

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



-- |Something that carries a transformation to world coordinates.
data HasTransform nativecsys a = HasTransform (XForm nativecsys WorldSpace) a

withTransform :: XForm nativecsys WorldSpace -> a -> HasTransform nativecsys a
withTransform x thing = HasTransform x thing


-- | Trace a ray against a primitive.
data TracePrim = TracePrim
    { tracePrimRay :: InSpace ObjectSpace Ray -> Maybe (InSpace ObjectSpace Intersection)
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
    trace :: InSpace ObjectSpace Ray -> Maybe (InSpace ObjectSpace Intersection)
    trace (InSpace ray) = case quadratic a b c of
                 Nothing       -> Nothing
                 Just (t1, t2) -> (checkIntersection t1) `mOr` (checkIntersection t2)
                   where

                     mOr :: Maybe a -> Maybe a -> Maybe a
                     mOr (Just x1)         _ = Just x1
                     mOr   Nothing (Just x2) = Just x2
                     mOr         _         _ = Nothing

                     checkIntersection :: Float -> Maybe (InSpace ObjectSpace Intersection)
                     checkIntersection t = if isValid
                                              then Just (InSpace (Intersection isectp isectn))
                                              else Nothing
                       where
                         isValid = ((zcomp isectp) >= zMin && (zcomp isectp) <= zMax && (degrees phi) <= phiMax)
                         isectp = rayAt ray t
                         isectn = toNormal3 isectp
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
    :: Color                      -- ^ raw color of the surface (color to be modulated)
    -> InSpace csys Ray           -- ^ incoming intersection ray
    -> InSpace csys Intersection  -- ^ intersection of the ray and the surface
    -> Color                      -- ^ computed `dotShade` color
dotShade inColor (InSpace (Ray _ nr)) (InSpace (Intersection _ ni)) = c
  where
    c = mulColor (abs dp) inColor
    dp = dot (toVector3 nr) (toVector3 ni)

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
rayForRasterCoord :: Camera -> RasterParams -> RasterCoord -> InSpace CameraSpace Ray
rayForRasterCoord camera rasterParams rasterCoord = InSpace (Ray p v)
  where
    v = toVector3 (toNormal3 p)
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

-- | Trace a scene in one color.
dotShadeTrace
    :: Color                               -- ^ underlying color to shade everything in the scene
    -> RasterParams                        -- ^ parameters of the raster to create
    -> HasTransform CameraSpace Camera     -- ^ camera for the scene
    -> HasTransform ObjectSpace TracePrim  -- ^ primitive (entire scene) to trace
    -> Raster                              -- ^ raster output
dotShadeTrace color rasterParams cameraWithTransform tracePrimWithTransform = raster
  where
    raster = Raster w h pixels
    RasterParams w h = rasterParams
    pixels = map shadeTuple (zip objRays intersections)

    shadeTuple :: (InSpace ObjectSpace Ray, Maybe (InSpace ObjectSpace Intersection)) -> Color
    shadeTuple (ray, maybeIntersection) =
      case maybeIntersection of
        Just isect -> dotShade color ray isect
        Nothing    -> black

    intersections :: [Maybe (InSpace ObjectSpace Intersection)]
    intersections = map (tracePrimRay tracePrim) objRays

    objRays :: [InSpace ObjectSpace Ray]
    objRays = map (xform world2object) worldRays

    worldRays :: [InSpace WorldSpace Ray]
    worldRays = map (xform camera2world) camRays

    camRays :: [InSpace CameraSpace Ray]
    camRays = map (rayForRasterCoord camera rasterParams) rasterCoords

    rasterCoords = map (\(rx,ry) -> RasterCoord rx ry) rawRasterCoords
    rawRasterCoords = [((fromIntegral i) - 0.5, (fromIntegral j) - 0.5) | j <- [1..h], i <- [1..w]]

    world2object = xformInv object2world
    HasTransform object2world tracePrim = tracePrimWithTransform
    HasTransform camera2world camera = cameraWithTransform

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

-- | Test camera.
testCamera :: HasTransform CameraSpace Camera
testCamera = withTransform (translate 0 0 (-5.0)) $ Camera 45.0 0.01

-- | Test raster parameters.
testRasterParams :: RasterParams
testRasterParams = RasterParams 800 600

-- | Test rendering the scene (as PPM bitmap).
testRender :: Float -> String
testRender angle = rasterToPPM color2ImageColor raster
  where
    testScene = withTransform (rotate angle (v3 1 0 1)) $ sphereTracePrim $ Sphere 1.0 300.0 (-0.8) (0.8)
    raster = dotShadeTrace white testRasterParams testCamera testScene
