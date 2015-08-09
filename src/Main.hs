-- |

module Main where

import VecMath       (Cartesian3Tuple (toVector3, toNormal3),
                      Normal3, Point3, Transformable (xform), Vector3, XForm, dot,
                      normalize, offsetPointAlongVector, p3, rotate, translate, v3, (.*),
                      (⋅), (⨯), radians, degrees, xformId, xformCompose)

import GPrim         (Hyperboloid(Hyperboloid), Torus(Torus))

import Chess         (pawn)

import Trace         (TracePrim, trace, Intersection(isectP, isectN),
                      Ray(Ray))

import Hyperboloid  (hyperboloidTracePrim)

import Data.Word     (Word8)

import Codec.Picture (DynamicImage (ImageRGB8), PixelRGB8 (PixelRGB8), generateFoldImage,
                      savePngImage)

import Torus (torusTracePrim)

-- import Control.Concurrent.Async (mapConcurrently)

import System.Random (RandomGen, getStdGen, randomRs, split)

main :: IO ()
main =
  let
    renderFrame :: RandomGen a => (a, Int) -> IO ()
    renderFrame (rng, frame) =
      let
        angle  = fromIntegral (frame)
        raster = testAORender rng angle -- testTorus rng angle  -- testAORender rng angle --testHyperboloid rng angle
        file   = fileName frame
      in savePngRaster color2ImageColor raster file

    frames :: [Int]
    frames = [100 .. 100]
  in
    do
      rng <- getStdGen
      sequence_ (map renderFrame (zip (randomGenList rng) frames))
  -- do
  --   rng <- getStdGen
  --   savePngRaster color2ImageColor (testAORender rng 90) "test.png"

fileName :: Int -> String
fileName frame = (numToStr frame) ++ ".png"

numToStr :: Int -> String
numToStr x
  | x < 10    = "000" ++ (show x)
  | x < 100   = "00"  ++ (show x)
  | x < 1000  = "0"   ++ (show x)
  | x < 9999  = (show x)
  | otherwise = error "only handles numbers < 9999"

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

-- |Mixes two colors.
mixColor :: Float -> Color -> Color -> Color
mixColor c (Color r1 g1 b1) (Color r2 g2 b2) =
  let mix x1 x2 = (1.0 - c) * x1 + c * x2
  in Color (mix r1 r2) (mix g1 g2) (mix b1 b2)

-- | Color for raster output.
data ImageColor =
    ImageColor {-# UNPACK #-} !Word8 !Word8 !Word8
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

-- |Something that carries a transformation to world coordinates.
data HasTransform a = HasTransform XForm a

withTransform :: XForm -> a -> HasTransform a
withTransform x thing = HasTransform x thing

-- | Computes the color of a surface as a simple dot product between the incoming ray direction
--   and the surface normal.
dotShade
    :: Color         -- ^ raw color of the surface (color to be modulated)
    -> Ray           -- ^ incoming intersection ray
    -> Intersection  -- ^ intersection of the ray and the surface
    -> Color         -- ^ computed `dotShade` color
dotShade inColor (Ray _ nr _ _) isect = c
  where
    ni = isectN isect
    c = mulColor (abs dp) inColor
    dp = dot (toVector3 nr) (toVector3 ni)

-- | Simple camera model.
data Camera = Camera
    { cameraFov    :: Float
    , cameraHither :: Float
    , cameraYon    :: Float
    }

-- |Finds the parametric range for rays shot from a given camera (assuming unit length ray vector).
rayParametricRangeForCamera :: Camera -> (Float, Float)
rayParametricRangeForCamera camera =
  let
    hither = cameraHither camera
    yon    = cameraYon    camera
  in (hither, yon)

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

rasterCoordOfs :: Float -> Float -> RasterCoord -> RasterCoord
rasterCoordOfs dx dy (RasterCoord x y) = RasterCoord (x + dx) (y + dy)

-- |Creates a list of raster coordinates that traverses a raster in row-major order.
traverseRasterRowMajor :: Int                -- ^ width of the raster
                       -> Int                -- ^ height of the raster
                       -> [ RasterCoord ]    -- ^ list of raster coordinates
traverseRasterRowMajor w h =
  let
    shift x = (fromIntegral x) - 0.5
    mkrc i j = RasterCoord (shift i) (shift j)
  in [ mkrc i j | j <- [1..h], i <- [1..w] ]

-- |Creates a list of lists of raster coordinates that regularly sample a raster in row-major order.
-- One list is created per pixel.
samplesRasterRowMajor :: Int                   -- ^ width of raster
                      -> Int                   -- ^ height of raster
                      -> Int                   -- ^ n samples x
                      -> Int                   -- ^ n samples y
                      -> [ [ RasterCoord ] ]   -- ^ list of lists of raster coordinates
samplesRasterRowMajor w h nx ny =
  let templ    = samplePixel nx ny
      mkrc i j = map (rasterCoordOfs (fromIntegral i) (fromIntegral j)) templ
  in [ mkrc i j | j <- [1..h], i <- [1..w] ]

-- |Creates a template of even samples for a single pixel. The samples span from (0,0) to (1.0,1.0).
samplePixel :: Int                -- ^ n samples x
            -> Int                -- ^ n samples y
            -> [ RasterCoord ]    -- ^ samples in the pixel, centred at 0.5, 0.5
samplePixel nx ny =
  let nxf      = fromIntegral nx
      nyf      = fromIntegral ny
      dx       = 1.0 / (2.0 * nxf)
      dy       = 1.0 / (2.0 * nyf)
      mkrc i j = RasterCoord ((fromIntegral i) / nxf - dx) ((fromIntegral j) / nyf - dy)
  in [ mkrc i j | j <- [1..nx], i <- [1..ny] ]

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
rayForRasterCoord camera rasterParams rasterCoord = Ray p v hither yon
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
    Camera fovX hither yon = camera
    NDC ndcx ndcy = rasterToNDC rasterParams rasterCoord
    RasterParams w h = rasterParams

-- | Trace a scene in one color.
dotShadeTrace
    :: Color                   -- ^ underlying color to shade everything in the scene
    -> RasterParams            -- ^ parameters of the raster to create
    -> HasTransform Camera     -- ^ camera for the scene
    -> TracePrim               -- ^ primitive (entire scene) to trace
    -> Raster                  -- ^ raster output
dotShadeTrace color rasterParams cameraWithTransform tracePrim = raster
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
    intersections = map (trace tracePrim) worldRays
    rp = rayParametricRangeForCamera camera

    worldRays :: [Ray]
    worldRays = map (xform camera2world) camRays

    camRays :: [Ray]
    camRays = map (rayForRasterCoord camera rasterParams) rasterCoords

    rasterCoords = traverseRasterRowMajor w h

    HasTransform camera2world camera = cameraWithTransform

----------------------------------------------------------------------------------------------------
-- AMBIENT OCCLUSION

-- |Transformation to align the +z axis with a given vector.
simpleZAlign :: Normal3 -> XForm
simpleZAlign normal =
  let
    z = v3 0 0 1
    n = toVector3 normal
    r = normalize (z ⨯ n)
    a = acos (z ⋅ n)
  in rotate (degrees a) r

-- |Samples a hemisphere uniformly.
sampleHemisphereUniformly :: RandomGen a
                          => a             -- ^ random generator
                          -> Int           -- ^ number of samples
                          -> Point3        -- ^ point from which to sample (center of sphere)
                          -> Normal3       -- ^ normal identifying the hemisphere
                          -> [ Ray ]       -- ^ list of rays sampling the hemisphere
sampleHemisphereUniformly rng nRays p n = rays -- [ Ray p (toVector3 n) ] -- rays
  where
    rays :: [ Ray ]
    rays = map (\v -> Ray p v 0.001 1e5) viewVecs

    viewVecs :: [ Vector3 ]
    viewVecs = map (xform viewVectorRot) rawViewVecs

    viewVectorRot :: XForm
    viewVectorRot = simpleZAlign n

    rawViewVecs :: [ Vector3 ]
    rawViewVecs = map xis2vector xis

    xis2vector :: (Float, Float) -> Vector3
    xis2vector (xi1, xi2) =
      let
        pt = 2.0 * pi * xi2
        rt = sqrt (1.0 - (xi1 * xi1))
        x = rt * (cos pt)
        y = rt * (sin pt)
        z = xi1
      in normalize $ v3 x y z

    xis :: [ (Float, Float) ]
    xis =
      let
        (g1, g2) = split rng
        xi1s = randomRs (0.0, 1.0) g1
        xi2s = randomRs (0.0, 1.0) g2
      in take nRays (zip xi1s xi2s)

-- |Shades a point using an ambient occlusion calculation.
-- The intersection should be in the coordinates of the TracePrim (object coordinates).
shadeAO :: RandomGen a
        => a              -- ^ random generator
        -> Int            -- ^ number of samples
        -> (Float, Float) -- ^ hither yon
        -> Intersection   -- ^ intersection point
        -> TracePrim      -- ^ primitive (entire scene) to use for occluding ambient light
        -> Float          -- ^ the visibility of the scene
shadeAO rng nRays (hither, yon) isect tracePrim =
  let
    trace' = trace tracePrim
    p = isectP isect
    n = isectN isect
    
    rays :: [ Ray ]
    rays = map nudge (sampleHemisphereUniformly rng nRays p n)

    nudge :: Ray -> Ray
    nudge (Ray pr vr tmin tmax) = Ray (offsetPointAlongVector vr 0.05 pr) vr tmin tmax

    intersections :: [ (Ray, Maybe Intersection) ]
    intersections = zip rays (map trace' rays)

    raycontrib :: (Ray, Maybe Intersection) -> Float
    raycontrib ((Ray _ rayvector _ _), Nothing) = abs (rayvector ⋅ (toVector3 n))
    raycontrib                                _ = 0

    rayhits :: [ Float ]
    rayhits = map raycontrib intersections

    result :: Float
    result = (sum rayhits) / (fromIntegral nRays)

  in result

-- |Creates an infinite list of random generators from one.
randomGenList :: RandomGen a => a -> [a]
randomGenList rng =
  let (g1, g2) = split rng
  in g1 : randomGenList g2

-- |Trace a scene in one color using simple ambient occlusion.
ambientOcclusionSceneTrace
  :: RandomGen a
  => a                        -- ^ random generator for sampling ambient occlusion
  -> Int                      -- ^ number of AO rays
  -> Color                    -- ^ underlying color to shade everything in the scene
  -> RasterParams             -- ^ parameters of the raster to shade
  -> HasTransform Camera      -- ^ camera for the scene
  -> TracePrim                -- ^ primitive (entire scene) to trace
  -> Raster                   -- ^ raster output
ambientOcclusionSceneTrace rng nRays color rasp xcamera scene = raster
  where
    raster :: Raster
    raster = Raster width height pixels

    pixels = map shadeIntersection (zip rngs raysAndIntersections)

    hitheryon = (1e-4, 1e8)

    shadeIntersection (pxrng, (_,  Just i)) = mulColor (shadeAO pxrng nRays hitheryon i scene) color
    shadeIntersection _ = black

    rngs = randomGenList rng

    raysAndIntersections :: [ (Ray, Maybe Intersection) ]
    raysAndIntersections = map faceforward (zip worldRays intersections)

    faceforward :: (Ray, Maybe Intersection) -> (Ray, Maybe Intersection)
    faceforward (r, Nothing) = (r, Nothing)
    faceforward (r@(Ray _ v _ _), Just isect) =
      let
        n = isectN isect
        nvec = toVector3 n
        nvec' = if (v ⋅ nvec) < 0 then nvec else (nvec .* (-1.0))
        n' = toNormal3 nvec'
      in (r, Just $ isect { isectN = n' })

    intersections :: [ Maybe Intersection ]
    intersections = map (trace scene) worldRays
    (mint, maxt) = rayParametricRangeForCamera cam

    worldRays :: [ Ray ]
    worldRays = map (xform cam2world) camRays

    camRays :: [ Ray ]
    camRays =
      let getRay = rayForRasterCoord cam rasp
      in map getRay rc

    rc :: [ RasterCoord ]
    rc = traverseRasterRowMajor width height

    RasterParams width height  = rasp
    HasTransform cam2world cam = xcamera

-- |Test rendering the scene using raytraced ambient occlusion.
testAORender :: RandomGen a
             => a
             -> Float
             -> Raster
testAORender rng angle = raster
  where
    rotobj     = rotate angle (v3 1 0 0)
    transobj   = translate 0 0 (-25)
    testScene  = xform (xformCompose transobj rotobj) pawn
    testCam    = withTransform (translate 0 0 (-120)) $ Camera 45.0 0.01 100000.0
    testRP     = RasterParams 800 600
    nAOSamples = 32
    raster     = ambientOcclusionSceneTrace rng nAOSamples white testRP testCam testScene

-- |Test rendering a hyperboloid.
testHyperboloid :: RandomGen a
                => a
                -> Float
                -> Raster
testHyperboloid rng angle =
  let
    cam        = withTransform (translate 0 0 (-10)) $ Camera 45.0 0.01 100000.0
    object     = hyperboloidTracePrim xformId $ Hyperboloid (p3 1 0 0) (p3 1 0 1) 90
    scene      = xform (rotate angle (v3 1 0 0)) object
    rasp       = RasterParams 400 300
    nAOSamples = 256
  in ambientOcclusionSceneTrace rng nAOSamples white rasp cam scene

-- |Test rendering a torus.
testTorus :: RandomGen a
          => a
          -> Float
          -> Raster
testTorus rng angle =
  let
    cam        = withTransform (translate 0 0 (-10)) $ Camera 45.0 0.01 100000.0
    object     = torusTracePrim xformId $ Torus 1 0.5 (-250) 0 330
    scene      = xform (rotate angle (v3 1 0 0)) object
    rasp       = RasterParams 400 300
    nAOSamples = 32
  in ambientOcclusionSceneTrace rng nAOSamples white rasp cam scene


----------------------------------------------------------------------------------------------------
-- JUICY PIXELS STUFF

savePngRaster :: (Color -> ImageColor) -> Raster -> FilePath -> IO ()
savePngRaster transfer raster file = savePngImage file (rasterToImage transfer raster)

rasterToImage :: (Color -> ImageColor) -> Raster -> DynamicImage
rasterToImage transfer (Raster w h px) =
  let
    f (color:remainder) _ _ = (remainder, imageColorToPixel $ transfer color)
    f []                _ _ = error "attempted to extract too many pixels"
  in ImageRGB8 $ snd $ generateFoldImage f px w h

imageColorToPixel :: ImageColor -> PixelRGB8
imageColorToPixel (ImageColor r g b) = PixelRGB8 r g b
