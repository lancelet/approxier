-- |

module Main where

import VecMath                  (Cartesian3Tuple (xcomp, ycomp, zcomp, cartesian3Tuple, toVector3, toNormal3),
                                 Normal3, Point3, Vector3, Ray (..), Transformable (xform), XForm, dot,
                                 normalize, offsetPointAlongVector, p3, rotate, translate, v3,
                                 xformCompose, xformInv, (⋅), (⨯), (.*))

import Data.Word                (Word8)

import Codec.Picture            (DynamicImage (ImageRGB8), PixelRGB8 (PixelRGB8), generateFoldImage,
                                 savePngImage)

import Control.Concurrent.Async (mapConcurrently)

import System.Random            (RandomGen, getStdGen, randomRs, split)

import qualified Debug.Trace as DT (trace)


main :: IO ()
main =
  let
    renderFrame :: RandomGen a => (a, Int) -> IO ()
    renderFrame (rng, frame) =
      let
        raster = testAORender rng (fromIntegral frame)
        file   = fileName frame
      in savePngRaster color2ImageColor raster file
  in do
     rng <- getStdGen
     --_   <- mapConcurrently renderFrame (zip (randomGenList rng) [0 .. 359])
     --return ()
     sequence_ (map renderFrame (zip (randomGenList rng) [0 .. 359]))

fileName :: Int -> String
fileName frame = (numToStr frame) ++ ".png"

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
data HasTransform a = HasTransform XForm a

withTransform :: XForm -> a -> HasTransform a
withTransform x thing = HasTransform x thing


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
                         isValid = ((zcomp isectp) >= zMin && (zcomp isectp) <= zMax && (degrees phi) <= phiMax && t > 0)
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
    :: Color         -- ^ raw color of the surface (color to be modulated)
    -> Ray           -- ^ incoming intersection ray
    -> Intersection  -- ^ intersection of the ray and the surface
    -> Color         -- ^ computed `dotShade` color
dotShade inColor (Ray _ nr) (Intersection _ ni) = c
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

-- |Creates a list of raster coordinates that traverses a raster in row-major order.
traverseRasterRowMajor :: Int                -- ^ width of the raster
                       -> Int                -- ^ height of the raster
                       -> [ RasterCoord ]    -- ^ list of raster coordinates
traverseRasterRowMajor w h =
  let
    shift x = (fromIntegral x) - 0.5
    mkrc i j = RasterCoord (shift i) (shift j)
  in [ mkrc i j | j <- [1..h], i <- [1..w] ]

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
rayForRasterCoord camera rasterParams rasterCoord = Ray p v
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
    :: Color                   -- ^ underlying color to shade everything in the scene
    -> RasterParams            -- ^ parameters of the raster to create
    -> HasTransform Camera     -- ^ camera for the scene
    -> HasTransform TracePrim  -- ^ primitive (entire scene) to trace
    -> Raster                  -- ^ raster output
dotShadeTrace color rasterParams cameraWithTransform tracePrimWithTransform = raster
  where
    raster = Raster w h pixels
    RasterParams w h = rasterParams
    pixels = map shadeTuple (zip objRays intersections)

    shadeTuple :: (Ray, Maybe Intersection) -> Color
    shadeTuple (ray, maybeIntersection) =
      case maybeIntersection of
        Just isect -> dotShade color ray isect
        Nothing    -> black

    intersections :: [Maybe Intersection]
    intersections = map (tracePrimRay tracePrim) objRays

    objRays :: [Ray]
    objRays = map (xform world2object) worldRays

    worldRays :: [Ray]
    worldRays = map (xform camera2world) camRays

    camRays :: [Ray]
    camRays = map (rayForRasterCoord camera rasterParams) rasterCoords

    rasterCoords = traverseRasterRowMajor w h

    world2object = xformInv object2world
    HasTransform object2world tracePrim = tracePrimWithTransform
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
    rays = map (\v -> Ray p v) viewVecs

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
        -> Intersection   -- ^ intersection point
        -> TracePrim      -- ^ primitive (entire scene) to use for occluding ambient light
        -> Float          -- ^ the visibility of the scene
shadeAO rng nRays (Intersection p n) (TracePrim trace) =
  let
    rays :: [ Ray ]
    rays = map nudge (sampleHemisphereUniformly rng nRays p n)

    nudge :: Ray -> Ray
    nudge (Ray pr vr) = Ray (offsetPointAlongVector vr 0.001 pr) vr

    intersections :: [ (Ray, Maybe Intersection) ]
    intersections = zip rays (map trace rays)

    raycontrib :: (Ray, Maybe Intersection) -> Float
    raycontrib ((Ray _ rayvector), Nothing) = abs (rayvector ⋅ (toVector3 n))
    raycontrib                            _ = 0

    rayhits :: [ Float ]
    rayhits = map raycontrib intersections

    --debugstr = "p = " ++ show p ++ "\nn = " ++ show n

    result :: Float
    result = (sum rayhits) / (fromIntegral nRays)

  in result --DT.trace debugstr result

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
  -> HasTransform TracePrim   -- ^ primitive (entire scene) to trace
  -> Raster                   -- ^ raster output
ambientOcclusionSceneTrace rng nRays color rasp xcamera xscene = raster
  where
    raster :: Raster
    raster = Raster width height pixels

    pixels = map shadeIntersection (zip rngs objRaysAndIntersections)

    shadeIntersection (pxrng, (ray,  Just i)) = mulColor (shadeAO pxrng nRays i obj) color
    shadeIntersection _ = black

    rngs = randomGenList rng

    objRaysAndIntersections :: [ (Ray, Maybe Intersection) ]
    objRaysAndIntersections = map faceforward (zip objRays objIntersections)

    faceforward :: (Ray, Maybe Intersection) -> (Ray, Maybe Intersection)
    faceforward (r, Nothing) = (r, Nothing)
    faceforward (r@(Ray _ v), Just (Intersection p n)) =
      let
        nvec = toVector3 n
        nvec' = if (v ⋅ nvec) < 0 then nvec else (nvec .* (-1.0))
        n' = toNormal3 nvec'
      in (r, Just (Intersection p n'))

    objIntersections :: [ Maybe Intersection ]
    objIntersections = map (tracePrimRay obj) objRays

    objRays :: [ Ray ]
    objRays = map (xform world2obj) worldRays
      -- let x = xform (xformCompose cam2world world2obj)
      -- in map x camRays

    worldRays :: [ Ray ]
    worldRays = map (xform cam2world) camRays

    camRays :: [ Ray ]
    camRays =
      let getRay = rayForRasterCoord cam rasp
      in map getRay rc

    rc :: [ RasterCoord ]
    rc = traverseRasterRowMajor width height

    world2obj :: XForm
    world2obj = xformInv obj2world

    RasterParams width height  = rasp
    HasTransform obj2world obj = xscene
    HasTransform cam2world cam = xcamera


-- | Test camera.
testCamera :: HasTransform Camera
testCamera = withTransform (translate 0 0 (-5.0)) $ Camera 45.0 0.01

-- | Test raster parameters.
testRasterParams :: RasterParams
testRasterParams = RasterParams 400 300

-- | Test rendering the scene.
testRender :: Float -> Raster
testRender angle = raster
  where
    testScene = withTransform (rotate angle (v3 0 0 1)) $ sphereTracePrim $ Sphere 1.0 300.0 (-0.7) (0.95)
    raster = dotShadeTrace white testRasterParams testCamera testScene

-- |Test rendering the scene using raytraced ambient occlusion.
testAORender :: RandomGen a
             => a
             -> Float
             -> Raster
testAORender rng angle = raster
  where
    testScene  = withTransform (rotate angle (v3 1 0 0)) $ sphereTracePrim $ Sphere 1.0 300.0 (-0.7) (0.95)
    nAOSamples = 2048
    raster     = ambientOcclusionSceneTrace rng nAOSamples white testRasterParams testCamera testScene

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
