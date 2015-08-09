-- |

module Main where

import Data.Monoid   ((<>))

import Camera        (perspectiveCamera)
import Chess         (pawn)
import Raster        (Raster, saveRasterPng, white)
import Sample        (simpleSample)
import Scene         (mkSceneObject, mkSimpleSceneObjectList)
import Shader        (constShader, dotShader)
import Shapes.Sphere (Sphere (Sphere), traceableSphere)
import VecMath       (rotate, translate, v3, xform, xformId)

main :: IO ()
main = saveRasterPng testSphere "test.png"

testSphere :: Raster
testSphere =
  let
    w      = 400
    h      = 300
    nx     = 4
    ny     = 4
    fovx   = 30
    hither = 0.1
    yon    = 1000
    aspect = ((fromIntegral w) / (fromIntegral h)) :: Float
    sph    = mkSceneObject (translate 0 1 0) Nothing $ traceableSphere $ Sphere 1 (-1) 1 360
    cam    = xform (translate 0 0 (-8)) $ perspectiveCamera xformId fovx hither yon aspect
    shader = dotShader white
  in simpleSample shader (w,h) (nx,ny) cam sph

testSpherePair :: Raster
testSpherePair =
  let w      = 400
      h      = 300
      nx     = 4
      ny     = 4
      fovx   = 30
      hither = 0.1
      yon    = 1000
      aspect = ((fromIntegral w) / (fromIntegral h)) :: Float
      sph1   = mkSceneObject xformId Nothing $ traceableSphere $ Sphere 1 (-1) 1 360
      sph2   = mkSceneObject (translate 0 0 2) Nothing $ traceableSphere $ Sphere 1 (-1) 1 360
      scene  = mkSimpleSceneObjectList xformId Nothing [sph1, sph2]
      camxf  = translate 0 0 (-8) <> rotate 90 (v3 1 0 0)
      cam    = perspectiveCamera camxf fovx hither yon aspect
      shader = dotShader white
  in simpleSample shader (w,h) (nx,ny) cam scene

testPawn :: Raster
testPawn =
  let
    w      = 400
    h      = 300
    nx     = 4
    ny     = 4
    fovx   = 30
    hither = 0.1
    yon    = 1000
    aspect = ((fromIntegral w) / (fromIntegral h)) :: Float
    camxf  = translate 0 (50) (-120) <> rotate (-120) (v3 1 0 0)
    cam    = perspectiveCamera camxf fovx hither yon aspect
    shader = dotShader white
  in simpleSample shader (w,h) (nx,ny) cam pawn
