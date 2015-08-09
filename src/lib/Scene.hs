{-|

-}
module Scene (
    SimpleShader (
        SimpleShader
      , shadeSurface
    )
  , SceneObject (
        SceneObject
      , soSetShader
      , soTransform
      , soTrace
      , soTraceHit
      , soShade
    )
  , mkSceneObject
  , mkSimpleSceneObjectList
  ) where

import Data.List (sortOn) -- TODO: since 4.8

import Data.Foldable (find)

import Data.Maybe (catMaybes)

import Raster (Color)

import Trace (Intersection, Traceable, Ray, trace, traceHit, isectRayParam)

import VecMath (XForm, Transform, xform, xformCompose, xformInv, inSpace, inSpace')

-- | Simple shader (still in development).
data SimpleShader = SimpleShader
  {
    -- | Shade a point on the surface.
    --
    --   Shaders can trace rays into the scene (using the passed SceneObject). The shader operates
    --   in object space, but is passed the object-to-world transformation so that it's possible to
    --   trace rays in world space. Most of the local illumination data required by the shader is
    --   stored in the 'Intersection' object.
    shadeSurface :: SceneObject    -- ^ the entire scene (for tracing additional rays)
                 -> XForm          -- ^ current object-to-world transformation
                 -> Intersection   -- ^ intersection point (object space!)
                 -> Color          -- ^ resultant color of the surface
  }

-- | Object in a scene.
data SceneObject = SceneObject {
    -- | Sets the shader of the object.
    soSetShader :: Maybe SimpleShader -> SceneObject

    -- | Transforms the object (concatenates the transformation).
  , soTransform :: XForm -> SceneObject

    -- | Traces a (world) ray against the object.
  , soTrace     :: Ray -> Maybe Intersection

    -- | Tests for a hit of a (world) ray against the object.
  , soTraceHit  :: Ray -> Bool

    -- | Shades the object using a world ray (if the object is hit, returns the object color).
  , soShade     :: SimpleShader -> SceneObject -> Ray -> Maybe Color
  }

instance Transform SceneObject where xform = flip soTransform

----------------------------------------------------------------------------------------------------
-- SINGLE PRIMITIVE

-- | Makes a scene object from a traceable thing, a transformation and a shader.
mkSceneObject :: XForm
              -> Maybe SimpleShader
              -> Traceable
              -> SceneObject
mkSceneObject x s t = SceneObject {
    soSetShader = (\s' -> mkSceneObject x s' t)
  , soTransform = (\x' -> mkSceneObject (xformCompose x x') s t)
  , soTrace     = inSpace  (xformInv x) (trace t)
  , soTraceHit  = inSpace' (xformInv x) (traceHit t)
  , soShade     = shadeTrace x s t
  }

-- | Shades a traceable scene primitive.
shadeTrace :: XForm
           -> Maybe SimpleShader
           -> Traceable
           -> SimpleShader
           -> SceneObject
           -> Ray
           -> Maybe Color
shadeTrace x ms t ss scene ray =
  let shader = maybe ss id ms
      shadeFn = shadeSurface shader
      traceFn = inSpace (xformInv x) (trace t)
  in fmap (shadeFn scene x) (traceFn ray)

----------------------------------------------------------------------------------------------------
-- COLLECTION OF PRIMITIVES

mkSimpleSceneObjectList :: XForm
                        -> Maybe SimpleShader
                        -> [SceneObject]
                        -> SceneObject
mkSimpleSceneObjectList x s ss = SceneObject {
    soSetShader = (\s' -> mkSimpleSceneObjectList x s' ss)
  , soTransform = (\x' -> mkSimpleSceneObjectList x' s ss)
  , soTrace     = simpleObjListTrace x ss
  , soTraceHit  = simpleObjListTraceHit x ss
  , soShade     = simpleObjListShadeTrace x s ss
  }

simpleObjListTrace :: XForm -> [SceneObject] -> Ray -> Maybe Intersection
simpleObjListTrace x ss r =
  let objray :: Ray
      objray = xform (xformInv x) r
    
      isects :: [Intersection]
      isects = sortOn isectRayParam $ catMaybes $ fmap (\s -> soTrace s objray) ss
  in case isects of
    (i:_) -> Just i
    _     -> Nothing

simpleObjListTraceHit :: XForm -> [SceneObject] -> Ray -> Bool
simpleObjListTraceHit x ss r =
  let objray = xform (xformInv x) r
      isects = find id $ fmap (\s -> soTraceHit s objray) ss
  in case isects of
    Just _  -> True
    Nothing -> False

simpleObjListShadeTrace :: XForm
                        -> Maybe SimpleShader
                        -> [SceneObject]
                        -> SimpleShader
                        -> SceneObject
                        -> Ray
                        -> Maybe Color
simpleObjListShadeTrace x ms ss sh scene ray =
  let shader  = maybe sh id ms
      shadeFn = shadeSurface shader
      traceFn = simpleObjListTrace x ss
  in fmap (shadeFn scene x) (traceFn ray)
