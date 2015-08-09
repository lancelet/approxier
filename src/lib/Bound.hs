{-|

-}
module Bound (
    BoundingBoxed(
        BoundingBoxed
      , objectSpaceBoundingBox
      , worldSpaceBoundingBox
    )
  , mkBoundingBoxed
  , BoundingBox(BoundingBox)
  , bboxUnion
  , bboxCorners
  , boundPts
  ) where

import VecMath (Point3, Transform, XForm, p3, xcomp3, xform, ycomp3, zcomp3)

-- TODO : Be careful of "thin" bounding boxes, where width, height, depth may be zero.

----------------------------------------------------------------------------------------------------
-- SOMETHING BOUNDED BY A BOUNDING BOX

-- | Bounding box calculations.
data BoundingBoxed = BoundingBoxed {
    -- | Returns the bounding box in object space.
    objectSpaceBoundingBox :: BoundingBox

    -- | Returns the bounding box in world space.
  , worldSpaceBoundingBox :: XForm        -- ^ object to world transformation
                          -> BoundingBox  -- ^ bounding box
  }

-- | Default implementation of a worldSpaceBoundingBox function, which just transforms the corners
--   of the object space bounding box.
defaultWorldSpaceBoundingBox :: BoundingBox -> XForm -> BoundingBox
defaultWorldSpaceBoundingBox = flip xform

-- | Creates a 'BoundingBoxed' from an object-space bounding box alone.
mkBoundingBoxed :: BoundingBox -> BoundingBoxed
mkBoundingBoxed b = BoundingBoxed {
    objectSpaceBoundingBox = b
  , worldSpaceBoundingBox  = defaultWorldSpaceBoundingBox b                       
  }

----------------------------------------------------------------------------------------------------
-- BOUNDING BOXES

-- |Bounding box for primitives.
data BoundingBox = BoundingBox
                   !Float  -- ^ min X
                   !Float  -- ^ min Y
                   !Float  -- ^ min Z
                   !Float  -- ^ max X
                   !Float  -- ^ max Y
                   !Float  -- ^ max Z
                   deriving (Show)

-- |Takes the union of two bounding boxes.
bboxUnion :: BoundingBox -> BoundingBox -> BoundingBox
bboxUnion a b =
  let
    BoundingBox aXmin aYmin aZmin aXmax aYmax aZmax = a
    BoundingBox bXmin bYmin bZmin bXmax bYmax bZmax = b
    xmin = min aXmin bXmin
    ymin = min aYmin bYmin
    zmin = min aZmin bZmin
    xmax = max aXmax bXmax
    ymax = max aYmax bYmax
    zmax = max aZmax bZmax
  in BoundingBox xmin ymin zmin xmax ymax zmax

-- |Returns the 8 corners of a bounding box.
bboxCorners :: BoundingBox -> [ Point3 ]
bboxCorners b =
  let
    BoundingBox xmin ymin zmin xmax ymax zmax = b
    pt1 = p3 xmin ymin zmin
    pt2 = p3 xmin ymax zmin
    pt3 = p3 xmax ymin zmin
    pt4 = p3 xmax ymax zmin
    pt5 = p3 xmin ymin zmax
    pt6 = p3 xmin ymax zmax
    pt7 = p3 xmax ymin zmax
    pt8 = p3 xmax ymax zmax
  in [ pt1, pt2, pt3, pt4, pt5, pt6, pt7, pt8 ]

-- |Returns a bounding box that precisely bounds a list of points.
boundPts :: [ Point3 ] -> BoundingBox
boundPts pts =
  let
    xs   = map xcomp3 pts
    ys   = map ycomp3 pts
    zs   = map zcomp3 pts
    minx = minimum xs
    miny = minimum ys
    minz = minimum zs
    maxx = maximum xs
    maxy = maximum ys
    maxz = maximum zs
  in BoundingBox minx miny minz maxx maxy maxz

-- |Bounding boxes are transformable by transforming their corner points.
instance Transform BoundingBox where
  xform x = boundPts . (map (xform x)) . bboxCorners

