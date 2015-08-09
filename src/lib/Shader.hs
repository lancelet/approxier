{-|

-}
module Shader (
    constShader
  , dotShader
  ) where

import Raster  (Color, mulColor)
import Scene   (SimpleShader (SimpleShader, shadeSurface))
import Trace   (isectN, isectRay, rayVector)
import VecMath (normalize, toVector3, (⋅))

constShader :: Color -> SimpleShader
constShader c =
  let shade _ _ _ = c
  in SimpleShader { shadeSurface = shade }

dotShader :: Color -> SimpleShader
dotShader c =
  let shade _ _ isect =
        let vn = (normalize . rayVector . isectRay) isect
            sn = (normalize . toVector3 . isectN)   isect
            dp = abs $ vn ⋅ sn
        in mulColor dp c
  in SimpleShader { shadeSurface = shade }


