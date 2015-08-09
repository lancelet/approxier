{-|
Module      : Spectrum
Description : Manipulating spectra
Copyright   : (c) Jonathan Merritt, 2015
License     : Apache License 2.0
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental

Spectra representing color and intensity.
-}
module Spectrum (
    Spectrum (
        spectrumAdd
      , spectrumMul
      , spectrumToRGB
      , spectrumFromRGB
    )
  , RGBSpectrum(
        RGBSpectrum
    )
  ) where

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke, peekElemOff, pokeElemOff)

----------------------------------------------------------------------------------------------------
-- SPECTRUM DEFINITION

class Spectrum a where
  spectrumAdd          :: a -> a -> a
  spectrumMul          :: Float -> a -> a
  spectrumToRGB        :: a -> RGBSpectrum
  spectrumFromRGB      :: RGBSpectrum -> a

----------------------------------------------------------------------------------------------------
-- TRINARY / RGB SPECTRUM

data RGBSpectrum = RGBSpectrum {-# UNPACK #-}
                   !Float -- ^ red
                   !Float -- ^ green
                   !Float -- ^ blue
                 deriving (Show)

instance Spectrum RGBSpectrum where
  spectrumAdd     = addRGBSpectra
  spectrumMul     = mulRGBSpectra
  spectrumToRGB   = id
  spectrumFromRGB = id

instance Storable RGBSpectrum where
  sizeOf    _ = sizeOf (undefined :: Float) * 3
  alignment _ = alignment (undefined :: Float)

  {-# INLINE peek #-}
  peek p = do
    r <- peekElemOff q 0
    g <- peekElemOff q 1
    b <- peekElemOff q 2
    return (RGBSpectrum r g b)
    where
      q = castPtr p

  {-# INLINE poke #-}
  poke p (RGBSpectrum r g b) = do
    pokeElemOff q 0 r
    pokeElemOff q 1 g
    pokeElemOff q 2 b
    where
      q = castPtr p

addRGBSpectra :: RGBSpectrum -> RGBSpectrum -> RGBSpectrum
addRGBSpectra (RGBSpectrum r1 g1 b1) (RGBSpectrum r2 g2 b2) = RGBSpectrum (r1+r2) (g1+g2) (b1+b2)

mulRGBSpectra :: Float -> RGBSpectrum -> RGBSpectrum
mulRGBSpectra c (RGBSpectrum r g b) = RGBSpectrum (c*r) (c*g) (c*b)
