module Hasklee.Colour
  ( module Data.Colour.Names
  , ToColour(..)
  , ToAlphaColour(..)
  , alphaToColour
  ) where

import Control.DeepSeq
import Control.Lens hiding (transform)
import Data.Binary
-- import Data.Colour as Colour
import Data.Colour
import Data.Colour.Names
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.SRGB.Linear
import Data.Hashable
import Linear

import Hasklee.Transform
-- import qualified Hasklee as Data


class ToColour t a | t -> a where
  _colour :: t -> Colour a

class ToAlphaColour t a | t -> a where
  _alphaColour :: t -> AlphaColour a

instance ToColour (Colour a) a where
  _colour = id

instance Fractional a => ToColour (RGB a) a where
  _colour = uncurryRGB rgb

instance Fractional a => ToColour (V3 a) a where
  _colour (V3 r g b) = rgb r g b

instance ToAlphaColour (AlphaColour a) a where
  _alphaColour = id

instance Fractional a => ToAlphaColour (V4 a) a where
  _alphaColour (V4 r g b a) = withOpacity (rgb r g b) a


-- ORHPANS

instance (Fractional a, NFData a) => NFData (Colour a) where
  rnf (toRGB -> RGB r g b) = rnf r `seq` rnf g `seq` rnf b

instance (Floating a, Ord a, NFData a) => NFData (AlphaColour a) where
  rnf x = rnf (alphaToColour x) `seq` rnf (alphaChannel x)

instance (Binary a, Fractional a) => Binary (Colour a) where
  put (toRGB -> RGB r g b) = put r >> put g >> put b
  get = rgb <$> get <*> get <*> get

instance (Binary a, Floating a, Ord a) => Binary (AlphaColour a) where
  put x = put (alphaToColour x) >> put (alphaChannel x)
  get = withOpacity <$> (rgb <$> get <*> get <*> get) <*> get

instance (Hashable a, Fractional a) => Hashable (Colour a) where
  hashWithSalt s (toRGB -> RGB r g b)= s `hashWithSalt` r `hashWithSalt` g `hashWithSalt` b

instance (Hashable a, Floating a, Ord a) => Hashable (AlphaColour a) where
  hashWithSalt s c = s `hashWithSalt` alphaToColour c `hashWithSalt` alphaChannel c

instance (Floating a, Ord a) => V4Iso AlphaColour a where
  v4 = iso (\c -> (\(RGB r g b, a) -> V4 r g b a) (toRGB . alphaToColour $ c, alphaChannel c)) _alphaColour

instance (Floating a, Ord a) => V3Iso Colour a where
  v3 = iso ((\(RGB r g b) -> V3 r g b) . toRGB) _colour


-- lifted from Diagrams
alphaToColour :: (Floating a, Ord a) => AlphaColour a -> Colour a
alphaToColour ac | alphaChannel ac == 0 = ac `Data.Colour.over` black
                 | otherwise = darken (recip (alphaChannel ac)) (ac `Data.Colour.over` black)
