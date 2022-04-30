module Hasklee.Attribute.Data where

import qualified Algebra.Graph as AG
import Control.DeepSeq (NFData)
import Control.Lens hiding (transform)
import Data.Binary (Binary)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Colour as Colour
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.SRGB.Linear
import Data.Default.Class
import Data.Hashable (Hashable)
import Data.List
import qualified Data.Map.Strict as Map
import Data.String
import qualified Data.Text as T
import GHC.Generics
import Linear

import Hasklee.Colour
import Hasklee.Component
import Hasklee.Mesh
import Hasklee.Transform (Trans, Transformable(transform),
                          Transform(Origin), HasLocalSpace(..))


class HasAttributes t a | t -> a where
  _attributes :: Traversal' t (Attribute a)

  attributes :: Traversal' t (Attribute a)
  attributes = _attributes

  --to remove
  _allAttributes :: Traversal' t (Attribute a)
  _allAttributes = attributes



type RealID = Int

data Attribute b = EmptyAtr
                 | ColourAtr (Colour b)
                 | AlphaColourAtr (AlphaColour b)
                 | SpecularAtr (Colour b)
                 | TransformAtr (Transform b)
                 | ColliderAtr (Mesh b)
                 | ColliderConvexAtr
                 | ColliderDisableAtr
                 | ComponentAtr [Component b]
                 | IgnoreRayCastAtr
                 | CsoundInline T.Text
                 | PathAtr T.Text [DanceStep b]
                 | PathRefAtr T.Text Int
                 | DanceAtr
                   { dName :: T.Text
                   , dSteps :: [DanceStep b]
                   , dOptions :: DanceOptions b
                   }
                 | DanceInstance
                   { dName :: T.Text
                   , dOptions :: DanceOptions b
                   }
                 | LightAtr LightType (LightOptions b)
                 | RealIDAtr RealID
                 | RealIDTAtr [RealID]
                 | NameAtr T.Text
                 | MaterialAtr (Material b)
                 | TagAtr T.Text
                 | ScalarAtr T.Text b
                 | CustomInt T.Text Int
                 | VectorAtr T.Text (V3 b)
                 | VectorAtr4 T.Text (V4 b)
                 | LuaCode T.Text T.Text
                 | LuaController

                 | ActionRR RealID Code
                 | ActionSS RealID Code

                 | StaticAtr
                 | IndexAtr ((Int,Int,Int),(Int,Int,Int))
                 | ListAtr [Attribute b]
                 | Param (Attribute b)
                 deriving (Binary, Eq, Generic, NFData, Show)

data Code = CodeString T.Text
          | CodeRef Int
          deriving (Binary, Eq, Generic, NFData, Show)

data Material a = Material
                  { _mShader :: T.Text
                  , _mTexture :: Maybe Texture
                  , _mOptions :: MaterialOptions a
                  }
                | MaterialRef Int
                | MaterialName T.Text
                deriving (Binary, Eq, Generic, Hashable, NFData, Show)

data Texture = TextureData B.ByteString
             | TextureRef Int
             | TexturePath T.Text
             deriving (Binary, Eq, Generic, Hashable, NFData, Show)

data DanceStep a = DanceStep (Transform a) a
                 deriving (Binary, Eq, Generic, NFData, Show)

instance Trans a => Transformable (DanceStep a) a where
  --local transform!!
  transform t1 (DanceStep t2 a) = DanceStep (t2 <> t1) a

data DanceOptions a = DanceOptions
                      { dRelative :: Bool
                      , dAutoPlay :: Bool
                      , dGoto :: a
                      , dLoops :: Int
                      , dLoopType :: LoopType
                      }
                    deriving (Binary, Eq, Generic, Hashable, NFData, Show)

instance Num a => Default (DanceOptions a) where
  def = DanceOptions False False 0 0 YoyoLoop

data LoopType = RestartLoop | YoyoLoop | IncrementalLoop |
                FlipFlopLoop | StartStopLoop
               deriving (Binary, Enum, Eq, Generic, Hashable, NFData, Show)

data LightType = SpotLight | PointLight | DirectionalLight | MeshLight
               deriving (Binary, Enum, Eq, Generic, NFData, Show)

data LightOptions a = LightOptions
                      { lRange :: a
                      , lIntensity :: a
                      , lAngle :: a
                      , lColor :: Colour a
                      }
                    deriving (Binary, Eq, Generic, NFData, Show)

instance Fractional a => Default (LightOptions a) where
  def = LightOptions 1 1 60 (rgb 1 1 1)

data MaterialOptions a = MaterialOptions
                         { mColor :: AlphaColour a
                         , mSmoothness :: a
                         , mMetallic :: a
                         }
                       deriving (Binary, Eq, Generic, Hashable, NFData, Show)

instance Fractional a => Default (MaterialOptions a) where
  def = MaterialOptions (_alphaColour $ V4 1 1 1 1) 0.3 0.3

instance Eq b => Ord (Attribute b) where
  compare RealIDAtr{} _ = LT
  compare _ RealIDAtr{} = GT
  compare _ _ = EQ

instance Semigroup (Attribute a) where
  (<>) = mappend

instance Monoid (Attribute b) where
  mempty = ListAtr []
  mappend (ListAtr a) (ListAtr b) = ListAtr (a ++ b)
  mappend a@(ListAtr _) b = mappend a (ListAtr [b])
  mappend a b@(ListAtr _) = mappend (ListAtr [a]) b
  mappend a b = mappend (ListAtr [a]) b

instance HasAttributes (Attribute a) a where
  _attributes = id
  attributes = _attributes.each

instance Each (Attribute a) (Attribute b) (Attribute a) (Attribute b) where
  each f (ListAtr as) = ListAtr <$> traverse f as
  each f a = f a

instance HasLocalSpace (Attribute a) a where
  _localSpace g (TransformAtr t) = TransformAtr <$> g t
  _localSpace g (ListAtr as) = maybe
    ((\x -> ListAtr (TransformAtr x:as)) <$> g Origin )
    (\i -> (\x -> ListAtr $ (ix i .~  x) as) <$> _localSpace g (as !! i) )
    (findIndex (\case { TransformAtr{} -> True; _ -> False }) as)
  _localSpace g a = _localSpace g (ListAtr [a])

instance Trans a => Transformable (Attribute a) a where
  transform t (ListAtr ls) = ListAtr (transform t <$> ls)
  transform t (PathAtr s ds) = PathAtr s (transform t <$> ds)
  transform t (DanceAtr s ds b) = DanceAtr s (transform t <$> ds) b
  transform t (ColliderAtr m) = ColliderAtr (transform t m)
  transform _ a = a

instance HasAttributes (Transform a) a where
  _attributes g a = (\(TransformAtr b) -> b) <$> g (TransformAtr a)

instance HasAttributes (Maybe (Attribute b)) b where
  _attributes _ Nothing = pure Nothing
  _attributes g (Just a) = Just <$> _attributes g a


makePrisms ''Attribute
makePrisms ''Material
makeLenses ''Material
makeClassy_ ''LightOptions
