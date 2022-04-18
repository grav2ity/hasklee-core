module Hasklee.Vertex where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Binary (Binary)
import Data.Default.Class
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Linear


data Vertex a = Vertex3 !(V3 a)
              | Vertex9 !(V3 a) !(V3 a) !(V3 a)
              | Vertex18 !(V3 a) !(V3 a) !(V3 a) !(V3 a) !(V2 a) !(V4 a)
              deriving (Binary, Eq, Generic, Hashable, NFData, Show)


class HasVertices t a | t -> a where
  vertices :: Traversal' t (Vertex a)

class ToVertices t a | t -> a where
  type VertexOptions t :: *
  type instance (VertexOptions t) = ()

  toVertices :: Default (VertexOptions t) => t -> [Vertex a]
  toVertices = toVerticesI def

  toVerticesI :: VertexOptions t -> t -> [Vertex a]
  default toVerticesI :: VertexOptions t ~ () => VertexOptions t -> t -> [Vertex a]
  toVerticesI _ = toVertices

class HasPosition t a | t -> a where
  _pos :: Lens' t (V3 a)

class HasNormal t a | t -> a where
  _n :: Lens' t (V3 a)

class HasTangent t a | t -> a where
  _tn :: Lens' t (V3 a)

class HasBitangent t a | t -> a where
  _btn :: Lens' t (V3 a)

class HasUV t a | t -> a where
  _uv :: Lens' t (V2 a)

class HasColor t a | t -> a where
  _color :: Lens' t (V4 a)

class HasTBN t a | t -> a where
  _TBN :: Lens' t (M33 a)


instance HasVertices (Vertex a) a where
  vertices = id

instance HasVertices (t a) a => HasVertices [t a] a where
  vertices = traversed.vertices

instance HasVertices (V3 a) a where
  vertices g v = view _pos <$> g (Vertex3 v)

instance ToVertices (Vertex a) a where
  toVerticesI _ = pure

instance ToVertices (t a) a => ToVertices [t a] a where
  type instance (VertexOptions [t a]) = VertexOptions (t a)
  toVerticesI m = foldMap (toVerticesI m)

instance ToVertices (V3 a) a where
  toVerticesI _  = pure . vertex

instance HasPosition (Vertex a) a where
  _pos g (Vertex3 v) = Vertex3 <$> g v
  _pos g (Vertex9 v n tn) = (\v' -> Vertex9 v' n tn) <$> g v
  _pos g (Vertex18 v n tn btn uv col) =
    (\v' -> Vertex18 v' n tn btn uv col) <$> g v

instance HasPosition (V3 a) a where
  _pos = id

instance Num a => HasUV (Vertex a) a where
  _uv g (Vertex3 v) =
    (\uv' -> Vertex18 v zero zero zero uv' (pure 1)) <$> g zero
  _uv g (Vertex9 v n tn) =
    (\uv' -> Vertex18 v n tn (cross n tn) uv' (pure 1)) <$> g zero
  _uv g (Vertex18 v n tn btn uv col) =
    (\uv' -> Vertex18 v n tn btn uv' col) <$> g uv

instance Num a => HasNormal (Vertex a) a where
  _n g (Vertex3 v) =
    (\n' -> Vertex18 v n' zero zero zero (pure 1)) <$> g zero
  _n g (Vertex9 v n tn) =
    (\n' -> Vertex9 v n' tn) <$> g n
  _n g (Vertex18 v n tn btn uv col) =
    (\n' -> Vertex18 v n' tn btn uv col) <$> g n

instance Num a => HasTangent (Vertex a) a where
  _tn g (Vertex3 v) =
    (\tn' -> Vertex18 v zero tn' zero zero (pure 1)) <$> g zero
  _tn g (Vertex9 v n tn) =
    Vertex9 v n <$> g tn
  _tn g (Vertex18 v n tn btn uv col) =
    (\tn' -> Vertex18 v n tn' btn uv col) <$> g tn

instance Num a => HasBitangent (Vertex a) a where
  _btn g (Vertex3 v) =
    (\btn' -> Vertex18 v zero zero btn' zero (pure 1)) <$> g zero
  _btn g (Vertex9 v n tn) =
    (\btn' -> Vertex18 v n tn btn' zero (pure 1)) <$> g (cross n tn)
  _btn g (Vertex18 v n tn btn uv col) =
    (\btn' -> Vertex18 v n tn btn' uv col) <$> g btn

instance (Num a) => HasColor (Vertex a) a where
  _color g (Vertex3 v) = Vertex18 v zero zero zero zero <$> g (pure 1)
  _color g (Vertex9 v n tn) = Vertex18 v n tn (cross n tn) zero <$> g (pure 1)
  _color g (Vertex18 v n tn btn uv col) = Vertex18 v n tn btn uv <$> g col

instance Num a => HasTBN (Vertex a) a where
  _TBN g (Vertex3 v) =
    (\m -> let (V3 tn' btn' n') = transpose m
           in Vertex18 v n' tn' btn' zero (pure 1))
    <$> g identity
  _TBN g (Vertex9 v n tn) =
    (\m -> let (V3 tn' btn' n') = transpose m
           in Vertex18 v n' tn' btn' zero (pure 1))
    <$> g (transpose (V3 tn (cross n tn) n))
  _TBN g (Vertex18 v n tn btn uv col) =
    (\m -> let (V3 tn' btn' n') = transpose m
           in Vertex18 v n' tn' btn' uv col)
    <$> g (transpose (V3 tn btn n))

instance Functor Vertex where
  fmap f (Vertex3 v) = Vertex3 (fmap f v)
  fmap f (Vertex9 v n tn) = Vertex9 (fmap f v) (fmap f n) (fmap f tn)
  fmap f (Vertex18 v n tn btn uv col) = Vertex18 (fmap f v) (fmap f n)
    (fmap f tn) (fmap f btn) (fmap f uv) (fmap f col)

instance Applicative Vertex where
  pure a = Vertex18 (pure a) (pure a) (pure a) (pure a) (pure a) (pure a)
  Vertex3 v1 <*> Vertex3 v2 = Vertex3 (v1 <*> v2)
  Vertex3 v1 <*> Vertex9 v2 _ _ = Vertex3 (v1 <*> v2)
  Vertex3 v1 <*> Vertex18 v2 _ _ _ _ _ = Vertex3 (v1 <*> v2)
  Vertex9 v1 _ _ <*> Vertex3 v2  = Vertex3 (v1 <*> v2)
  Vertex9 v1 n1 tn1 <*> Vertex9 v2 n2 tn2 =
    Vertex9 (v1 <*> v2) (n1 <*> n2) (tn1 <*> tn2)
  Vertex9 v1 n1 tn1 <*> Vertex18 v2 n2 tn2 _ _ _ =
    Vertex9 (v1 <*> v2) (n1 <*> n2) (tn1 <*> tn2)
  Vertex18 v1 _ _ _ _ _ <*> Vertex3 v2 = Vertex3 (v1 <*> v2)
  Vertex18 v1 n1 tn1 _ _ _ <*> Vertex9 v2 n2 tn2 =
    Vertex9 (v1 <*> v2) (n1 <*> n2) (tn1 <*> tn2)
  Vertex18 v1 n1 tn1 btn1 uv1 col1 <*> Vertex18 v2 n2 tn2 btn2 uv2 col2 =
    Vertex18 (v1 <*> v2) (n1 <*> n2) (tn1 <*> tn2)
    (btn1 <*> btn2) (uv1 <*> uv2) (col1 <*> col2)

instance Additive Vertex where
  zero = pure 0
  liftI2 = liftA2
  liftU2 f (Vertex3 v1) (Vertex3 v2) = Vertex3 (f <$> v1 <*> v2)
  liftU2 f (Vertex3 v1) (Vertex9 v2 n2 tn2) = Vertex9 (f <$> v1 <*> v2) n2 tn2
  liftU2 f (Vertex3 v1) (Vertex18 v2 n2 tn2 btn2 uv2 col2) =
    Vertex18 (f <$> v1 <*> v2) n2 tn2 btn2 uv2 col2
  liftU2 f (Vertex9 v1 n1 tn1) (Vertex3 v2) = Vertex9 (f <$> v1 <*> v2) n1 tn1
  liftU2 f (Vertex9 v1 n1 tn1) (Vertex9 v2 n2 tn2) =
    Vertex9 (f <$> v1 <*> v2) (f <$> n1 <*> n2) (f <$> tn1 <*> tn2)
  liftU2 f (Vertex9 v1 n1 tn1) (Vertex18 v2 n2 tn2 btn2 uv2 col2) =
    Vertex18 (f <$> v1 <*> v2) (f <$> n1 <*> n2)
    (f <$> tn1 <*> tn2) btn2 uv2 col2
  liftU2 f (Vertex18 v1 n1 tn1 btn1 uv1 col1) (Vertex3 v2) =
    Vertex18 (f <$> v1 <*> v2) n1 tn1 btn1 uv1 col1
  liftU2 f (Vertex18 v1 n1 tn1 btn1 uv1 col1) (Vertex9 v2 n2 tn2) =
    Vertex18 (f <$> v1 <*> v2) (f <$> n1 <*> n2)
    (f <$> tn1 <*> tn2) btn1 uv1 col1
  liftU2 f (Vertex18 v1 n1 tn1 btn1 uv1 col1)
           (Vertex18 v2 n2 tn2 btn2 uv2 col2) =
    Vertex18 (f <$> v1 <*> v2) (f <$> n1 <*> n2) (f <$> tn1 <*> tn2)
    (f <$> btn1 <*> btn2) (f <$> uv1 <*> uv2) (f <$> col1 <*> col2)

instance Metric Vertex where
  dot a b = dot (a ^. _pos) (b ^. _pos)

instance R1 Vertex where
  _x = _pos._x

instance R2 Vertex where
  _y = _pos._y
  _xy = _pos._xy

instance R3 Vertex where
  _z = _pos._z
  _xyz = _pos

-- ??
instance Ord a => Ord (Vertex a) where
  (<=) a b = (a ^. _pos) <= (b ^. _pos)

instance Num a => Num (Vertex a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Vertex a) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance HasNormal (M33 a) a where
  _n = column _z

instance HasTangent (M33 a) a where
  _tn = column _x

instance HasBitangent (M33 a) a  where
  _btn = column _y

instance HasNormal (M44 a) a where
  _n = _m33 . column _z

instance HasTangent (M44 a) a where
  _tn = _m33 . column _x

instance HasBitangent (M44 a) a  where
  _btn = _m33 . column _y

instance HasPosition (M44 a) a  where
  _pos = column _w . _xyz


normal :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a
normal v1 v2 = normalize $ cross v1 v2

flipNormal :: Num a => Vertex a -> Vertex a
flipNormal = over _n negated

flipTangent :: Num a => Vertex a -> Vertex a
flipTangent = over _tn negated

transformVertex :: Num a => M44 a -> M33 a -> Vertex a -> Vertex a
transformVertex m _ (Vertex3 v) = Vertex3 . view _xyz . (m !*) . point $ v
transformVertex m mti (Vertex9 v n tn) = Vertex9 v' n' tn'
  where v' = (view _xyz . (m !*) . point) v
        n' = mti !* n
        tn' = mti !* tn
transformVertex m mti (Vertex18 v n tn btn uv col) =
  Vertex18 v' n' tn' btn' uv col
  where v' = (view _xyz . (m !*) . point) v
        n' = mti !* n
        tn' = mti !* tn
        btn' = mti !* btn

vertex :: V3 a -> Vertex a
vertex = Vertex3
