{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Hasklee.Mesh where

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Binary (Binary)
import Data.Default.Class
import Data.Foldable
import Data.Hashable
import Data.List hiding (transpose)
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Linear

import Hasklee.Vertex


data Mesh a = EmptyMesh
            | Triangle !(Vertex a) !(Vertex a) !(Vertex a)
            | Quad !(Vertex a) !(Vertex a) !(Vertex a) !(Vertex a)
            | TriangleFan (Vertex a) [Vertex a]
            | TriangleSeq (Seq (Vertex a)) (Seq Indices)
            | FlipSided (Mesh a)
            | DoubleSided (Mesh a)
            | Closed (Mesh a)
            | TaggedMesh String (Mesh a)
            | MeshSeq (Seq (Mesh a))
            deriving (Binary, Eq, Generic, NFData, Ord, Show)

data Indices = Indices Int Int Int
             deriving (Binary, Eq, Generic, NFData, Ord, Show)

data Edge a = Edge a a
            deriving (Foldable, Functor, Show)

makePrisms ''Mesh


class HasMesh t a | t -> a where
  _mesh :: Lens' t (Mesh a)

class ToMesh t a | t -> a where
  type MeshOptions t :: *
  type instance (MeshOptions t) = ()

  toMesh :: Default (MeshOptions t) => t -> Mesh a
  toMesh = toMeshI def

  toMeshI :: MeshOptions t -> t -> Mesh a
  default toMeshI :: MeshOptions t ~ () => MeshOptions t -> t -> Mesh a
  toMeshI _ = toMesh

class HasFaces t a | t -> a where
  faces :: Traversal' t (Mesh a)

class HasEdges t a | t -> a where
  edges :: Traversal' t (Edge (Vertex a))

class HasCenter t a | t -> a where
  _center :: Lens' t (V3 a)

class FaceSpace t a | t -> a where
  faceSpace :: t -> M33 a

class TwoFaced a where
  flipFace :: a -> a

class Tagged s t a b | s -> a, t -> b, s b -> t, t a -> s where
  tagged :: String -> Traversal s t a b


instance Semigroup (Mesh a) where
  (<>) = mappend

instance Monoid (Mesh a) where
  mempty = EmptyMesh
  mappend EmptyMesh b = b
  mappend a EmptyMesh = a
  mappend (MeshSeq a) (MeshSeq b) = MeshSeq (a >< b)
  mappend (MeshSeq a) b = MeshSeq (a |> b)
  mappend a (MeshSeq b) = MeshSeq (a <| b)
  mappend a b = MeshSeq $ a <| b <| Seq.empty

instance Num a => Each (Mesh a) (Mesh a) (Mesh a) (Mesh a) where
  each = faces

instance Functor Mesh where
  fmap _ EmptyMesh = EmptyMesh
  fmap f (Triangle a b c) = Triangle (fmap f a) (fmap f b) (fmap f c)
  fmap f (Quad a b c d) = Quad (fmap f a) (fmap f b) (fmap f c) (fmap f d)
  fmap f (TriangleFan c ls) = TriangleFan (fmap f c) (fmap f <$> ls)
  fmap f (TriangleSeq vs is) = TriangleSeq (fmap f <$> vs) is
  fmap f (FlipSided m) = FlipSided (fmap f m)
  fmap f (DoubleSided m) = DoubleSided (fmap f m)
  fmap f (Closed m) = Closed (fmap f m)
  fmap f (TaggedMesh s m) = TaggedMesh s (fmap f m)
  fmap f (MeshSeq a) = MeshSeq (fmap f <$> a)

instance HasMesh (Mesh a) a where
  _mesh = id

instance ToMesh (Mesh a) a where
  toMesh = id

instance Num a => HasFaces (Mesh a) a where
  faces g EmptyMesh = g EmptyMesh
  faces g m@Triangle{} = g m
  faces g m@Quad{} = g m
  faces g (TriangleFan c ls) = faces g ms
    where
      ms = if length ls > 1
           then meshSeq $ zipWith (\a b -> Triangle a b c) (init ls) (tail ls)
           else EmptyMesh
  faces g (TriangleSeq vs is) = MeshSeq <$> traverse (g . f) is
    where f (Indices a b c) = Triangle (Seq.index vs a)
                              (Seq.index vs b) (Seq.index vs c)
  faces g (FlipSided m) = faces g (reverseMesh m)
  faces g (DoubleSided m) = faces g (m <> reverseMesh m)
  faces g (Closed EmptyMesh) = g EmptyMesh
  faces g (Closed m) = (faces.(\g' x -> g' (closedMe x))) g m
  faces g (TaggedMesh s m) = (faces.(\g' x -> g' (TaggedMesh s x))) g m
  faces g (MeshSeq m) = MeshSeq <$> traverse (faces g) m

instance Num a => HasVertices (Mesh a) a where
  vertices _ EmptyMesh = pure EmptyMesh
  vertices g (Triangle a b c) = Triangle <$> g a <*> g b <*> g c
  vertices g (Quad a b c d) = Quad <$> g a <*> g b <*> g c <*> g d
  vertices g (TriangleFan c ls) = TriangleFan <$> g c <*> traverse g ls
  vertices g (TriangleSeq vs is) = MeshSeq <$> traverse (vertices g) (f <$> is)
    where f (Indices a b c) = Triangle (Seq.index vs a)
                              (Seq.index vs b) (Seq.index vs c)
  vertices g (FlipSided m) = vertices g (reverseMesh m)
  vertices g (DoubleSided m) = vertices g (m <> reverseMesh m)
  vertices g (Closed (Triangle a b c)) =
    Closed <$> (Triangle <$> g a <*> g b <*> g c <* g a)
  vertices g (Closed (Quad a b c d)) =
    Closed <$> (Quad <$> g a <*> g b <*> g c <*> g d <* g a)
  vertices g (Closed m) = (faces.vertices) g (Closed m)
  vertices g (TaggedMesh s m) = TaggedMesh s <$> vertices g m
  vertices g (MeshSeq m) = MeshSeq <$> traverse (vertices g) m

instance Num a => ToVertices (Mesh a) a where
  toVerticesI _ = toListOf vertices

instance Num a => HasEdges (Mesh a) a where
  edges g (Triangle a b c) = (\(Edge a' b') (Edge _ c') _ -> Triangle a' b' c')
    <$> g (Edge a b) <*> g (Edge b c) <*> g (Edge c a)
  edges g (Quad a b c d) =
    (\(Edge a' b') (Edge _ c') (Edge _ d') _ -> Quad a' b' c' d' )
    <$> g (Edge a b) <*> g (Edge b c) <*> g (Edge c d) <*> g (Edge d a)
  edges g (Closed m) = Closed <$> edges g m
  edges g (TaggedMesh s m) = TaggedMesh s <$> edges g m
  edges g m = (faces.edges) g m

instance (Fractional a, Ord a) => HasCenter (Mesh a) a where
  _center g m = meanOf (pureMe.vertices._pos) g m

instance (Fractional a, Ord a) => HasCenter [Mesh a] a where
  _center = meanOf (traversed._center)

instance (Fractional a, Ord a) => HasPosition (Mesh a) a where
  _pos = _center._pos

instance (Epsilon a, Floating a) => FaceSpace (Mesh a) a where
  faceSpace EmptyMesh = identity
  faceSpace (Triangle a b c) = triangleSpace a b c
  faceSpace (Quad a b _ d) = triangleSpace a b d
  faceSpace (Closed m) = faceSpace m
  faceSpace (TaggedMesh _ m) = faceSpace m
  faceSpace m = faceSpace (m ^. ix 0)

type instance Index (Mesh a) = Int
type instance IxValue (Mesh a) = Mesh a

instance Num a => Ixed (Mesh a) where
  ix = elementOf faces

instance Num a => Field1 (Mesh a) (Mesh a) (Vertex a) (Vertex a) where
  _1 g (Triangle a b c) = g a <&> (\x -> Triangle x b c)
  _1 g (Quad a b c d) = g a <&> (\x -> Quad x b c d)
  _1 g m = (singular (ix 0)._1) g m

instance Num a => Field2 (Mesh a) (Mesh a) (Vertex a) (Vertex a) where
  _2 g (Triangle a b c) = g b <&> (\x -> Triangle a x c)
  _2 g (Quad a b c d) = g b <&> (\x -> Quad a x c d)
  _2 g m = (singular (ix 0)._2) g m

instance Num a => Field3 (Mesh a) (Mesh a) (Vertex a) (Vertex a) where
  _3 g (Triangle a b c) = g c <&> Triangle a b
  _3 g (Quad a b c d) = g c <&> (\x -> Quad a b x d)
  _3 g m = (singular (ix 0)._3) g m

instance Num a => Field4 (Mesh a) (Mesh a) (Vertex a) (Vertex a) where
  _4 g (Triangle a b c) = g c <&> Quad a b c
  _4 g (Quad a b c d) = g d <&> Quad a b c
  _4 g m = (singular (ix 0)._4) g m

-- fix this nonsense
instance (Epsilon a, Floating a) => HasNormal (Mesh a) a where
  _n g m = let m0 = m ^. ix 0; fs = faceSpace m0 in m <$ _n g fs

-- fix this nonsense
instance (Floating a, Epsilon a) => HasTangent (Mesh a) a where
  _tn g m = let m0 = m ^. ix 0; fs = faceSpace m0 in m <$ _tn g fs

-- fix this nonsense
instance (Floating a, Epsilon a) => HasBitangent (Mesh a) a where
  _btn g m = let m0 = m ^. ix 0; fs = faceSpace m0 in m <$ _btn g fs

instance Num a => Tagged (Mesh a) (Mesh a) (Mesh a) (Mesh a) where
  tagged s g  = (each._TaggedMesh) f
    where f (s1, m) =
            if s1 == s then (,) s1 <$> g m
            else pure (s1, m)

instance Hashable a => Hashable (Mesh a) where
  hashWithSalt s EmptyMesh = s `hashWithSalt` (0::Int)
  hashWithSalt s (Triangle a b c) =
    s `hashWithSalt` (1::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
  hashWithSalt s (Quad a b c d) =
    s `hashWithSalt` (2::Int) `hashWithSalt` a
    `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
  hashWithSalt s (TriangleFan c ls) =
    s `hashWithSalt` (4::Int) `hashWithSalt` c `hashWithSalt` ls
  hashWithSalt s (TriangleSeq vs _) =
    s `hashWithSalt` (5::Int) `hashWithSalt` toListOf folded vs
  hashWithSalt s (FlipSided m) = s `hashWithSalt` (6::Int) `hashWithSalt` m
  hashWithSalt s (DoubleSided m) = s `hashWithSalt` (7::Int) `hashWithSalt` m
  hashWithSalt s (Closed m) = s `hashWithSalt` (8::Int) `hashWithSalt` m
  hashWithSalt s (TaggedMesh k m) =
    s `hashWithSalt` (9::Int) `hashWithSalt` k `hashWithSalt` m
  hashWithSalt s (MeshSeq m) =
    s `hashWithSalt` (10::Int) `hashWithSalt` toListOf folded m

instance Fractional a => HasCenter (Edge (Vertex a)) a where
  _center g (Edge a b) =
    (\c' -> let d = c' - c
            in Edge (over _pos (d +) a) (over _pos (d +) b)) <$> g c
    where
      a' = view _pos a
      b' = view _pos b
      c = (a' + b') * 0.5

instance HasVertices (Edge (Vertex a)) a where
  vertices g (Edge v1 v2) = Edge <$> g v1 <*> g v2

instance ToVertices (Edge (Vertex a)) a where
  toVerticesI _ = toListOf vertices

instance Num a => Field1 (Edge (Vertex a)) (Edge (Vertex a)) (Vertex a) (Vertex a) where
  _1 g (Edge v1 v2) = (Edge ?? v2) <$> g v1

instance Num a => Field2 (Edge (Vertex a)) (Edge (Vertex a)) (Vertex a) (Vertex a) where
  _2 g (Edge v1 v2) = Edge v1 <$> g v2

instance Num a => TwoFaced (Vertex a) where
  flipFace = flipNormal . flipTangent

instance (Fractional a, Ord a) => ToMesh [Vertex a] a where
  toMesh vs = TriangleFan c vs
    where c = Vertex3 $ view _center vs

instance Fractional a => HasCenter (Vertex a) a where
  _center = _pos

instance (Fractional a, Ord a) => HasCenter [Vertex a] a where
  _center = meanOf (traversed._center)

instance Fractional a => HasCenter (V3 a) a where
  _center = id

instance (Fractional a, Ord a) => HasCenter [V3 a] a where
  _center = meanOf traversed

instance Fractional a => FaceSpace (Vertex a) a where
  faceSpace m = m ^. _TBN

-- NOT
instance Fractional a => FaceSpace [Vertex a] a where
  faceSpace m = fromMaybe identity (m ^? _head._TBN)


mesh :: (ToMesh t a, Default (MeshOptions t)) => t -> Mesh a
mesh = toMesh

vmap :: (Vertex a -> Vertex a) -> Mesh a -> Mesh a
vmap _ EmptyMesh = EmptyMesh
vmap f (Triangle a b c) = Triangle (f a) (f b) (f c)
vmap f (Quad a b c d) = Quad (f a) (f b) (f c) (f d)
vmap f (TriangleFan c ls) = TriangleFan (f c) (f <$> ls)
vmap f (TriangleSeq vs is) = TriangleSeq (f <$> vs) is
vmap f (FlipSided m) = FlipSided (vmap f m)
vmap f (DoubleSided m) = DoubleSided (vmap f m)
vmap f (Closed m) = Closed (vmap f m)
vmap f (TaggedMesh s m) = TaggedMesh s (vmap f m)
vmap f (MeshSeq a) = MeshSeq (vmap f <$> a)

closed :: [a] -> [a]
closed l@(a:_:_) = l ++ [a]
closed a = a

pureMe :: Traversal' (Mesh a) (Mesh a)
pureMe g (Closed m) = Closed <$> g m
pureMe g (TaggedMesh s m) = TaggedMesh s <$> g m
pureMe g (MeshSeq a) = MeshSeq <$> traverse g a
pureMe g m = g m

mseq :: Num a => Traversal' (Mesh a) (Mesh a)
mseq g (FlipSided sq) = mseq g (reverseMesh sq)
mseq g (DoubleSided m) = (<>) <$> mseq g m <*> mseq g (reverseMesh m)
mseq g (Closed m) = (mseq.(\g' x -> g' (closedMe x))) g m
mseq g (TaggedMesh s m) = (mseq.(\g' x -> g' (TaggedMesh s x))) g m
mseq g (MeshSeq sq) = MeshSeq <$> traverse g sq
mseq g m = faces g m

imx :: Num a => Int -> Traversal' (Mesh a) (Mesh a)
-- ?? !!
imx _ g EmptyMesh = pure EmptyMesh
-- ?? make indices other than 0 traverse EmptyMesh ??
imx _ g m@Triangle{} = g m
imx _ g m@Quad{} = g m
imx i g (FlipSided m) = imx i g (reverseMesh m)
imx 0 g (DoubleSided m) = (<> reverseMesh m) <$> g m
imx _ g (DoubleSided m) = (m <>) <$> g (reverseMesh m)
imx i g (Closed m) = (imx i.(\g' x -> g' (closedMe x))) g m
imx i g (TaggedMesh s m) = (imx i.(\g' x -> g' (TaggedMesh s x))) g m
imx i g (MeshSeq s)
  | i < 0 = MeshSeq <$> ix (length s + i) g s
  | otherwise = MeshSeq <$> ix i g s
imx i g m = ix i g m

meshSeq :: [Mesh a] -> Mesh a
meshSeq = MeshSeq . Seq.fromList

tri :: (Epsilon a, Floating a, HasVertices f a) => f -> Mesh a
tri l = tri' (toListOf vertices l)
  where tri' (a:b:c:_) = setPerVertexTBN $ Triangle a b c
        tri' _ = EmptyMesh

quad :: (Epsilon a, Floating a, HasVertices f a) => f -> Mesh a
quad l = quad' (toListOf vertices l)
  where quad' (a:b:c:d:_) = setPerVertexTBN $ Quad a b c d
        quad' _ = EmptyMesh

primitive :: (Epsilon a, Floating a, HasVertices f a) => f -> Mesh a
primitive l = prim' (toListOf vertices l)
  where
    prim' l@(_:_:_:[]) = tri l
    prim' l@(_:_:_:_:_) = quad l
    prim' _ = EmptyMesh

tagMesh :: String -> Mesh a -> Mesh a
tagMesh = TaggedMesh

flipSided :: HasMesh t a => t -> t
flipSided = over _mesh flipSided'
  where
    flipSided' (FlipSided m) = m
    -- flipSided' m@DoubleSided{} = m
    flipSided' m = FlipSided m

doubleSided :: HasMesh t a => t -> t
doubleSided = over _mesh doubleSided'
  where
    -- doubleSided' (FlipSided m) = DoubleSided m
    doubleSided' m@DoubleSided{} = m
    doubleSided' m = DoubleSided m

closedMe, closedMesh :: Mesh a -> Mesh a
closedMe (Closed m) = Closed m
closedMe m = Closed m
closedMesh = closedMe

isFacing :: (Epsilon a, Floating a, Ord a) => V3 a -> a -> Mesh a -> Bool
isFacing v a q@Quad{} = acos (dot v (view (to faceSpace._n) q)) < a
isFacing v a q@Triangle{} = acos (dot v (view (to faceSpace._n) q)) < a
isFacing v a m = allOf faces (isFacing v a) m

facing :: (Epsilon a, Floating a, Ord a) => V3 a -> a -> Traversal' (Mesh a) (Mesh a)
facing v a = faces.filtered (isFacing v a)

notFacing :: (Epsilon a, Floating a, Ord a) => V3 a -> a -> Traversal' (Mesh a) (Mesh a)
notFacing v a = faces.filtered (not . isFacing v a)

maximumFacing :: (Epsilon a, Floating a, Num a, Ord a, HasFaces t a)
              => V3 a -> t -> Mesh a
maximumFacing v o =
  let
    v' = normalize v
    (_, rE) = foldlOf' faces k (0, EmptyMesh) o
    k (!m, !r) a =
      let m' = dot v' (a ^. to faceSpace._n)
      in if m' > m then (m', a) else (m, r)
  in rE

setPerVertexTBN ::(Epsilon a, Floating a) => Mesh a -> Mesh a
setPerVertexTBN (TriangleSeq vs is) = TriangleSeq (foldl f vs is) is
  where f vs' (Indices i1 i2 i3) =
          let fs = triangleSpace (vs' `Seq.index` i1)
                   (vs' `Seq.index` i2) (vs' `Seq.index` i3)
          in vs' & ix i1 %~ set _TBN fs
                 & ix i2 %~ set _TBN fs
                 & ix i3 %~ set _TBN fs
setPerVertexTBN m =
  over faces (\face -> let fs = faceSpace face in vmap (set _TBN fs) face) m

faceUV :: Num a => Mesh a -> Mesh a
faceUV (Triangle a b c) = Triangle a' b' c'
  where a' = set _uv (V2 0 0) a
        b' = set _uv (V2 1 0) b
        c' = set _uv (V2 1 1) c
faceUV (Quad a b c d) = Quad a' b' c' d'
  where a' = set _uv (V2 0 0) a
        b' = set _uv (V2 1 0) b
        c' = set _uv (V2 1 1) c
        d' = set _uv (V2 0 1) d
faceUV m = over faces faceUV m

toTriangleSeq :: Num a => Mesh a -> Mesh a
toTriangleSeq EmptyMesh = TriangleSeq Seq.empty Seq.empty
toTriangleSeq (Triangle a b c) =
  TriangleSeq (Seq.fromList [a, b, c]) (Seq.fromList [Indices 0 1 2])
toTriangleSeq (Quad a b c d) =
  TriangleSeq (Seq.fromList [a, b, c, d]) (Seq.fromList [Indices 0 1 2, Indices 2 3 0])
toTriangleSeq (TriangleFan a as) =
  let las = length as
  in if las > 1 then TriangleSeq (Seq.fromList (a:as))
                     (Seq.fromList [Indices x (x+1) 0 | x <- [1..(las - 1)]])
     else TriangleSeq Seq.empty Seq.empty
toTriangleSeq t@TriangleSeq{} = t
toTriangleSeq (FlipSided m) = toTriangleSeq (reverseMesh m)
toTriangleSeq (DoubleSided m) = toTriangleSeq (m <> reverseMesh m)
toTriangleSeq (Closed m) = toTriangleSeq m
toTriangleSeq (TaggedMesh _ m) = toTriangleSeq m
toTriangleSeq (MeshSeq s) =
  let (_, triangleSeq) = foldl' f (0, TriangleSeq Seq.empty Seq.empty) s
      f (!vc, TriangleSeq !verts !fcs) m =
        let (TriangleSeq !vertsNew !facesNew) = toTriangleSeq m
            vertsNewCount = length vertsNew
            plusN = (\(Indices a b c) -> Indices (a + vc) (b + vc) (c + vc))
        in (vc + vertsNewCount,
            TriangleSeq (verts >< vertsNew) (fcs >< (plusN <$> facesNew)))
  in triangleSeq

rewiseMesh :: Mesh a -> Mesh a
rewiseMesh EmptyMesh = EmptyMesh
rewiseMesh (Triangle a b c) = Triangle b a c
rewiseMesh (Quad a b c d) = Quad b a d c
rewiseMesh (TriangleSeq vs is) = TriangleSeq vs (rewiseIndices <$> is)
rewiseMesh (TriangleFan c ls) = TriangleFan c (reverse ls)
rewiseMesh (DoubleSided m) = DoubleSided $ rewiseMesh m
rewiseMesh (FlipSided m) = FlipSided $ rewiseMesh m
rewiseMesh (Closed m) = Closed $ rewiseMesh m
rewiseMesh (TaggedMesh s m) = TaggedMesh s (rewiseMesh m)
rewiseMesh (MeshSeq s) = MeshSeq (fmap rewiseMesh s)

rewiseIndices :: Indices -> Indices
rewiseIndices (Indices a b c) = Indices b a c

reverseMesh :: Num a => Mesh a -> Mesh a
reverseMesh (FlipSided m) = m
reverseMesh m = vmap flipFace $ rewiseMesh m

triangleSpace :: (Epsilon a, Floating a, HasPosition t a) => t -> t -> t -> M33 a
triangleSpace v1 v2 v3 =
  let edge1 = (v2 ^. _pos) ^-^ (v1 ^. _pos)
      edge2 = (v3 ^. _pos) ^-^ (v1 ^. _pos)
      n = normal edge1 edge2
      tn = normalize edge1
      btn = cross n tn
  in transpose $ V3 tn btn n

uvSpace :: (Epsilon a, Floating a, HasPosition t a, HasUV t a)
        => t -> t -> t -> M33 a
uvSpace v1 v2 v3 =
  let edge1 = (v2 ^. _pos) ^-^ (v1 ^. _pos)
      edge2 = (v3 ^. _pos) ^-^ (v1 ^. _pos)
      deltaUV1 = (v2 ^. _uv) ^-^ (v1 ^. _uv)
      deltaUV2 = (v3 ^. _uv) ^-^ (v1 ^. _uv)
      uvM = V2 deltaUV1 deltaUV2
      eM = V2 edge1 edge2
      tbM = inv22 uvM !*! eM
      n = normal edge1 edge2
      tn = normalize $ tbM ^._x
      btn = normalize $ tbM ^._y
  in transpose $ V3 tn btn n

area :: Floating a => Mesh a -> a
area (Triangle a b c) = 0.5 * norm (cross (a' - b') (a' - c'))
  where a' = a ^. _pos
        b' = b ^. _pos
        c' = c ^. _pos
area (Quad a b c d) = area (Triangle a b c) + area (Triangle a c d)
area (Closed m) = area m
area (TaggedMesh _ m) = area m
area m = sumOf (faces.to area) m

hLine :: (Floating a, Ord a, RealFrac a) => a -> a -> Mesh a -> Vertex a
hLine y x (Quad e1 e2 e3 e4) = lerp y (lerp x e3 e4) (lerp x e2 e1)
hLine y x (Triangle e1 e2 e3) =
  lerp y (if x <= 0.5 then lerp (2*x) e3 e1
           else lerp (2 * (x-0.5)) e2 e3) (lerp x e2 e1)
hLine y x (TaggedMesh _ m) = hLine y x m
hLine y x (Closed m) = hLine y x m
hLine y x m = hLine y x (m ^. ix 0)

vLine :: (Floating a, Ord a, RealFrac a) => a -> a -> Mesh a -> Vertex a
vLine = flip hLine

fixDegen :: (Epsilon a, Floating a) => Mesh a -> Mesh a
fixDegen EmptyMesh = EmptyMesh
fixDegen (Quad a b c d) = fd [a, b, c, d]
  where fd l = let uniq = nubBy (\a b -> nearZero ((a ^. _pos) - (b  ^. _pos))) l
               in case length uniq of
                    4 -> let t1 = Triangle a b c
                             t2 = Triangle a c d
                             t1d = isCollinear t1
                             t2d = isCollinear t2
                      in if t1d then
                           if t2d then EmptyMesh else t2
                         else
                           if t2d then t1 else quad uniq
                    3 -> fixDegen $ tri uniq
                    _ -> EmptyMesh
fixDegen t@Triangle{} = if isCollinear t then EmptyMesh else t
fixDegen m = over faces fixDegen m

isCollinear :: (Epsilon a, Floating a) => Mesh a -> Bool
isCollinear t@(Triangle a b c) =
  let a' = a ^. _pos
      b' = b ^. _pos
      c' = c ^. _pos
      (V3 v1 v2 v3) = (b' ^-^ a') / (c' ^-^ a')
  in (nearZero (v1 - v2) && nearZero (v1 - v3))

faceCount :: Mesh a -> Int
faceCount EmptyMesh = 0
faceCount Triangle{} = 1
faceCount Quad{} = 1
faceCount (TriangleFan _ ls) = length ls - 1
faceCount (TriangleSeq _ fcs) = length fcs
faceCount (DoubleSided m) = 2 * faceCount m
faceCount (FlipSided m) = faceCount m
faceCount (Closed m) = faceCount m
faceCount (TaggedMesh _ m) = faceCount m
faceCount (MeshSeq a) = sum (fmap faceCount a)

edgeE :: (Epsilon a, Floating a, HasPosition t a) => Edge t -> V2 a -> a
edgeE (Edge a b) v =
  let (V2 pa pb) = (view (_pos._xy) <$> V2 a b)
      n = (normalize . perp  $ (pb - pa))
  in dot n v  - dot n pa

insideE :: (Floating a, Epsilon a, Ord a, HasPosition s a, HasEdges t a) =>
           t -> s -> a -> Bool
insideE m a r = allOf edges (\e -> edgeE e (a ^. _pos._xy) > r) m

pointOnEdge :: (Additive t, Num a) => a -> Edge (t a) -> t a
pointOnEdge x (Edge a b) = lerp x a b

meanOf :: Fractional a => Traversal' t a -> Lens' t a
meanOf l g t = (\c' -> let d = c' - c in over l (d+) t) <$> g c
  where (n, d) = foldOf (l.to (\x -> (Sum x, Sum 1))) t
        n' = getSum n
        d' = getSum d
        c = if d' == 0 then 0 else n' / fromIntegral d'

rotateL :: Num a => Mesh a -> Mesh a
rotateL (Triangle a b c) = Triangle b c a
rotateL (Quad a b c d) = Quad b c d a
rotateL x = over faces rotateL x

rotateR :: Num a => Mesh a -> Mesh a
rotateR (Triangle a b c) = Triangle c a b
rotateR (Quad a b c d) = Quad d a b c
rotateR x = over faces rotateR x
