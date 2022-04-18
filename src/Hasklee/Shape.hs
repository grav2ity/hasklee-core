module Hasklee.Shape
  ( Shape(..)
  , ShapeMeshOptions(..)
  , shapeV
  , polygon
  , fromMesh, fromList
  , shape
  , closedShape
  , addHoles, addHoles'
  , localVertices
  , polygonShape
  , extrudeShape
  , ocircle
  ) where

import Control.DeepSeq (NFData)
import Control.Lens hiding (transform)

import Data.Binary (Binary)
import Data.Default.Class
import Data.Ext
import Data.Foldable
import qualified Data.Geometry as G
import Data.Geometry.Polygon as G
import Data.List (transpose)
import Data.Maybe
import Data.Ratio
import GHC.Generics (Generic)
import Linear
import Numeric.AD as AD
import System.Random
import Test.QuickCheck as Q

import Hasklee.Coords
import Hasklee.HGeometry
import Hasklee.Mesh
import Hasklee.Randomize
import Hasklee.Spatial
import Hasklee.Transform
import Hasklee.Vertex
import Hasklee.Weave


data Shape a = EmptyShape (Transform a)
             | Circle a (Transform a)
             | Ellipse a a (Transform a)
             | Arc a a a (Transform a)
             | Wedge a a a a a a (Transform a)
             | Ngon Int a (Transform a)
             | Point (Transform a)
             | Rectangle a a (Transform a)
             | Polygon (MultiPolygon () a) (Transform a)
             | ClosedShape (Shape a)
             deriving (Binary, Eq, Generic, NFData, Show)


instance Trans a => Transformable (Shape a) a where
  transform t = _localSpace %~ (t <>)

instance Trans a => HasLocalSpace (Shape a) a where
  _localSpace g (EmptyShape t) = EmptyShape <$> g t
  _localSpace g (Circle r t) = Circle r <$> g t
  _localSpace g (Ellipse r1 r2 t) = Ellipse r1 r2 <$> g t
  _localSpace g (Arc r as ae t) = Arc r as ae <$> g t
  _localSpace g (Wedge r1 r2 rs re as ae t) = Wedge r1 r2 rs re as ae <$> g t
  _localSpace g (Ngon i a t) = Ngon i a <$> g t
  _localSpace g (Point t) = Point <$> g t
  _localSpace g (Rectangle a b t) = Rectangle a b <$> g t
  _localSpace g (Polygon p t) = Polygon p <$> g t
  _localSpace g (ClosedShape s) = ClosedShape <$> _localSpace g s

instance Trans a => HasPosition (Shape a) a where
  _pos = _localSpace._pos

instance Trans a => HasNormal (Shape a) a where
  _n = _localSpace._n

instance Trans a => HasTangent (Shape a) a where
  _tn = _localSpace._tn

instance Trans a => HasBitangent (Shape a) a where
  _btn = _localSpace._btn

instance Trans a => HasCenter (Shape a) a where
  _center g (Polygon p t) =
    let c = transform t . view (G.outerBoundary . to G.centroid . p2v3) $ p
    in (\c' -> translate (c' ^-^ (t ^. _pos)) (Polygon p t)) <$> g c
  _center g (ClosedShape s) = _center g s
  _center g p = (_localSpace._pos) g p

instance Trans a => HasCenter [Shape a] a where
  _center = meanOf (traversed._center)

newtype ShapeMeshOptions = ShapeMeshOptions {shapeSegments :: Int}

instance Default ShapeMeshOptions where
  def = ShapeMeshOptions 30

instance Trans a => ToMesh (Shape a) a where
  type instance MeshOptions (Shape a) = ShapeMeshOptions

  toMeshI _ EmptyShape{} = EmptyMesh
  toMeshI _ Point{} = EmptyMesh
  toMeshI _ (Rectangle a b t) = transform t . quad $ Vertex3 <$> rectangle a b
  toMeshI _ (Polygon p t) = transform t $ toMesh p
  toMeshI mo (ClosedShape c) = toMeshI mo c
  toMeshI mo c@Arc{} = meshSeq $ tri <$> Data.List.transpose
                      [init vs, tail vs, replicate (length vs - 1) o]
    where o = transform t $ Vertex9 (V3 0 0 0) (V3 0 0 (-1)) (V3 0 0 0)
          vs = toVerticesI mo c
          t = c ^. _localSpace
  toMeshI mo (Wedge r1 r2 rs re as ae t) =
    transform t . flipSided $ weaveMeshSeq [innerArc, outerArc]
    where
      innerArc = vertex <$> if nearZero rs then [zero]
                            else arcL _xy (ofEllipse (rs*r1) (rs*r2)) as ae (shapeSegments mo)
      outerArc = vertex <$> arcL _xy (ofEllipse (re*r1) (re*r2)) as ae (shapeSegments mo)
  toMeshI mo c = meshSeq $ tri <$> Data.List.transpose
             [init vs, tail vs, replicate (length vs - 1) o]
    where o = transform t $ Vertex9 (V3 0 0 0) (V3 0 0 (-1)) (V3 0 0 0)
          vs = closed $ toVerticesI mo c
          t = c ^. _localSpace

instance Trans a => FaceSpace (Shape a) a where
  faceSpace = view (_localSpace._TBN)

instance Trans a => HasVertices (Shape a) a where
  vertices g (ClosedShape s) = fromList . init <$>
    traverse g (closed $ transform (s ^. _localSpace) $ localVertices def s)
  vertices g s = fromList <$>
    traverse g (transform (s ^. _localSpace) $ localVertices def s)

instance Trans a => ToVertices (Shape a) a where
  type instance VertexOptions (Shape a) = ShapeMeshOptions
  toVerticesI mo (ClosedShape s) = closed $ transform (s ^. _localSpace) $ localVertices mo s
  toVerticesI mo s = transform (s ^. _localSpace) $ localVertices mo s

instance Trans a => ToPolygon (Shape a) a where
  polygonT t (ClosedShape m) = polygonT t m
  polygonT t m = polygonLL t (toVertices m)

instance Trans a => Extent (Shape a) a where
  extent v (Circle r t) =  dot v (t ^. _pos) + r
  extent v m = extent v (mesh m)

instance Trans a => HasEdges (Shape a) a where
  edges _ p@EmptyShape{} = pure p
  edges _ p@Point{} = pure p
  edges g (Polygon p t) = fromList . fmap (\(Edge v1 _) -> v1) <$>
    traverse g (fmap (transform t) . ls2edge <$> toList (G.outerBoundaryEdges p))
  edges g p = edges g (polygonShape p)

instance (Random a, Trans a) => HasRandomPoint (Shape a) a where
  randomPoint (EmptyShape t) = randomPoint (Point t)
  randomPoint (Circle r t) = do
    rr <- choose (0, r)
    aa <- choose (0, 2*pi)
    return $ transform t $ rotateZ aa $ V3 1 0 0 ^* rr
  randomPoint (Point t) = return . transform t $ pure 0
  randomPoint (Rectangle a b t) = do
    aa <- 0.5 *^ choose (-a, a)
    bb <- 0.5 *^ choose (-b, b)
    return $ transform t $ V3 aa bb 0
  randomPoint (Polygon r t) = transform t <$> randomPoint r
  randomPoint (ClosedShape s) = randomPoint s
  randomPoint s = randomPoint (polygonShape s)

polygon :: (Eq a, Num a, R2 t) => [t a] -> Transform a -> Shape a
polygon ls t =
  let f a = ext $ G.Point2 (a ^. _x) (a ^. _y)
  in Polygon
     (MultiPolygon (G.toCounterClockWiseOrder $ G.fromPoints (f <$> ls)) []) t

fromMesh :: Trans a => Mesh a -> Shape a
fromMesh m =
  let n = view (_localSpace._n) m
      nnn = dot n (m ^. _1._pos)
  in Polygon (MultiPolygon (polygonW m) [])
  (transform (m ^.to faceSpace.to m33_to_m44.to matrixT) (translateT (V3 0 0 nnn)))

fromList :: (Trans a, HasPosition (t a) a, Transformable (t a) a) =>
            [t a] -> Shape a
fromList m =
  let n = view (_localSpace._n) m
      nnn = dot n (view _pos $ head m)
  in Polygon (MultiPolygon (polygonW m) [])
  (transform (m ^._localSpace._TBN.to m33_to_m44.to matrixT) (translateT (V3 0 0 nnn)))


shape :: Trans a => Iso' (Mesh a) (Shape a)
shape = iso fromMesh toMesh

closedShape :: Shape a -> Shape a
closedShape s@ClosedShape{} = s
closedShape s = ClosedShape s

polygonShape :: Trans a => Shape a -> Shape a
polygonShape (ClosedShape s) = polygonShape s
polygonShape p@Polygon{} = p
polygonShape s =
  Polygon (MultiPolygon (G.fromPoints . fmap f $ localVertices def s) [])
  (s ^. _localSpace)
  where
    f (view _pos -> V3 x y _) = ext $ G.Point2 x y

localVertices :: Trans a => ShapeMeshOptions -> Shape a -> [Vertex a]
localVertices mo (ClosedShape s) = localVertices mo s
localVertices _ (Ngon i r t) = shapeV (Circle r t) i
localVertices _ (Rectangle a b _) = Vertex3 <$> rectangle a b
localVertices _ (Polygon sm _) = ff (view G.outerBoundary sm)
  where
    ff x =  fmap f' . toList . view G.outerBoundaryVector . toCounterClockWiseOrder $ x
    f' (G.Point2 x y :+ _) = Vertex3 $ V3 x y 0
localVertices mo s = shapeV s (shapeSegments mo)


shapeV :: Trans a => Shape a -> Int -> [Vertex a]
shapeV (Circle r _) s = f <$> vs
  where f t = Vertex9 (t^._pos) (V3 0 0 (-1)) (t^._tn)
        ff = continuousPath (circleF _xy (AD.auto r))
        vs = init $ ff <$> range 0 (2*pi) (s+1)
shapeV (Ellipse r1 r2 _) s = f <$> vs
  where f t = Vertex9 (t^._pos) (V3 0 0 (-1)) (t^._tn)
        ff = continuousPath (ellipseF _xy (AD.auto r1) (AD.auto r2))
        vs = init $ ff <$> range 0 (2*pi) (s+1)
-- change this
shapeV (Arc r as ae _) seg = f <$> arcL _xy (ofCircle r) as ae seg
  where f v = Vertex9 v n tn
          where n = V3 0 0 (-1)
                tn = signorm $ negated v
-- change this
shapeV (Wedge r1 r2 rs re as ae t) seg = outerArc ++ reverse innerArc
    where
      innerArc = vertex <$> if nearZero rs then [zero]
                                 else arcL _xy (ofEllipse (rs*r1) (rs*r2)) as ae seg
      outerArc = vertex <$> arcL _xy (ofEllipse (re*r1) (re*r2)) as ae seg
shapeV (Point _) _ = pure $ Vertex3 zero
shapeV r@(Rectangle _ _ t) ii = shapeV (polygon (localVertices def r) t) ii
shapeV p@Polygon{} ii =
  [ Vertex9 s nor (e - s) | (p0, p1) <- zip <$> init <*> tail $ closed ps,
                            (s, e) <- zip <$> init <*> tail $ lerp <$> rs <*> [p1] <*> [p0] ]
  where
    rs = range 0 1 ii
    ps = localVertices def p ^.. (each._pos)
    nor = V3 0 0 (-1)
shapeV (ClosedShape c) ii = shapeV c ii
shapeV o _  = error $ "shapeV for '"  ++ show o ++ "' not yet implemented"


addHoles' :: Trans a => [G.SimplePolygon () a] -> Shape a -> Shape a
addHoles' nhls (Polygon p t) =
  Polygon ((\(MultiPolygon b hls) -> MultiPolygon b (nhls++hls)) p) t
addHoles' sp sh = addHoles' sp (polygonShape sh)

addHoles :: Trans a => [Shape a] -> Shape a -> Shape a
addHoles nhls (Polygon p t) =
  Polygon ((\(MultiPolygon b hls) -> MultiPolygon b (nhls'++hls)) p) t
  where
    nhls' = polygonT t <$> nhls
addHoles nhls shape = addHoles nhls (polygonShape shape)

extrudeShape :: Trans a => a -> Shape a -> Mesh a
extrudeShape d p@(Polygon mp@(MultiPolygon o ho) t) =
  let
    n = p ^. _localSpace._n
    top = translate (n ^* d) (mesh p)
    bottom = FlipSided (mesh p)
    outer = weaveF (translate (n ^* d)) (closed $ ff mp)
    holese = fmap (FlipSided . weaveF (translate (n ^* d)) . closed . ff) ho
    ff x =  fmap f' . toList . view outerBoundaryVector . toCounterClockWiseOrder $ x
    f' (G.Point2 x y :+ _) = Vertex3 $ V3 x y 0
  in transform t outer <> transform t (fold holese) <> top <> bottom
extrudeShape d s = weaveFC2 (translate ((s ^. _localSpace._n) ^* d)) (toMesh s)

ocircle :: Trans a => Shape a -> Shape a
ocircle (EmptyShape t) = Circle 0 t
ocircle a@Circle{} = a
ocircle (Ellipse r1 r2 t) = Circle (max r1 r2) t
ocircle (Arc r _ _ t) = Circle r t
ocircle (Wedge r1 r2 _ _ _ _ t) = ocircle (Ellipse r1 r2 t)
ocircle (Ngon _ r t) = Circle r t
ocircle (Point t) = Circle 0 t
ocircle (Rectangle a b t) = Circle (sqrt $ a'*a' + b'*b') t
  where a' = a*0.5
        b' = b*0.5
ocircle a@Polygon{} =
  Circle (fromMaybe 0 $ maximumOf (to toVertices.folded._pos.to norm) a)
         (a ^. _localSpace)
ocircle (ClosedShape a) = ocircle a
