{-# LANGUAGE ScopedTypeVariables #-}

module Hasklee.HGeometry
  ( ToPolygon(..)
  , p2v3
  , point2
  , ls2edge
  , polygonLL
  , polygonInsidePolygon
  , conic
  ) where

import Algorithms.Geometry.PolygonTriangulation.Triangulate
import Control.Lens hiding (transform)
import Control.Monad
import Data.Binary
import Data.Ext
import Data.Foldable
import Data.Geometry as G
import Data.Geometry.Box as G
import Data.Geometry.PlanarSubdivision.Basic hiding (edges, faces, vertices)
import Data.Geometry.Polygon as G
import qualified Data.Geometry.Triangle as G
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.PlaneGraph.Core as PG
import Data.Proxy
import Data.Ratio
import qualified Data.Vector as V
import Data.Vinyl
import Data.Vinyl.CoRec
import Linear
import System.Random
import Test.QuickCheck hiding (scale)

import Hasklee.Mesh
import Hasklee.Randomize
import Hasklee.Transform
import Hasklee.Vertex


class ToPolygon t a | t -> a where
  polygonT :: Transform a -> t -> SimplePolygon () a

  polygonL :: t -> SimplePolygon () a
  default polygonL :: (HasLocalSpace t a) => t -> SimplePolygon () a
  polygonL m = polygonT (m ^. _localSpace) m

  polygonW :: t -> SimplePolygon () a
  default polygonW :: (Trans a, HasLocalSpace t a) => t -> SimplePolygon () a
  polygonW m = polygonT (set _pos zero $ m ^. _localSpace) m


instance (Trans a, HasPosition (t a) a, Transformable (t a) a) =>
         ToPolygon [t a] a where
  polygonT = polygonLL

instance Trans a => ToPolygon (Mesh a) a where
  polygonT t m = polygonLL t (toListOf vertices m)


-- orphans

instance Binary a => Binary (Point 2 a) where
  put = put . view v2
  get = review v2 <$> get

instance Binary a => Binary (Point 3 a) where
  put = put . view v3
  get = review v3 <$> get

instance (Binary a, Eq a, Num a) => Binary (Polygon 'Simple () a) where
  put o@SimplePolygon{} = put (_core <$> toPoints o)
  get = fromPoints . fmap ext <$> get

instance (Binary a, Eq a, Num a) => Binary (Polygon 'Multi () a) where
  put o@MultiPolygon{} = put (view outerBoundary o) >> put (view polygonHoles o)
  get = MultiPolygon <$> get <*> get

instance (Random a, Trans a) => HasRandomPoint (Polygon t p a) a where
  randomPoint = randomPointInsidePolygon >=> return . view p2v3

instance V2Iso (Point 2) a where
  v2 = iso to' from'
    where
      to' (Point2 x y) = V2 x y
      from' (V2 x y) = Point2 x y

instance V3Iso (Point 3) a where
  v3 = iso to' from'
    where
      to' (Point3 x y z) = V3 x y z
      from' (V3 x y z) = Point3 x y z

instance Trans a => ToMesh (SimplePolygon p a) a where
  toMesh sp =
    if length (polygonVertices sp) < 4 then simplePolygonMesh' sp
    else toMesh (MultiPolygon (view outerBoundary sp) [])

instance Trans a => ToMesh (MultiPolygon p a) a where
  toMesh =
    foldMap (either simplePolygonMesh' (const EmptyMesh) . _core . snd) .
    V.toList .
    V.filter (\f -> f^._2.extra == Inside) .
    internalFacePolygons . triangulate (Proxy :: Proxy ())

instance Trans a => ToMesh (SomePolygon () a) a where
  toMesh = either toMesh toMesh

instance Trans a => ToMesh (PG.PlaneGraph s v () () a) a where
  toMesh = foldMap (simplePolygonMesh' . _core . snd) . V.toList .  PG.internalFacePolygons

instance Trans a => ToMesh (PlanarSubdivision s v () () a) a where
  toMesh =
    foldMap (either simplePolygonMesh' (const EmptyMesh) . _core . snd) .
    internalFacePolygons


p2v3 :: Num a => Getter (Point 2 a) (V3 a)
p2v3 = to (\(Point2 x y) -> V3 x y 0)

point2 :: R2 t => t a -> Point 2 a
point2 t = Point2 (t ^. _x) (t ^. _y)

point3 :: R3 t => t a -> Point 3 a
point3 t = Point3 (t ^. _x) (t ^. _y) (t ^. _z)

ls2edge :: Num a => LineSegment 2 () a -> Edge (Vertex a)
ls2edge (LineSegment' (Point2 x1 y1 :+ _) (Point2 x2 y2 :+ _)) =
  Edge (Vertex3 (V3 x1 y1 0)) (Vertex3 (V3 x2 y2 0))


polygonLL :: (Trans a, Foldable t, HasPosition b a) => Transform a -> t b -> SimplePolygon () a
polygonLL t m =
  let pts = ext . point2 . transform (inv t) . view _pos <$> toList m
  in G.fromPoints pts


randomPointInsidePolygon :: Random r => RealFrac r => Polygon t p r -> Gen (Point 2 r)
randomPointInsidePolygon p = do
  let bbox = boundingBox p
      rrr x = fmap (approxRational ?? 0.001) x
  suchThat
    (choose (_core $ minPoint bbox, _core $ maxPoint bbox))
    (\a -> insidePolygon (pmap rrr a) (pmap rrr p))

pointInsidePolygon :: RealFrac r => Polygon t p r -> Point 2 r -> Bool
pointInsidePolygon poly point =
  let rrr x = fmap (approxRational ?? 0.001) x
  in G.insidePolygon (pmap rrr point) (pmap rrr poly)

polygonInsidePolygon :: RealFrac r => Polygon t1 p r -> Polygon t2 p r -> Bool
polygonInsidePolygon poly1 poly2 =
  all (pointInsidePolygon poly2) (_core <$> G.polygonVertices poly1)


-- for triangles only
simplePolygonMesh' :: Trans a => SimplePolygon p a -> Mesh a
simplePolygonMesh' = tri . fmap (view $ core.p2v3.to vertex) . NE.toList .
                     polygonVertices . toCounterClockWiseOrder


conic :: (Trans a, HasVertices t a) => V3 a -> t -> Mesh a -> [V3 a]
conic t c m =
    let
      o = review v3 t
      es = review v3 <$> toListOf (vertices._pos) c
      lines = G.lineThrough o <$> es
      tris = catMaybes $ preview mhtri <$> toListOf faces (toTriangleSeq m)

      collect :: Trans r => Line 3 r -> G.Triangle 3 () r -> Maybe (Point 3 r)
      collect l t = match (l `intersect` t) $
                  H (\NoIntersection -> Nothing)
                  :& H (\a@Point3{} -> Just a)
                  :& H (const Nothing)
                  :& RNil

      lineInter l = asum $ collect l <$> tris

      in view v3 <$> catMaybes (lineInter <$> lines)

mhtri :: Prism' (Mesh a) (G.Triangle 3 () a)
mhtri = prism' meshTriangle triangleMesh
  where
    meshTriangle (G.Triangle (p :+ _) (q :+ _) (r :+ _)) =
      Triangle (p ^. (v3.to vertex)) (q ^. (v3.to vertex)) (r ^. (v3.to vertex))
    triangleMesh (Triangle a b c) =
      Just $ G.Triangle (point3 a :+ ()) (point3 b :+ ()) (point3 c :+ ())
    triangleMesh _ = Nothing
