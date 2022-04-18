module Hasklee.Solid
  ( Solid(..)
  , SolidMeshOptions(..)
  , solidV, solidM
  , cubeS
  , facePyramid
  , solidTop, solidBottom
  ) where

import Control.DeepSeq (NFData)
import Control.Lens hiding (transform)
import Data.Binary (Binary)
import Data.Default.Class
import Data.Foldable
import Data.List (transpose)
import GHC.Generics (Generic)
import Linear

import Hasklee.Coords
import Hasklee.Mesh
import Hasklee.Shape as Shape
import Hasklee.SolidRaw as SolidRaw
import Hasklee.Spatial
import Hasklee.Transform
import Hasklee.Vertex
import Hasklee.Weave


data Solid a = EmptySolid
             | Cube a
             | CubeS (V3 a)
             | Cone a a
             | Cylinder a a
             | Extrude (Shape a) (Shape a)
             | Icosahedron a
             | Pyramid a
             | Sphere a
             | Torus a a
             | TransformedSolid (Solid a) (Transform a)
             deriving (Binary, Eq, Generic, NFData, Show)


instance Trans a => Transformable (Solid a) a where
  transform t (TransformedSolid s t1) = TransformedSolid s (t <> t1)
  transform t s = TransformedSolid s t

instance Trans a => Local (Solid a) where
  _local g (TransformedSolid s t) =
    (\x -> x & _localSpace .~ (t <> x ^. _localSpace)) <$> g s
  _local g s = _local g (TransformedSolid s Origin)

instance Trans a => HasLocalSpace (Solid a) a where
  _localSpace g (TransformedSolid s t) = TransformedSolid s <$> _localSpace g t
  _localSpace g s = TransformedSolid s <$> g Origin

data SolidMeshOptions = SolidMeshOptions {hSegments :: Int, vSegments :: Int}

instance Default SolidMeshOptions where
  def = SolidMeshOptions 30 30

instance Trans a => ToMesh (Solid a) a where
  type instance MeshOptions (Solid a) = SolidMeshOptions

  toMeshI _ EmptySolid = EmptyMesh
  toMeshI _ (Cube s) = toMesh (CubeS $ pure s)
  toMeshI _ (CubeS (V3 x y z)) =
    weaveF (translateY y) (Closed bottom) <> top <> FlipSided bottom
    where bottom = translateY (-y * 0.5) (quad $ Vertex3 <$> rectangleL _zx z x)
          top = translateY y bottom
  toMeshI mo (Cone r h) =
    FlipSided bottom <>
    fold (weaveMesh (toVerticesI hSegs bottomShape)
          (toVerticesI hSegs top))
    where hSegs = ShapeMeshOptions $ hSegments mo
          bottomShape = translateY (-h * 0.5) .
                        rotateX (-0.5 * pi) $
                        ClosedShape (Circle r Origin)
          bottom = toMeshI hSegs bottomShape
          top = translateY (h * 0.5) $ Point Origin
  toMeshI mo (Cylinder r h) =
    weaveF (translateY h) (toVerticesI hSegs bottomShape) <>
    top <> FlipSided bottom
    where hSegs = ShapeMeshOptions $ hSegments mo
          bottomShape = translateY (-h * 0.5) .
                        rotateX (-0.5 * pi) $
                        ClosedShape (Circle r Origin)
          topShape = translateY h bottomShape
          bottom = toMeshI hSegs bottomShape
          top = toMeshI hSegs topShape
  toMeshI mo (Extrude sh1 sh2) =
    fold (weaveMesh (toVerticesI hSegs $ closedShape sh1)
          (toVerticesI hSegs $ closedShape sh2))
    <> FlipSided bottom <> top
    where hSegs = ShapeMeshOptions $ hSegments mo
          bottom = toMeshI hSegs sh1
          top = toMeshI hSegs sh2
  toMeshI _ (Icosahedron a) = scaleU a SolidRaw.icosahedron
  toMeshI _ (Pyramid s) = facePyramid q
    where q = translateY (h * (-0.5)) $ quad $ Vertex3 <$> squareL _zx s
          h = sqrt 3.0 * s * 0.5
  toMeshI mo s@Sphere{} = runIdentity $ solidM mo s Identity
  toMeshI mo t@Torus{} = runIdentity $ solidM mo t Identity
  toMeshI mo (TransformedSolid s t) = transform t $ toMeshI mo s

instance Trans a => Extent (Solid a) a where
  extent _ EmptySolid = 0
  extent v (Cube s) = abs $ dot v (pure s) * 0.5
  extent v (CubeS s) = abs $ dot v s * 0.5
  -- extent (V3 x y z) (Cylinder r h) = norm (V2 x z) * r + y * 0.5 * h
  extent v@(V3 x y z) (Cone r h) =
    let (V2 x' z') = r *^ normalize (V2 x z)
    in maximum $ dot v <$> [V3 0 (0.5*h) 0, V3 x' (-0.5*h) z', V3 x' (-0.5*h) z']
  extent v (Sphere r) = norm v * r
  extent v (TransformedSolid s t) = extentR v t s
  extent v m = extent v $ toMesh m

instance Trans a => Default (Mesh a) where
    def = toMesh $ Cube 1


-- what about when s does not divide 1. last part will be off.
solidV :: Trans a => SolidMeshOptions -> Solid a -> [[Vertex a]]
solidV mo (Sphere r) =
  let sh = 1 / fromIntegral (hSegments mo)
      sv = 1 / fromIntegral (vSegments mo)
      p = V3 r 0 0
      gr = fmap (fmap ver) $
           [ [ rotateY (2 * pi * ry) . rotateZ (pi * rz) $ p |
               ry <- drop 1 [0,(0 + sv)..1] ] |
             rz <- tail . init $ [-0.5,(-0.5 + sh)..0.5] ]
  in [bottomV] : gr ++ [[topV]]
  where ver v = Vertex9 v n tn
          where n = signorm v
                tn = signorm $ cross (V3 0 1 0) v
        topV = Vertex9 (V3 0 r 0) (V3 0 1 0) (V3 1 0 0)
        bottomV = Vertex9 (V3 0 (-r) 0) (V3 0 (-1) 0) (V3 (-1) 0 0)
solidV mo (Torus r1 r2) =
  let sh = 1 / fromIntegral (hSegments mo)
      sv = 1 / fromIntegral (vSegments mo)
      p = V3 r1 0 0
      cir = fmap ver [ rotateZ (2 * pi * rz) p | rz <- init [0,(0 + sv)..1] ]
      cir' = translateX r2 cir
      in reverse [ rotateY (2 * pi * ry) cir' | ry <- drop 1 [0,(0 + sh)..1] ]
  where ver v = Vertex9 v n tn
          where n = signorm v
                tn = signorm $ cross (V3 0 0 1) v
solidV mo o = error $ "solidV for '"  ++ show o ++ "' not yet implemented"

solidM :: (Trans a , Monad m) => SolidMeshOptions -> Solid a ->
          ([[Vertex a]] -> m [[Vertex a]]) -> m (Mesh a)
solidM mo o@Sphere{} f = weaveMeshSeq . fmap closed <$> f (solidV mo o)
solidM mo o@Torus{} f = weaveMeshSeq . fmap closed . closed <$> f (solidV mo o)
solidM _ o _  = error $ "solidM for '"  ++ show o ++ "' not yet implemented"


cubeS :: a -> a -> a -> Solid a
cubeS x y z = CubeS (V3 x y z)


facePyramid :: Trans a => Mesh a -> Mesh a
facePyramid m@(Quad a b c d) =
  m' <> foldMapOf edges (\(Edge e1 e2) -> setPerVertexTBN $ tri [e2, e1, v]) m'
  where o = (a ^+^ b ^+^ c ^+^d) ^* 0.25
        h = norm (b ^-^ a) * sqrt 3.0 * 0.5
        v = o ^+^ Vertex3 (view _n a) ^* h
        m' = FlipSided m
facePyramid m@(Triangle a b c) =
  m' <> foldMapOf edges (\(Edge e1 e2) -> setPerVertexTBN $ tri [e2, v, e2]) m'
  where o = (a ^+^ b ^+^ c) ^/ 3
        h = norm (b ^-^ a) * sqrt 3.0 * 0.5
        v = o ^+^ Vertex3 (view _n a) ^* h
        m' = FlipSided m
facePyramid m = over faces facePyramid m


solidTop :: Trans a => Solid a -> Shape a
solidTop EmptySolid = EmptyShape Origin
solidTop (Cube s) = solidTop (CubeS $ pure s)
solidTop (CubeS (V3 x y z)) = translateY (0.5 * y) . rotateX (-0.5 * pi) $ Rectangle x z Origin
solidTop (Cone r h) = translateY (0.5 * h) . rotateX (-0.5 * pi) $ Point Origin
solidTop (Cylinder r h) = translateY (0.5 * h) . rotateX (-0.5 * pi) $ Circle r Origin
solidTop (Extrude _ sh) = sh
solidTop (Icosahedron r) = translateY r . rotateX (-0.5 * pi) $ Point Origin
solidTop (Pyramid s) = translateY (0.25 * sqrt 3.0 * s) . rotateX (-0.5 * pi) $ Point Origin
solidTop (Sphere r) = translateY r . rotateX (-0.5 * pi) $ Point Origin
solidTop (Torus r1 r2) = translateY r1 . rotateX (-0.5 * pi) $ Arc r2 0 (2*pi) Origin
solidTop (TransformedSolid s t) = transform t $ solidTop s

solidBottom :: Trans a => Solid a -> Shape a
solidBottom EmptySolid = EmptyShape Origin
solidBottom (Cube s) = solidBottom (CubeS $ pure s)
solidBottom (CubeS (V3 x y z)) = translateY (-0.5 * y) . rotateX (0.5 * pi) $ Rectangle x z Origin
solidBottom (Cone r h) = translateY (-0.5 * h) . rotateX (0.5 * pi) $ Circle r Origin
solidBottom (Cylinder r h) = translateY (-0.5 * h) . rotateX (0.5 * pi) $ Circle r Origin
solidBottom (Extrude sh _) = rotateX pi sh
solidBottom (Icosahedron r) = translateY (-r) . rotateX (0.5 * pi) $ Point Origin
solidBottom (Pyramid s) = translateY (-0.25 * sqrt 3.0 * s) . rotateX (0.5 * pi) $ Rectangle s s Origin
solidBottom (Sphere r) = translateY (-r) . rotateX (0.5 * pi) $ Point Origin
solidBottom (Torus r1 r2) = translateY (-r1) . rotateX (0.5 * pi) $ Arc r2 0 (2*pi) Origin
solidBottom (TransformedSolid s t) = transform t $ solidBottom s
