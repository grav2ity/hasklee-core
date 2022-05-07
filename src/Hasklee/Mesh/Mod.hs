module Hasklee.Mesh.Mod
  ( rectangleMesh, rectangleMe
  , squareMesh, squareMe
  , triangleMesh, triangleMe
  , buttonize
  , slit
  , onEdge
  ) where

import qualified Algebra.Graph as AG
import qualified Algebra.Graph.Labelled as AGL
import Control.Monad.State
import Data.Default.Class
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq
import Control.Lens hiding (transform)
import Linear

import Hasklee.Coords
import Hasklee.Mesh
import Hasklee.Spatial
import Hasklee.Transform
import Hasklee.Weave
import Hasklee.Vertex


rectangleMesh, rectangleMe :: (Epsilon a, Floating a) => a -> a -> Mesh a
rectangleMesh a b = quad $ Vertex3 <$> rectangle a b
rectangleMe = rectangleMesh

squareMesh, squareMe :: (Epsilon a, Floating a) => a -> Mesh a
squareMesh a = rectangleMe a a
squareMe = squareMesh

triangleMesh, triangleMe :: (Epsilon a, Floating a) => a -> Mesh a
triangleMesh a = tri $ Vertex3 <$> triangle a
triangleMe = triangleMesh

buttonize :: Trans a => Mesh a -> Mesh a
buttonize m = weaveMeshSeq [m, m'] <> m' ^. faces
  where t = translateTBN (V3 0 0 (0.2 * h)) . surfaceScale (V2 0.6 0.6)
        m' = t m
        h = maximum . view _xy . extents $ m

slit :: (Trans a, ToVertices t a, Default (VertexOptions t))
     => V2 a -> t -> Mesh a
slit s t = weaveMeshSeq . fmap closed $ [toVertices t, surfaceScale s (toVertices t)]



onEdge :: (Trans a, Transformable t a, Extent t a)
       => Edge (Vertex a) -> V3 a -> a -> Spatial t a -> [t]
onEdge (Edge a b) n x t =
  let ap = view _pos a
      bp = view _pos b
      o = lerp x bp ap
      tn = normalize (bp ^-^ ap)
      btn = cross n tn
      ls = matrixT $ mkTransformationMat (Linear.transpose $ V3 btn n tn) o
  in transform ls <$> rend zero t
