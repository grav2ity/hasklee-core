module Hasklee.MeshMod
  ( rectangleMesh, rectangleMe
  , squareMesh, squareMe
  , triangleMesh, triangleMe
  , buttonize
  , slit
  ) where

import Data.Default.Class
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
