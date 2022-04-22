module Hasklee.Weave
  ( weave, weaveWith
  , weaveMesh, weaveMeshList, weaveMeshSeq
  , weaveF, weaveFC, weaveFC2
  , pipeWeave
  , uvGrid
  , zipF
  ) where

import Control.Lens hiding (transform)
import Data.Default.Class
import Data.Foldable
import Data.Functor.Compose
import Linear

import Hasklee.Coords
import Hasklee.Mesh
import Hasklee.Transform
import Hasklee.Vertex


uvGrid :: (Epsilon a, Floating a, HasUV t a) => [[t]] -> [[t]]
uvGrid vs =
  let vDiv = fromIntegral $ length vs - 1
      uDiv = fromIntegral $ minimum (length <$> vs) - 1
      f (i, j) v = _uv .~ V2 (fromIntegral j / uDiv) (fromIntegral i / vDiv) $ v
  in getCompose $ imap f (Compose vs)

weave :: Foldable f => f a -> f a -> [[a]]
weave = weaveWith id

weaveWith :: Foldable f => ([a] -> b) -> f a -> f a -> [b]
weaveWith f l r = weaveWith' (toList l) (toList r)
  where
    weaveWith' (a1:a2:as) (b1:b2:bs) = f [a1, a2, b2, b1] : weaveWith' (a2:as) (b2:bs)
    weaveWith' (a1:a2:as) [b] = f [a1, a2, b] : weaveWith' (a2:as) [b]
    weaveWith' [a] (b1:b2:bs) = f [a, b2, b1] : weaveWith' [a] (b2:bs)
    weaveWith'  _ _ = []

weaveMesh :: (Epsilon a, Floating a, ToVertices f a, Default (VertexOptions f))
          => f -> f -> [Mesh a]
weaveMesh a b = weaveWith (fixDegen . primitive) (toVertices a) (toVertices b)

zipF :: (Foldable f, Foldable g) => g (a -> a -> b) -> f a -> [b]
zipF f a = zipWith3 ($) (toList f) (toList a) (tail $ toList a)

weaveMeshList :: (Epsilon a, Floating a, Foldable f, ToVertices b a, Default (VertexOptions b))
              => f b -> [[Mesh a]]
weaveMeshList =  zipF (repeat weaveMesh)

weaveMeshSeq :: (Epsilon a, Floating a, Foldable f, ToVertices b a, Default (VertexOptions b))
             => f b -> Mesh a
weaveMeshSeq = meshSeq . fmap meshSeq . weaveMeshList . uvGrid . fmap toVertices . toList

weaveF :: (Trans a, ToVertices t a, Default (VertexOptions t)) => (t -> t) -> t -> Mesh a
weaveF f m = fold (weaveMesh m (f m))

weaveFC :: (Trans a, ToMesh t a, Default (MeshOptions t), ToVertices t a, Default (VertexOptions t))
        => (t -> t) -> t -> Mesh a
weaveFC f m = fold (weaveMesh m m') <> (m' ^. to mesh.faces)
  where m' = f m

weaveFC2 :: (Trans a, ToMesh t a, Default (MeshOptions t), ToVertices t a, Default (VertexOptions t))
         => (t -> t) -> t -> Mesh a
-- order
weaveFC2 f m = fold (weaveMesh m m') <> (m' ^. to mesh.faces) <> FlipSided (m ^. to mesh.faces)
  where m' = f m

pipeWeave :: (Trans a, ToDiscretePath t a) => [V3 a] -> [a] -> t -> Mesh a
pipeWeave shape w t = let s = Vertex3 <$> closed shape in
  weaveMeshSeq $ zipWith (\x y -> transform x . scaleU y $ s) (dpath t) w
