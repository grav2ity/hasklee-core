module Hasklee.Mesh.Graph
  ( MeshGraph(..)
  , meshGraph
  , faceAdjList
  , edge
  ) where

import Control.Lens hiding (transform)
import Control.Monad.State
import Data.Foldable
import Data.Function
import qualified Data.PlanarGraph.Immutable as PGI
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Linear

import Hasklee.Mesh
import Hasklee.Vertex


-- terrible brute force mesh adjacency untill there is a proper Mesh data

data MeshGraph a = MeshGraph
                   { mgFaces :: V.Vector (Mesh a)
                   , mgVertices :: V.Vector (Vertex a)
                   , mgPlanarGraph :: PGI.PlanarGraph
                   }

meshGraph :: (Epsilon a, Floating a) => Mesh a -> MeshGraph a
meshGraph m =
  let mvec = V.fromList $ m ^.. faces
      (a, b) = bruteAdj' mvec
      pg = PGI.pgFromFaces (toList b)
  in MeshGraph mvec (fmap fst a) pg

faceAdj :: PGI.PlanarGraph -> Int -> [(PGI.Face, PGI.Face)]
faceAdj pg ei =
  let (he1, he2) = PGI.edgeHalfEdges (PGI.Edge ei)
      f h = guard (PGI.halfEdgeIsInterior h pg) *> return (PGI.halfEdgeFace h pg)
  in (,) <$> f he1 <*> f he2

faceAdjList :: PGI.PlanarGraph -> [(Int, Int, Int)]
faceAdjList pg = do
  (PGI.Edge ei) <- PGI.pgEdges pg
  (PGI.faceId -> f1, PGI.faceId -> f2) <- faceAdj pg ei
  return (ei, f1, f2)

bruteAdj' :: (Epsilon a, Floating a)
          => V.Vector (Mesh a)
          -> ( V.Vector (Vertex a, [Int])
             , V.Vector [Int] )
bruteAdj' mvec =
  let
    nearlyEq v1 v2 = nearZero $ ((-) `on` view _pos) v1 v2
    findOrAdd fi v vs =
      let mi = Seq.findIndexL (nearlyEq v . fst) vs
      in case mi of
        Just i -> (Seq.adjust' (over _2 (cons fi)) i vs, i)
        Nothing -> (vs |> (v, [fi]), Seq.length vs)


    go i rs = if i + 1 > V.length rs then return () else do
      (vs, faces) <- get
      let l = rs V.! i
          (vs', newFace) = foldlOf' vertices (\(vs, fi) v ->
                                                 let (vs', ni) = findOrAdd i v vs
                                                 in (vs', ni:fi)
                                             ) (vs, []) l
      put (vs', faces |> newFace)
      go (i + 1) rs

    (s1, s2) = execState (go 0 mvec) (Seq.empty, Seq.empty)
  in (V.fromList . toList $ s1, V.fromList . toList $ s2)

edge :: (Epsilon a, Floating a) => MeshGraph a -> Int -> (Edge (Vertex a), V3 a)
edge mg ei =
  let pg = mgPlanarGraph mg
      fa = faceAdj pg ei
      (f1i, f2i) = head fa
      (he1, he2) = PGI.edgeHalfEdges (PGI.Edge ei)
      v1 = mgVertices mg V.! PGI.vertexId (PGI.halfEdgeVertex he1 pg)
      v2 = mgVertices mg V.! PGI.vertexId (PGI.halfEdgeVertex he2 pg)
      f1 = mgFaces mg V.! PGI.faceId f1i
      f2 = mgFaces mg V.! PGI.faceId f2i
      n = signorm $ ((f1 ^. to faceSpace._n) ^+^ (f2 ^. to faceSpace._n)) ^* 0.5
  in if null fa then (Edge zero zero, zero) else (Edge v1 v2, n)
