module Hasklee.Subdiv
  ( Subdiv(..)
  , middle, subdivided
  , subdiv', sbd
  ) where

import Control.Lens hiding (transform)
import Data.Array
import Data.List
import Data.Ratio
import Linear

import Hasklee.Coords
import Hasklee.Mesh
import Hasklee.Shape as Shape
import Hasklee.Solid
import Hasklee.Spatial
import Hasklee.Transform
import Hasklee.Vertex
import Hasklee.Weave


class Subdiv t a | t -> a where
  type SubdivT t :: *
  subdiv :: V3 [a] -> t -> SubdivT t


subdivided :: (Trans r, Subdiv s r, a ~ SubdivT s) =>
  Int -> Int -> Int -> Traversal s a a a
subdivided x y z g a = g (subdiv' x y z a)


middle :: (Trans r, Subdiv s r, t ~ SubdivT s,
           Ixed t, Index t ~ Int, IxValue t ~ a) => Traversal s t a a
middle g a = ix 4 g (subdiv (pure (fmap fromRational [0, 1%3, 2%3, 1])) a)


instance (Epsilon a, Floating a, Ord a) => Subdiv (Mesh a) a where
  type instance SubdivT (Mesh a) = Mesh a

  subdiv _ EmptyMesh = EmptyMesh
  subdiv v (Closed m) = subdiv v m

  subdiv (V3 xs ys _) (Triangle e1 e2 e3) =
    collectX . fmap fixDegen . collectY . Data.List.transpose $ weaveMeshList
    [ [ lerp y xT xB | x <- xs,
        let xT = if x <= 0.5 then lehT1 (2*x) else lehT2 (2*(x-0.5)),
        let xB = lehB x ]
    | y <- ys ]
    where
      lehB x = lerp x e2 e1
      lehT1 x = lerp x e3 e1
      lehT2 x = lerp x e2 e3
      collectX = if length xs > 2 then meshSeq else head
      collectY  = if length ys > 2 then fmap meshSeq else concat

  subdiv (V3 xs ys _) (Quad e1 e2 e3 e4) =
    collectX . fmap fixDegen . collectY . Data.List.transpose $ weaveMeshList
    [ [ lerp y xT xB | x <- xs, let xT = lehT x, let xB = lehB x ] | y <- ys ]
    where
      lehB x = lerp x e2 e1
      lehT x = lerp x e3 e4
      collectX = if length xs > 2 then meshSeq else head
      collectY  = if length ys > 2 then fmap meshSeq else concat

  subdiv v m = over faces (subdiv v) m


instance Trans a => Subdiv (Solid a) a where
  type SubdivT (Solid a) = [Solid a]

  subdiv vs EmptySolid =
    (\(TransformedSolid _ t) -> TransformedSolid EmptySolid t) <$> subdiv vs (Cube 1)
  subdiv vs (Cube s) = subdiv vs (CubeS $ pure s)

  subdiv (V3 xs ys zs) (CubeS (V3 a b c)) = translate (-0.5 * V3 a b c) <$>
    [ TransformedSolid (CubeS v) (translateT $ V3 xi yi zi + 0.5 *^ v)
    | (xi, xe) <- zip <$> init <*> tail $ xs ^* a,
      (yi, ye) <- zip <$> init <*> tail $ ys ^* b,
      (zi, ze) <- zip <$> init <*> tail $ zs ^* c,
      let v = V3 (xe - xi) (ye - yi) (ze - zi) ]

  subdiv (V3 xs ys zs) p@Pyramid{} =
    let py = extent (V3 0 1 0) p
        p' = (\s -> translateY py . scaleU (1-s) $ translateY (-py) p) <$> ys
        p'' = p' ^.. folded.to solidBottom.to (over (_localSpace._local) $ rotateX pi)
        aas = subdiv (V3 xs zs []) <$> p'' -- :: [[Shape a]]

        f a11 a22 = zipWith Extrude a11 a22
        aas' = zipF (repeat f) aas
    in toListOf (folded.folded) aas'

  subdiv (V3 xs ys zs) p@Cone{} =
    let py = extent (V3 0 1 0) p
        p' = (\s -> translateY py . scaleU (1-s) $ translateY (-py) p) <$> ys
        p'' = p' ^.. folded.to solidBottom.to (over (_localSpace._local) $ rotateX pi)
        aas = subdiv (V3 xs zs []) <$> p'' -- :: [[Shape a]]

        f a11 a22 = zipWith Extrude a11 a22
        aas' = zipF (repeat f) aas
    in toListOf (folded.folded) aas'

  -- this is nonsense
  -- subdiv (V3 xs ys zs) (Extrude sh1 sh2) =
  --   let
  --     slicef s1 s2 = zipWith Extrude (subdiv (V3 xs zs []) s1) (subdiv (V3 xs zs []) s2)
  --     trs = tlerp <$> ys <*> [sh2 ^. _localSpace] <*> [sh1 ^. _localSpace]
  --   in case sh1 of
  --     (Point _) -> concat $ zipF (repeat slicef) (sh1 : tail (_localSpace (const trs) sh2))
  --     _ -> concat $ zipF (repeat slicef) (init (_localSpace (const trs) sh1) ++ [sh2])

  subdiv vs (TransformedSolid s t) = transform t <$> subdiv vs s
  subdiv _ o =  error $ "subdiv for '"  ++ show o ++ "' not yet implemented"


instance Trans a => Subdiv (Shape a) a where
  type instance SubdivT (Shape a) = [Shape a]

  subdiv (V3 xs ys _) (Point t) = Point t <$ drop 1 xs <* drop 1 ys
  subdiv (V3 xs ys _) (EmptyShape t) = EmptyShape t <$ drop 1 xs <* drop 1 ys

  subdiv (V3 xs ys _) (Circle r t) =
    [ Wedge r r rs re as ae t | (rs, re) <- zip <$> init <*> tail $ ys,
                                (as, ae) <- zip <$> init <*> tail $ (2 * pi *^ xs) ]

  subdiv (V3 xs ys _) (Ellipse r1 r2 t) =
    [ Wedge r1 r2 rs re as ae t | (rs, re) <- zip <$> init <*> tail $ ys,
                                  (as, ae) <- zip <$> init <*> tail $ (2 * pi *^ xs)]

  subdiv (V3 xs ys _) (Wedge r1 r2 rs re as ae t) =
    [ Wedge r1 r2 (lerp rs' (const re) (const rs) ())
                  (lerp re' (const re) (const rs) ())
                  (lerp as' (const ae) (const as) ())
                  (lerp ae' (const ae) (const as) ()) t
    | (rs', re') <- zip <$> init <*> tail $ ys,
      (as', ae') <- zip <$> init <*> tail $ xs ]

  subdiv (V3 xs ys _) (Rectangle a b t) = transform t . translate (-0.5 * V3 a b 0) <$>
    [ Rectangle a' b' (translateT $ V3 xi yi 0 + 0.5 * V3 a' b' 0)
    | (xi, xe) <- zip <$> init <*> tail $ a *^ xs,
      (yi, ye) <- zip <$> init <*> tail $ b *^ ys,
      let a' = xe - xi, let b' = ye - yi ]

  subdiv v (ClosedShape s) = subdiv v s
  subdiv _ o =  error $ "subdiv for '"  ++ show o ++ "' not yet implemented"


subdiv' :: (Enum b, Fractional b, Subdiv a b, s ~ SubdivT a) => Int -> Int -> Int -> a -> s
subdiv' x y z = subdiv (sbd x y z)

sbd :: (Enum a, Fractional a) => Int -> Int -> Int -> V3 [a]
sbd x y z =
  let f a = let step = (1 / fromIntegral a) in take (a + 1) [0, step..]
  in f <$> V3 x y z
