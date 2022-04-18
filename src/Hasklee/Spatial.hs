module Hasklee.Spatial
  ( Extent(..)
  , Spatial(..)
  , OABB(..), oabb, eoabb
  , extentX, extentY, extentZ, extents
  , extentR
  , radius
  , juxt
  , xx, yy, zz
  , catX, catY, catZ, catS
  , repeatX, repeatY, repeatZ
  , replicateX, replicateY, replicateZ, replicateS
  , arrayXYZ
  , interspace
  , row, column
  , render
  , rend, rend0, rend1
  , onSurface, ontoSurface
  , centerX, centerY, centerZ
  , levelX, levelY, levelZ
  , levelXm, levelYm, levelZm
  , qI, qII, qIII, qIV
  ) where

import Control.DeepSeq (NFData)
import Data.Default.Class
import Control.Lens hiding (transform)
import Data.Binary (Binary)
import Data.Foldable
import Data.Functor.Compose
import Data.List
import Data.Maybe
import GHC.Generics (Generic)
import Linear hiding (column)

import Hasklee.Mesh
import Hasklee.Transform
import Hasklee.Vertex


class Num a => Extent t a | t -> a where
  type SpatialT t :: *
  type instance SpatialT t = t

  spatial :: t -> Spatial (SpatialT t) a
  default spatial :: (t ~ SpatialT t) => t -> Spatial (SpatialT t) a
  spatial = Spatial

  spaceOf :: t -> Spatial b a
  default spaceOf ::  t -> Spatial b a
  spaceOf = Space . extents

  extent :: V3 a -> t -> a

  extentF :: V3 a -> t -> a
  extentF v x = extent v x + extent (-v) x


data Spatial t a = Spatial t
                 | SpaceOf t
                 | Space (V3 a)
                 -- Repeat is only valid outermost and has no extent implementation
                 | Repeat (V3 a) (Spatial t a)
                 | Juxt (V3 a) (Spatial t a) (Spatial t a)
                 deriving Show

type SpatialTT t = Spatial (SpatialT t)

data OABB a = OABB (V3 a) (V3 a)
            deriving (Binary, Generic, NFData, Show)

instance (Fractional a, Ord a, HasCenter t a) => HasCenter (Spatial t a) a where
  _center g (Spatial t) = Spatial <$> _center g t
  _center g (SpaceOf t) = SpaceOf <$> _center g t
  _center g (Space t) = Space <$> _center g t
  _center g (Repeat v t) = Repeat v t <$ g zero
  _center g (Juxt v t1 t2) = uncurry (Juxt v) <$>
    meanOf (both._center) g (t1, t2)

instance Each (Spatial t a) (Spatial t a) t t where
  each f (Spatial t) = Spatial <$> f t
  each f (Juxt v l r) = Juxt v <$> each f l <*> each f r
  each _ n = pure n

instance (Fractional a, Extent t a) => Extent (Spatial t a) a where
  type SpatialT (Spatial t a) = t
  spatial = id
  spaceOf = Space . extents

  extent v (Spatial x) = extent v x
  extent v (SpaceOf x) = extent v x
  extent v (Space s) = abs $ dot v s * 0.5
  extent v Repeat{} = error "Repeat has no extent."
  extent v (Juxt vx s1 s2) = 0.5 * extentF v (Juxt vx s1 s2)

  extentF v (Spatial x) = extent v x + extent (-v) x
  extentF v (SpaceOf x) = extent v x + extent (-v) x
  extentF v (Space s) = abs $ dot v s
  extentF v Repeat{} = error "Repeat has no extent."
  extentF v (Juxt vx s1 s2) = extentF v s1 + (extentF v s2 * abs(dot v vx))

instance (Epsilon a, Ord a, Floating a) => Extent (Mesh a) a where
  extent _ EmptyMesh = 0
  extent v m = fromMaybe 0 . maximumOf (vertices._pos.to (dot v)) $ m

instance (Ord a, Num a) => Extent (OABB a) a where
  extent v (OABB a b) = max (dot v a) (dot v b)

instance (Ord a, Num a, Extent (t a) a) => Extent [t a] a where
  extent v = maximum . fmap (extent v)

instance Trans b => Extent (Transform b) b where
  extent v t = dot v (t ^. _pos)

instance Functor OABB where
  fmap f (OABB a b) = OABB (fmap f a) (fmap f b)

instance (Trans a, Transformable t a) => Transformable (Spatial t a) a where
  transform t (Spatial x) = Spatial (transform t x)
  transform t (SpaceOf x) = SpaceOf (transform t x)
  transform t (Space x) = Space (transform t x)
  transform t (Repeat v x) = Repeat v (transform t x)
  --scaling is prolly wrong here. need to take v into account somehow
  transform t (Juxt v x1 x2) = Juxt v (transform t x1) (transform t x2)

instance Trans a => Transformable (OABB a) a where
  transform t (OABB a b) = OABB (transform t a) (transform t b)


extents :: Extent t a => t -> V3 a
extents a = flip extentF a <$> V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

extentX, extentY, extentZ :: Extent t a => t -> a
extentX = extentF (V3 1 0 0)
extentY = extentF (V3 0 1 0)
extentZ = extentF (V3 0 0 1)

extentR :: (Trans a, HasLocalSpace t a, Extent s a) => V3 a -> t -> s -> a
extentR v o s = dot v (t ^. _pos) + extent v' s * scaleFactor
    where
      t = o ^. _localSpace
      v' = normalize $ (Linear.transpose . view _m33 . toM44 $ t) !* v
      scaleFactor = norm $ (view _m33 . toM44 $ t) !* v

radius :: (Floating a, Ord a, ToVertices t a, Default (VertexOptions t)) => t -> a
radius = maybe 0 sqrt . maximumOf (to toVertices.folded._pos.to quadrance)

juxt :: (Extent t a, Extent s a, SpatialT t ~ r, SpatialT s ~ r)
     => V3 a -> t -> s -> Spatial r a
juxt v t s = Juxt v (spatial t) (spatial s)

xx, yy, zz :: (Extent t a, Extent s a, SpatialT t ~ r, SpatialT s ~ r)
           => t -> s -> Spatial r a
xx a b = Juxt (V3 1 0 0) (spatial a) (spatial b)
yy a b = Juxt (V3 0 1 0) (spatial a) (spatial b)
zz a b = Juxt (V3 0 0 1) (spatial a) (spatial b)

catX, catY, catZ :: (Num a, Extent t a) => [t] -> SpatialTT t a
catX = catS (V3 1 0 0) . fmap spatial
catY = catS (V3 0 1 0) . fmap spatial
catZ = catS (V3 0 0 1) . fmap spatial

catS :: Num a => V3 a -> [Spatial t a] -> Spatial t a
catS _ [] = Space (pure 0)
catS v ls = foldr1 (Juxt v) ls

repeatX, repeatY, repeatZ :: (Num a, Extent t a) => t -> SpatialTT t a
repeatX a = catX (repeat a)
repeatY a = catY (repeat a)
repeatZ a = catZ (repeat a)

replicateS :: (Num a, Extent t a) => V3 a -> Int -> t -> SpatialTT t a
replicateS v n = catS v . fmap spatial . replicate n

replicateX, replicateY, replicateZ
  :: (Num a, Extent t a)
  => Int -> t -> SpatialTT t a
replicateX n a = catX (replicate n a)
replicateY n a = catY (replicate n a)
replicateZ n a = catZ (replicate n a)


interspace :: (Num a, Foldable k, Extent t a, Extent s a)
           => s -> k t -> [SpatialTT t a]
interspace s k = intersperse (spaceOf s) $ spatial <$> toList k

row :: (Fractional a, Foldable k, Extent t a, Extent s a, Extent (SpatialT t) a)
    => s -> k t -> SpatialTT t a
row s = catX . interspace s

column :: (Fractional a, Foldable k, Extent t a, Extent s a, Extent (SpatialT t) a)
       => s -> k t -> SpatialTT t a
column s = catY . interspace s

arrayXYZ :: (Trans a, Extent t a, Extent s a, Transformable t a) => V3 Int -> s -> t -> [t]
arrayXYZ (V3 nx ny nz) spa t =
  [translate (v + sv) t | x <- [0,1..nx-1], y <- [0,1..ny-1], z <- [0,1..nz-1],
    let v = (*) . fromIntegral <$> V3 x y z <*> e,
    let sv = (*) . fromIntegral <$> V3 x y z <*> extents spa]
  where e = extents t

render :: (Trans a, Extent t a, Extent s a, Transformable t a)
       => V3 Bool -> Bool -> Spatial t a -> s -> [t]
render fv' aspectR (Repeat v s) t =
  let eT = Just <$> extents t
      fv = if or fv' then fv' else pure True
      fb = (\x -> if x then Just () else Nothing) <$> fv
      eS = (\x -> if x == 0 then Nothing else Just x) <$> extentF v s *^ v
      eS' = getCompose $
            (\_ b c -> b / c) <$> Compose fb <*> Compose eT <*> Compose eS
      eS'' = (maybe 1 floor $ minimumOf (each. _Just) eS')
  in render fv' aspectR (catS v (replicate eS'' s)) t
render fv aspectR s t =
  let eT = Just <$> extents t
      fb = (\x -> if x then Just () else Nothing) <$> fv
      eS = (\x -> if x == 0 then Nothing else Just x) <$> extents s
      eS' = getCompose $
            (\_ b c -> b / c) <$> Compose fb <*> Compose eT <*> Compose eS
      eS'' = if aspectR then pure (fromMaybe 1 $ minimumOf (each. _Just) eS')
             else fromMaybe 1 <$> eS'
  in rend zero (scale eS'' s)

onSurface :: (Trans a, Monoid t, Extent t a, Transformable t a, Extent s a, HasLocalSpace s a)
          => Spatial t a -> s -> t
onSurface t s =
  let tr = set _pos zero $ view _localSpace s
  in translate (s ^. _localSpace._pos) . mconcat $
     render (V3 False False False) True (transform tr t) s

ontoSurface :: (Trans a, Monoid t, Extent t a, Transformable t a, Extent s a, HasLocalSpace s a)
            => Spatial t a -> s -> t
ontoSurface t s =
  let tr = set _pos zero $ view _localSpace s
  in translate (s ^. _localSpace._pos) . mconcat $
     render (V3 True True False) True (transform tr t) s


centerX, centerY, centerZ :: (Num a, HasCenter t a) => t -> t
centerX = set (_center._x) 0
centerY = set (_center._y) 0
centerZ = set (_center._z) 0

levelX, levelY, levelZ :: (Trans a, Extent t a, Transformable t a) => t -> t
levelX = levelv (V3 1 0 0)
levelY = levelv (V3 0 1 0)
levelZ = levelv (V3 0 0 1)

levelXm, levelYm, levelZm :: (Trans a, Extent t a, Transformable t a) => t -> t
levelXm = levelv (V3 (-1) 0 0)
levelYm = levelv (V3 0 (-1) 0)
levelZm = levelv (V3 0 0 (-1))

levelv :: (Trans a, Extent t a, Transformable t a) => V3 a -> t -> t
levelv v t =
  let n = normalize v
      x = extent (negate n) t
  in translate (n ^* x) t

qI, qII, qIII, qIV :: (Trans a, Extent t a, Transformable t a) => t -> t
qI = levelX . levelY
qII = levelXm . levelY
qIII = levelXm . levelYm
qIV = levelX . levelYm


rend :: (Trans a, Ord a, Extent t a, Transformable t a)
     => V3 a -> Spatial t a -> [t]
rend o (Spatial x) = [translate o x]
rend _ Space{} = []
rend _ SpaceOf{} = []
rend _ Repeat{} = error ""
rend o (Juxt v s1 s2) = rend o1 s1 ++ rend o2 s2
  where
    fu = extentF v s1 + extentF v s2
    oo1 = extent (-v) s1 - 0.5 * fu
    oo2 = extent (-v) s2 + extentF v s1 - 0.5 * fu
    o1 = o ^+^ oo1 *^ v
    o2 = o ^+^ oo2 *^ v

rend0 :: (Trans a, Ord a, Extent t a, Transformable t a) => Spatial t a -> [t]
rend0 = rend zero

rend1 :: (Trans a, Ord a, Extent t a, Transformable t a, HasLocalSpace t a) => Spatial t a -> [t]
rend1 s =
  let (ol, os) = singular each._localSpace._pos <<.~ zero $ s
      ee = rend0 os
      on = ee ^. singular each._localSpace._pos
  in translate (ol ^-^ on) <$> ee

eoabb :: Extent t a => t -> OABB a
eoabb t = OABB
          (negate $ V3 (extent (V3 (-1) 0 0) t) (extent (V3 0 (-1) 0) t) (extent (V3 0 0 (-1)) t))
          (V3 (extent (V3 1 0 0) t) (extent (V3 0 1 0) t) (extent (V3 0 0 1) t))

oabb :: (Num a, Ord a, Foldable t, HasPosition b a) => t b -> OABB a
oabb t = oabb' (toList t)
  where
    oabb' [] = OABB zero zero
    oabb' l = OABB vMin vMax
      where
        vInit = view _pos $ head l
        (vMin, vMax) = foldl' k (vInit, vInit) t
        k (!vMin, !vMax) v = let v' = view _pos v
                             in (min <$> vMin <*> v', max <$> vMax <*> v')
