module Hasklee.Transform
  ( Trans
  , Transform(..)
  , Transformable(..)
  , HasLocalSpace(..)
  , Local(..)
  , Handed(..)
  , ToDiscretePath(..)
  , V4Iso(..), V3Iso(..), V2Iso(..)
  , M44Iso(..), M33Iso(..)
  , translateX, translateY, translateZ
  , rotateX, rotateY, rotateZ
  , scaleX, scaleY, scaleZ, scaleU
  , originT, transformT, matrixT
  , translateT, rotateT, scaleT
  , aroundAxisT
  , planeZX
  , translateTBN, transformTBN
  , surfaceScale
  , inv
  , toTRS, toM44
  , timedT
  , (>|), (|<)
  , time, transformTo
  , atOrigin
  , lookAlong0, lookAlongT
  , unangle3, rotFromTo
  , discretePath, continuousPath
  , followPath
  , local, overSpaceOf, overSpaceOf', inSpaceOf
  , copyTo, copyToM
  , tr
  , anyPerp
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Lens hiding (transform)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State
import Data.Binary (Binary)
import Data.Default.Class
import Data.Hashable
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Linear hiding (rotate)
import qualified Linear (rotate)
import Numeric.AD as AD
import Numeric.AD.Rank1.Tower
import System.Random (Random)

import Hasklee.Mesh
import Hasklee.Vertex


class (Conjugate a, Enum a, Epsilon a, Hashable a, NFData a,
       Ord a, Random a, RealFloat a, Show a) => Trans a

instance Trans Float
instance Trans Double


data Transform a = Origin
                 | Transform (V3 a) (Quaternion a) (V3 a)
                 | TransformI (Transform a) (Transform a)
                 | Matrix (M44 a)
                 | ComposeT (Transform a) (Transform a)
                 | TimedT (Map.Map a (Transform a))
                 deriving (Binary, Eq, Generic, Ord, NFData, Show)


class Transformable t a | t -> a where
  transform :: Transform a -> t -> t

  transform1 :: HasLocalSpace s a => s -> t -> t
  transform1 = transform . view _localSpace

  rotate :: Trans a => Quaternion a -> t -> t
  rotate = transform . rotateT

  scale :: Trans a => V3 a -> t -> t
  scale = transform . scaleT

  translate :: Trans a => V3 a -> t -> t
  translate = transform . translateT

class HasLocalSpace t a | t -> a where
  _localSpace :: Lens' t (Transform a)

class Local t where
  _local :: Traversal' t t

class Handed t where
  changeHand :: t -> t

class ToDiscretePath t a | t -> a where
  dpath :: t -> [Transform a]

class V4Iso s a where
  v4 :: Iso' (s a) (V4 a)

class V3Iso s a where
  v3 :: Iso' (s a) (V3 a)

class V2Iso s a where
  v2 :: Iso' (s a) (V2 a)

class M44Iso s a where
  m44 :: Iso' (s a) (M44 a)

class M33Iso s a where
  m33 :: Iso' (s a) (M44 a)


instance Ord a => Semigroup (Transform a) where
  (<>) = mappend

instance Ord a => Monoid (Transform a) where
  mempty = Origin
  mappend Origin r = r
  mappend l Origin = l
  mappend (TimedT t1) (TimedT t2) = TimedT (Map.unionWith mappend t1 t2)
  mappend (TimedT t1) t = TimedT $ Map.updateMax (Just . (<> t)) t1
  mappend t (TimedT t1) = TimedT $ Map.updateMax (Just . (t <>)) t1
  mappend a b = ComposeT a b

instance Functor Transform where
  fmap _ Origin = Origin
  fmap f (Transform t r s) = Transform (fmap f t) (fmap f r) (fmap f s)
  fmap f (TransformI t1 t2) = TransformI (fmap f t1) (fmap f t2)
  fmap f (Matrix m) = Matrix (fmap (fmap f) m)
  fmap f (ComposeT a b) = ComposeT (fmap f a) (fmap f b)
  -- this does not make much sense
  fmap f (TimedT m) = TimedT (Map.mapKeysMonotonic f . Map.map (fmap f) $ m)

instance Ord a => Transformable (Transform a) a where
  transform = mappend

instance HasLocalSpace (Transform a) a where
  _localSpace = id

instance Trans a => Local (Transform a) where
  _local g t = (t <>) <$> g originT

instance Trans a => HasTBN (Transform a) a where
  _TBN g (Matrix m) = Matrix <$> _m33 g m
  _TBN g t = matrixT <$> _m33 g (toM44 t)

instance Trans a => HasPosition (Transform a) a where
  _pos g Origin = translateT <$> g zero
  _pos g (Transform p r s) = (\x -> Transform x r s) <$> g p
  _pos g (TransformI t _) = transformI <$> _pos g t
  _pos g (Matrix m) = matrixT <$> (column _w._xyz) g m
  _pos g t = _pos g (matrixT . toM44 $ t)

instance Trans a => HasNormal (Transform a) a where
  _n = _TBN._n

instance Trans a => HasTangent (Transform a) a where
  _tn = _TBN._tn

instance Trans a => HasBitangent (Transform a) a where
  _btn = _TBN._btn

instance Transformable (Const a b) b where
  transform _ c = c

instance (Trans a, Transformable (b a) a, Transformable (c a) a) =>
         Transformable (b a, c a) a where
  transform t (b, c) = (transform t b, transform t c)

instance Trans a => Transformable (b -> V3 a) a where
  transform t = fmap (transform t)

instance (Transformable (t a) a) => Transformable [t a] a where
  transform t = fmap (transform t)

instance Trans a => Transformable (V3 a) a where
  transform t = over vertices (transform t)

instance Trans a => Transformable (Vertex a) a where
  transform Origin v = v
  transform (TransformI t1 t2) v =
    transformVertex (toM44 t1) (view _m33 $ toM44 t2) v
  transform (Transform t r s) (Vertex3 v) =
    Vertex3 (Linear.rotate r (s*v) ^+^ t)
  transform (Matrix m) v =
    transformVertex m (Linear.transpose . inv33 . view _m33 $ m) v
  transform t v = transform (matrixI t) v

instance Trans a => Transformable (Edge (Vertex a)) a where
  transform = fmap . transform

instance Trans a => Transformable (Mesh a) a where
  transform Origin m = m
  transform (TransformI t1 t2) m =
    vmap (transformVertex (toM44 t1) (view _m33 $ toM44 t2)) m
  transform t m = transform (matrixI t) m

instance Trans a => HasLocalSpace (Vertex a) a where
  _localSpace g v = (\t -> set _TBN (t ^. _TBN) . set _pos (t ^. _pos) $ v) <$>
                    g (matrixT $ mkTransformationMat (v ^. _TBN) (v ^. _pos))


instance (Trans a, HasPosition (t a) a,
          Transformable (t a) a) => HasLocalSpace [t a] a where
  _localSpace g v@(v1:v2:v3:_) =
    let pos = view (meanOf (traversed._pos)) v
        ls = matrixT $ mkTransformationMat (triangleSpace v1 v2 v3) pos
    in (\x -> transform (x <> inv ls) v) <$> g ls
  _localSpace g v =
    let pos = view (meanOf (traversed._pos)) v
        ls = matrixT $ mkTransformationMat identity pos
    in (\x -> transform (x <> inv ls) v) <$> g ls

instance Trans a => HasLocalSpace (Mesh a) a where
  _localSpace g m@Triangle{} =
    let ls = matrixT $ mkTransformationMat (faceSpace m) (m ^. _center._pos)
    in (\x -> transform (x <> inv ls) m) <$> g ls
  _localSpace g m@Quad{} =
    let ls = matrixT $ mkTransformationMat (faceSpace m) (m ^. _center._pos)
    in (\x -> transform (x <> inv ls) m) <$> g ls
  _localSpace g (Closed m) = Closed <$> _localSpace g m
  -- ???
  _localSpace g (FlipSided a) = _localSpace g (reverseMesh a)
  _localSpace g EmptyMesh = EmptyMesh <$ g Origin
  _localSpace g m = (\t -> _center._pos .~ (t ^. _pos) $ m) <$>
                    g (translateT (m ^. _center._pos))

instance Trans a => HasLocalSpace (Edge (Vertex a)) a where
  _localSpace g e@(Edge a b) =
    let ap = view _pos a
        bp = view _pos b
        o = lerp 0.5 bp ap
        n = a ^. _n
        tn = normalize (bp ^-^ ap)
        btn = cross n tn
        ls = matrixT $ mkTransformationMat (transpose $ V3 tn btn n) o
    in (\x -> transform (x <> inv ls) e) <$> g ls

instance Trans a => M44Iso Transform a where
  m44 = iso to' from'
    where
      to' = toM44
      from' = Matrix

instance Num a => Handed (V3 a) where
  changeHand (V3 x y z) = V3 x y (negate z)

instance Num a => Handed (V4 a) where
  changeHand (V4 x y z w) = V4 x y (negate z) w

instance Num a => Handed (M44 a) where
  changeHand m =
    let k = identity & column _z._z %~ negate
    in k !*! m !*! k

-- and the rest?
-- instance Num a => Handed (Vertex a) where
--   changeHand v = v & _pos._3 %~ negate & _n._3 %~ negate

instance Num a => Handed (Vertex a) where
  changeHand v@Vertex3{} = v & _pos._3 %~ negate
  changeHand v@Vertex9{} = v & _pos._3 %~ negate & _n._3 %~ negate & _tn._3 %~ negate
  changeHand v@Vertex18{} = v & _pos._3 %~ negate & _n._3 %~ negate & _tn._3 %~ negate
                            & _btn._3 %~ negate

instance Num a => Handed (Mesh a) where
  changeHand m = rewiseMesh (vmap changeHand m)

instance Trans a => Handed (Transform a) where
  changeHand (Matrix m) =
    let k = identity & column _z._z %~ negate
    in Matrix $ k !*! m !*! k
  changeHand m = changeHand . Matrix . toM44 $ m

instance Trans a => ToDiscretePath [V3 a] a where
  dpath = discretePath

instance ToDiscretePath [Transform a] a where
  dpath = id


translateX, translateY, translateZ ::
  (Trans a, Transformable t a) => a -> t -> t
translateX x = translate (V3 x 0 0)
translateY y = translate (V3 0 y 0)
translateZ z = translate (V3 0 0 z)

rotateX, rotateY, rotateZ ::
  (Trans a, Transformable t a) => a -> t -> t
rotateX an = rotate $ axisAngle (V3 1 0 0) an
rotateY an = rotate $ axisAngle (V3 0 1 0) an
rotateZ an = rotate $ axisAngle (V3 0 0 1) an

scaleX, scaleY, scaleZ, scaleU ::
  (Trans a, Transformable t a) => a -> t -> t
scaleX d = scale (V3 d 1 1)
scaleY d = scale (V3 1 d 1)
scaleZ d = scale (V3 1 1 d)
scaleU s = scale (pure s)


originT :: Trans a => Transform a
originT = transformT zero (Quaternion 1 zero) (pure 1)

transformT :: Trans a => V3 a -> Quaternion a -> V3 a -> Transform a
transformT t r s = transformI $ Transform t r s

matrixT :: Trans a => M44 a -> Transform a
matrixT = transformI . Matrix

translateT :: Trans a => V3 a -> Transform a
translateT v = transformT v (Quaternion 1 zero) (pure 1)

rotateT :: Trans a => Quaternion a -> Transform a
rotateT q = transformT zero q (pure 1)

scaleT :: Trans a => V3 a -> Transform a
scaleT = transformT zero (Quaternion 1 zero)

aroundAxisT :: Trans a => V3 a -> V3 a -> a -> Transform a
aroundAxisT o ax an = translateT o <> rotateT (axisAngle ax an) <> translateT (-o)

planeZX :: Trans a => Transform a
planeZX = rotateX (pi*0.5) Origin


translateTBN :: (Trans a, HasVertices t a) => V3 a -> t -> t
translateTBN t = over vertices
  (\x -> transform (translateT $ view _TBN x !* t) x)

transformTBN :: (Trans a, HasVertices t a) => Transform a -> t -> t
transformTBN t = over vertices f
  where
    f x =
      let tbnm = matrixT $ mkTransformationMat (view _TBN x) zero
      in transform (tbnm <> t <> inv tbnm) x

surfaceScale :: (Trans a, FaceSpace t a, HasCenter t a, HasVertices t a) =>
                V2 a -> t -> t
surfaceScale (V2 x z) m = over vertices (tranF (faceSpace m)) m
  where
    c = view _center m
    tranF fs v = translate c .
                 set _pos npos $ v
      where tv = translate (-c) v
            dy = dot (view _pos tv) (view _n fs)
            dx = dot (view _pos tv) (view _tn fs)
            btn2 = cross (view _tn fs) (view _n fs)
            dz = dot (view _pos tv) btn2
            npos = (dx * x *^ view _tn fs) +
                   (dz * z *^ btn2) +
                   (dy * 1 *^ view _n fs)


inv :: Trans a => Transform a -> Transform a
inv = matrixT . inv44 . toM44

toTRS :: Trans a => Transform a -> Maybe (Transform a)
toTRS Origin = Just originT
toTRS t@Transform{} = Just t
toTRS (TransformI t _) = toTRS t
toTRS (ComposeT l r) = f <$> toTRS l <*> toTRS r
  where
    f (Transform t1 r1 s1) (Transform t2 r2 s2) =
      transformT (Linear.rotate r1 (t2*s1) ^+^ t1) (r1*r2) (s1*s2)
toTRS (TimedT t) = toTRS $ snd $ Map.findMax t
toTRS _ = Nothing

toM44 :: Num a => Transform a -> M44 a
toM44 Origin = identity
toM44 (Transform v q (V3 xs ys zs)) =
  mkTransformation q v !*! (identity & _x._x *~ xs & _y._y *~ ys & _z._z *~ zs)
toM44 (TransformI t _) = toM44 t
toM44 (Matrix m) = m
toM44 (ComposeT t t1) = toM44 t !*! toM44 t1
toM44 (TimedT t) = toM44 $ snd $ Map.findMax t


invT :: Trans a => Transform a -> Transform a
invT = Matrix . over _m33 Linear.transpose . inv44 . toM44

matrixI :: Trans a => Transform a -> Transform a
matrixI t = transformI (Matrix $ toM44 t)

transformI :: Trans a => Transform a -> Transform a
transformI t@TransformI{} = t
transformI t = TransformI t (invT t)


timedT :: a -> Transform a -> Transform a
timedT a t = TimedT $ Map.singleton a t

timed :: (Num a, HasLocalSpace t a) => t -> t
timed = over _localSpace timed'
  where
    timed' t@(TimedT _) = t
    timed' t = timedT 0 t

(>|) :: (Trans a, HasLocalSpace t a) => t -> (t -> t) -> t
(>|) t f = f (timed t)

(|<) :: (Trans a, HasLocalSpace t a) => (t -> t) -> t -> t
(|<) = flip (>|)

time :: (Trans a, HasLocalSpace t a) => a -> t -> t
time a o =
  let (TimedT r) = view _localSpace (timed o)
      (time1, t1) = Map.findMax r
  in set _localSpace (TimedT r <> TimedT (Map.singleton (time1 + a) (view _localSpace t1))) o

transformTo :: (HasLocalSpace t a, HasLocalSpace s a) => s -> t -> t
transformTo s = over _localSpace f
  where
    f (TimedT l) = TimedT $ Map.updateMax (Just . const (view _localSpace s)) l
    f _ = view _localSpace s


atOrigin :: (Trans a, HasLocalSpace t a) => t -> t
atOrigin = set (_localSpace._pos) zero

anyPerp :: Num a => V3 a -> V3 a
anyPerp (V3 x y z) = cross (V3 x y z) (V3 y z x)


lookAlong0 :: Trans a => V3 a -> Transform a
lookAlong0 = lookAlongT (V3 0 0 1)

lookAlongT :: Trans a => V3 a -> V3 a -> Transform a
lookAlongT v n = matrixT $ mkTransformation quat zero
  where
    (n', v') = (normalize n, normalize v)
    -- a = cross v' n'
    -- d = dot v' n'
    -- -- ang = (\dd a -> let an = asin (min 1 $ norm a)
    -- --                 in if dd >= 0 then an
    -- --                    else pi*0.5 + (pi*0.5 - an))
    -- --       d axis
    -- axis = if nearZero (norm a) && (d < 0) then anyPerp n' else a
    -- ang = unangle3 axis d
    -- ang' = if nearZero (norm a) && (d < 0) then pi else ang
    -- quat = axisAngle axis ang'
    quat = rotFromTo v n

unangle3 :: (Epsilon a, Floating a, Ord a) => V3 a -> a -> a
unangle3 cross' dot' =
  let an = asin (min 1 $ norm cross')
  in if dot' >= 0
       then an
       else pi - an

                      -- in if nearZero (norm a) && (d < 0) then (anyPerp n0, pi) else (a, ang)

  -- axisAngle from to  !!!!!!!!!!!!!!!!!!!!!!
-- axisAngle' :: (Epsilon a, Floating a) => V3 a -> a -> Quaternion a
-- axisAngle' axis theta
--   | nearZero theta = Quaternion 1 zero
--   | nearZero axis = axisAngle (anyPerp axis) pi
--   | otherwise = axisAngle axis theta

rotFromTo :: (Epsilon a, Floating a, Ord a) => V3 a -> V3 a -> Quaternion a
rotFromTo v1 v2 =
  let axis = cross v1 v2
      theta = unangle3 axis (dot v1 v2)
  in if nearZero theta
       then Quaternion 1 zero
       else if nearZero axis
               then axisAngle (anyPerp v1) pi
               else axisAngle axis theta


discretePath :: Trans a => [V3 a] -> [Transform a]
discretePath ls' = matrixT <$> zipWith mkTransformation quats2 ls
  where ls = reverse $ foldl (\b a -> if nearZero (head b ^-^ a) then b else a:b)
             [head ls'] ls'
        tns = normalize <$> zipWith (^-^) (tail ls) (init ls)
        -- axes = zipWith cross (V3 0 0 1 : tns) tns
        -- dots = zipWith dot (V3 0 0 1 : tns) tns
        -- angles = zipWith
                 -- (\d a -> let an = asin (min 1 $ norm a)
                 --          in if d >= 0 then an
                 --             else pi*0.5 + (pi*0.5 - an))
                 -- dots axes
        -- angles = zipWith unangle3 axes dots
        -- quats = zipWith
        --         (\ax an -> if nearZero ax || nearZero an then Quaternion 1 zero
        --                    else axisAngle ax an)
        --   axes angles ++ [Quaternion 1 zero]
        quats = zipWith rotFromTo (V3 0 0 1 : tns) tns ++ [Quaternion 1 zero]
        quats2 = drop 1 $ scanl (\x y -> normalize $ y * x) (Quaternion 1 zero) quats

continuousPath :: Trans a => (forall s. AD s (Tower a) -> V3 (AD s (Tower a))) -> a -> Transform a
continuousPath f a = Matrix identity
               & _pos .~ pos
               & _tn .~ tn'
               & _n .~ n'
               & _btn .~ cross n' tn'
  where derivs = take 3 . (\x -> x ++ repeat 0) <$> AD.diffsF f a
        [pos, tn, n] = getZipList $ traverse ZipList derivs
        tn' = normalize tn
        n' = normalize n


followPath :: (Trans a, Foldable t, HasLocalSpace s a) => a -> t s -> Transform a
followPath t p = evalState (foldlMOf (folded._localSpace) foldF Origin p) 0
  where foldF b a = do
          i <- get
          put (i+t)
          return $ timedT i a <> b


type Space a = Reader.Reader (Transform a)

local :: (Trans r, Transformable a r) => a -> Space r a
local x = Reader.reader (\t -> transform (inv t) x )

overSpaceOf :: (Trans r, HasLocalSpace s r, Transformable a r) => Space r a -> s -> a
overSpaceOf s b = Reader.runReader (s >>= \x -> Reader.reader (\t -> transform t x)) (b ^. _localSpace)

overSpaceOf' :: (Trans r, HasLocalSpace s r) => Space r a -> s -> a
overSpaceOf' s b = Reader.runReader s (b ^. _localSpace)

inSpaceOf :: (Trans r, Transformable a r, HasLocalSpace s r) => a -> s -> a
inSpaceOf b s = local b `overSpaceOf'` s


copyTo :: (Trans a, Transformable t a, ToVertices s a, Default (VertexOptions s))
       => s -> t -> [t]
copyTo s t = flip transform1 t <$> s ^.. to toVertices.folded._localSpace

copyTo2 :: (Trans a, HasLocalSpace t a, ToVertices s a, Default (VertexOptions s))
       => s -> t -> [t]
copyTo2 s t =
  flip copyTo' t <$> s ^.. to toVertices.folded
  where
    copyTo' (Vertex3 v) = set (_localSpace._pos) v

copyToM :: (Monad m, Trans a, Transformable t a, ToVertices s a, Default (VertexOptions s)) =>
           s -> m t -> m [t]
copyToM s t = mapM (\x -> transform1 x <$> t) (s ^.. to toVertices.folded._localSpace)


tr :: (Num a, Fractional a) => (a, a) -> (a, a) -> a -> a
tr (s1, e1) (s2, e2) p = (p - s1) * ((e2 - s2) / (e1 - s1)) + s2
