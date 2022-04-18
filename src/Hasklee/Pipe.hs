{-# LANGUAGE DeriveFunctor #-}

module Hasklee.Pipe
  ( Inbound(..)
  , Outbound(..)
  , Normal(..)
  , PipeSegment(..)
  , Pipe(..)
  , PipeMeshOptions(..)
  , pipe
  , pipel
  , pipeMesh
  , pipeMesh8
  , pipeLine, pipeSpline, pipeSpline'
  , fromPositions, fromOrient, fromShape
  , toPositions, toSegments
  , endpoint
  , samplePipe
  , randomizePipe
  ) where

import Control.Lens hiding (transform)
import Data.Default.Class
import Data.Foldable
import Data.List (transpose)
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.VectorSpace as Vs
import Data.Traversable
import qualified Diagrams as D
import Diagrams.Core.V
import Diagrams.Located
import GHC.Generics (Generic)
import Linear
import Linear.Affine
import Math.Spline
import qualified Math.Spline.BSpline as Bs
import qualified Math.Spline.Knots as Ks
import Numeric.AD as AD
import System.Random

import Hasklee
import Hasklee.Coords
import Hasklee.Sample
import Hasklee.Weave


class Inbound t a | t -> a where
  inbound :: t -> Located (Normal a)

class Outbound t a | t -> a where
  outbound :: t -> Located (Normal a)


newtype Normal a = Normal (V3 a)
                 deriving (Eq, Generic, Ord, Show)

type instance N (Normal a) = a
type instance V (Normal a) = V3

instance Wrapped (Normal a)

instance HasNormal (Normal a) a where
  _n = _Wrapped'


data PipeSegment a = PipeLine (V3 a)
                   | PipeSpline [V3 a] (V3 a)
                   deriving (Eq, Functor, Generic, Show)

type instance N (PipeSegment a) = a
type instance V (PipeSegment a) = V3

instance Trans a => HasNormal (PipeSegment a) a where
  _n g (PipeLine a) = (\x -> PipeLine (norm a *^ x)) <$> g (normalize a)
  _n g (PipeSpline as a) = (\x -> PipeSpline as (norm a *^ (last as + x))) <$>
                           g (normalize (a - last as))


newtype Pipe a = Pipe { getPipe :: Seq.Seq (PipeSegment a) }
               deriving (Functor, Generic, Show)

type instance N (Pipe a) = a
type instance V (Pipe a) = V3

instance Wrapped (Pipe a)
instance Rewrapped (Pipe a) (Pipe b)

instance Semigroup (Pipe a) where
  (<>) a b = Pipe (getPipe a >< getPipe b)

instance Monoid (Pipe a) where
  mappend = (<>)
  mempty = Pipe Seq.empty

instance Cons (Pipe a) (Pipe b) (PipeSegment a) (PipeSegment b) where
  _Cons = prism
    (\(a, as) -> Pipe (a Seq.<| getPipe as)) $
    \aas -> case Seq.viewl (getPipe aas) of
              a Seq.:< as -> Right (a, Pipe as)
              Seq.EmptyL -> Left mempty

instance Snoc (Pipe a) (Pipe b) (PipeSegment a) (PipeSegment b) where
  _Snoc = prism
    (\(as, a) -> Pipe (getPipe as Seq.|> a)) $
    \aas -> case Seq.viewr (getPipe aas) of
              as Seq.:> a -> Right (Pipe as, a)
              Seq.EmptyR -> Left mempty

-- is this even a desired behaviour?
instance Num a => Cons (Located (Pipe a)) (Located (Pipe a))
                       (PipeSegment a) (PipeSegment a) where
  _Cons = prism
    (\(a, as) -> over (located._Wrapped) (a Seq.<|) .
                 over (_loc._Point) (subtract (offset a))
                 $ as) $
    \aas -> case Seq.viewl (getPipe (view located aas)) of
              a Seq.:< as -> Right (a, Pipe as `D.at` (view _loc aas + P (offset a)))
              Seq.EmptyL -> Left (Pipe Seq.empty `D.at` view _loc aas)

instance Snoc (Located (Pipe a)) (Located (Pipe a)) (PipeSegment a) (PipeSegment a) where
  _Snoc = prism
    (\(as, a) -> over (located._Wrapped) (Seq.|> a) as) $
    \aas -> case Seq.viewr (getPipe (view located aas)) of
              as Seq.:> a -> Right (Pipe as `D.at` view _loc aas, a)
              Seq.EmptyR -> Left (Pipe Seq.empty `D.at` view _loc aas)

instance Trans a => Outbound (Located (Pipe a)) a where
  outbound (Loc o (Pipe (ps :|> s))) =
    Normal (view _n s) `D.at` P (getSum (foldMap (Sum . offset) ps) + unP o + offset s)
  outbound (Loc o _) = Normal zero `D.at` o

instance Trans a => Outbound (Located (Normal a)) a where
  outbound = id

instance Trans a => Inbound (Located (Pipe a)) a where
  inbound (Loc o (Pipe (PipeLine a :<| _))) = Loc o (Normal (normalize a))
  inbound (Loc o (Pipe (PipeSpline as _ :<| _))) = Loc o (Normal (normalize (head as)))
  inbound (Loc o _) = Normal zero `D.at` o

instance Trans a => Transformable (Located (Pipe a)) a where
  transform t = over (_loc._Point) (transform t)


data PipeMeshOptions a = PipeMeshOptions { pipeRadius :: a,
                                           pipeShape :: a -> [V3 a],
                                           pipeStep :: a
                                         }

instance (Enum a, Floating a) => Default (PipeMeshOptions a) where
  def = PipeMeshOptions 0.08 (\w -> circleL _xy w 5) 0.1

instance Trans a => ToMesh (Pipe a) a where
  type instance MeshOptions (Pipe a) = PipeMeshOptions a
  toMeshI mo p = pipeMesh mo (p `D.at` P zero)


-- orphans

instance (N (t a) ~ a, V (t a) ~ V3) => HasPosition (Located (t a)) a where
  _pos = _loc.lensP

instance (HasNormal (t a) a, N (t a) ~ a, V (t a) ~ V3) => HasNormal (Located (t a)) a where
  _n g (Loc p a) = Loc p <$> _n g a

instance Num a => Vs.AdditiveGroup (V3 a) where
  zeroV = zero
  (^+^) = (^+^)
  negateV = negate

instance Num a => Vs.VectorSpace (V3 a) where
  type Scalar (V3 a) = a
  (*^) = (*^)


pipe :: PipeSegment a -> Pipe a
pipe = Pipe . Seq.singleton

toPositions :: Trans a => a -> Located (Pipe a) -> Seq (V3 a)
toPositions step (Loc o (Pipe p)) = foldl' segToPath (Seq.singleton $ unP o) p
  where
    segToPath (ps :|> s) (PipeLine a) = ps |> s |> (s ^+^ a)
    segToPath (ps :|> s) (PipeSpline v a) =
      ps <> (Bs.evalBSpline (smoothSpline s (s ^+^ a) (fmap (s ^+^) v)) <$> Seq.fromList vv)
      where vv = [0,step..(fromIntegral (length v + 1))]
    segToPath _ _ = undefined

toSegments :: Trans a => Located (Pipe a) -> Seq (Located (PipeSegment a))
toSegments (Loc o (Pipe p)) =
  snd $ mapAccumL (\a b -> (a + offset b, Loc (D.P a) b)) (unP o) p


offset :: PipeSegment a -> V3 a
offset (PipeLine a) = a
offset (PipeSpline _ a) = a

endpoint :: Trans a => Located (PipeSegment a) -> V3 a
endpoint (Loc o p) = unP o + offset p

pipeMesh :: Trans a => PipeMeshOptions a -> Located (Pipe a) -> Mesh a
pipeMesh mo p = pipeWeave (pipeShape mo $ pipeRadius mo) (repeat 1)
                (toList $ toPositions (pipeStep mo) p)

pipeMesh8 :: Trans a => a -> [V3 a] -> [a] -> Located (Pipe a) -> Mesh a
pipeMesh8 sr shape w p = pipeWeave shape w (view _3 <$> samplePipe sr p)


fromPositions :: (Num a, HasPosition t a, Foldable s) => s t -> Located (Pipe a)
fromPositions s =
  let s' = toListOf (folded._pos) s
  in if length s' < 2 then Loc zero $ Pipe Seq.empty
     else Loc (P $ head s') . Pipe . Seq.fromList . fmap PipeLine $
          zipWith (^-^) (tail s') (init s')

fromOrient :: (Trans a, HasPosition t a, HasNormal t a, Foldable s) => s t -> Located (Pipe a)
fromOrient s =
  let s' = toList s
  in if length s < 2 then Loc zero $ Pipe Seq.empty
     else Loc (P . view _pos . head $ s') . Pipe . Seq.fromList $
          zipWith pipeSpline (init s') (tail s')

fromShape :: Trans a => Shape a -> Located (Pipe a)
fromShape shape = fromPositions (ClosedShape shape ^.. to toVertices.folded._pos)


toBSpline :: (Enum a, Ord a, Fractional a) => PipeSegment a -> BSpline V.Vector (V3 a)
toBSpline (PipeSpline as a) =
  let l = fromIntegral $ length as
      ks = Ks.fromList ([(0,3)] ++ zip [1,2..l] (repeat 1) ++ [(l+1,3)])
      vs = V.fromList ([zero]++as++[a])
  in bSpline ks vs

smoothSpline :: (Enum a, Ord a, Fractional a) => V3 a -> V3 a -> [V3 a] -> BSpline V.Vector (V3 a)
smoothSpline s e ps =
  let l = fromIntegral $ length ps
      ks = Ks.fromList ([(0,3)] ++ zip [1,2..l] (repeat 1) ++ [(l+1,3)])
      vs = V.fromList ([s]++ps++[e])
  in bSpline ks vs


pipel :: HasPosition s a => (s -> t -> PipeSegment a) -> (s -> t -> Located (Pipe a))
pipel f s t = pipe (f s t) `D.at` P (s ^. _pos)

pipeLine :: (Num a, HasPosition s a, HasPosition t a) => s -> t -> PipeSegment a
pipeLine s t = PipeLine (t ^. _pos - s ^. _pos)

pipeSpline
  :: (Trans a, HasPosition s a, HasNormal s a, HasPosition t a, HasNormal t a)
  => s -> t -> PipeSegment a
-- ARBITRARY CONSTANT
pipeSpline = pipeSpline' 0.1

pipeSpline'
  :: (Trans a, HasPosition s a, HasNormal s a, HasPosition t a, HasNormal t a)
  => a -> s -> t  -> PipeSegment a
pipeSpline' a s t =
  let
    start = s ^. _pos
    end = t ^. _pos
    outboundN = (s ^. _n) ^* (co * a)
    inboundN = negate (t ^. _n) ^* (co * a)
    middle = lerp 0.5 start end
    co = distance end start
  in PipeSpline [outboundN, middle + outboundN + inboundN - start, end + inboundN - start] (end - start)


sampleBSpline :: Trans a => a -> Maybe a -> BSpline V.Vector (V3 a) -> ([(a, a, V3 a)], a)
sampleBSpline d slen spl = sample' d slen mlen (Bs.evalBSpline spl) (Bs.evalBSpline diff)
  where mlen = fromIntegral (V.length (controlPoints spl) - 1)
        diff = Bs.differentiateBSpline spl

samplePipeSegment :: Trans a => a -> Maybe a -> Located (PipeSegment a) -> ([(a, a, V3 a)], a)
samplePipeSegment d lo (Loc o ss@PipeSpline{}) =
  over (_1.traversed._3) (\x -> x ^+^ unP o) (sampleBSpline d lo (Hasklee.Pipe.toBSpline ss))
samplePipeSegment d lo (Loc o (PipeLine vl)) =
  samplePipeSegment d lo (Loc o (PipeSpline [] vl ))

samplePipe :: Trans a => a -> Located (Pipe a) -> [(a, a, V3 a)]
samplePipe d p =
  let
    go Seq.Empty _ as = as
    go (a Seq.:<| Seq.Empty) lo as =
      let
        (asam, leftover) = _1 %~ reverse $ samplePipeSegment d (Just lo) a
      in merge as asam
    go (a Seq.:<| rs) lo as =
      let
        (asam, leftover) = _1 %~ reverse . drop 1 $ samplePipeSegment d (Just lo) a
      in go rs leftover (merge as asam)
    merge [] a = a
    merge as aaaa = let (s, l, _) = last as in as ++ fmap (\(a, b, vs) -> (a + s, b + l, vs)) aaaa
  in go (toSegments p) 0 []


randomizePipeSegment :: (Num a, Random a) => a -> PipeSegment a -> IO (PipeSegment a)
randomizePipeSegment _ p@PipeLine{} = return p
randomizePipeSegment r (PipeSpline l@(a:b:c:ls) v) =
  (\y -> PipeSpline (a:y++[last l]) v) <$>
  mapM (\x -> randomRIO (pure (-r), pure r) >>= \rr -> return $ x ^+^ rr) (init . tail $ l)
randomizePipeSegment _ p@PipeSpline{} = return p

randomizePipe :: (Num a, Random a) => a -> Pipe a -> IO (Pipe a)
randomizePipe r (Pipe se) = Pipe <$> mapM (randomizePipeSegment r) se
