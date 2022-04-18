module Hasklee.Pipe.Conduit
  ( pipeConduit, pipeConduitM
  , pipes, pipesN, pipesR
  , pipesRand, pipesRand2, pipesRMesh, pipesRBounds
  , conduitShuffle
  ) where

import Control.Lens hiding (transform)
import Control.Monad.State
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Foldable
import Data.Hashable
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Diagrams.Located hiding (at)
import Linear
import Linear.Affine
import System.Random
import Test.QuickCheck

import Hasklee
import Hasklee.Pipe
import Hasklee.Generate (findNearestSeqF, silCircles)
import Hasklee.Spatial


data PipeC a = PipeC { unpc :: Located (Pipe a) }
             | PipeO (Located (Normal a))

instance Trans a => Outbound (PipeC a) a where
  outbound (PipeC p) = outbound p
  outbound (PipeO n) = outbound n


pipeO :: (HasPosition t a, HasNormal t a) => t -> PipeC a
pipeO t = PipeO $ Loc (P $ t ^. _pos) (Normal $ t ^. _n)

pipeConduit :: (HasNormal t a, HasPosition t a)
            => ConduitT (PipeC a) (PipeC a) Identity () -> [t] -> [Located (Pipe a)]
pipeConduit c p = unpc <$> (runIdentity . C.runConduit $ C.yieldMany ini .| c .| C.sinkList)
  where ini = pipeO <$> p

pipeConduitM :: (HasNormal t a, HasPosition t a, Monad m)
             => ConduitT (PipeC a) (PipeC a) m () -> [t] -> m [Located (Pipe a)]
pipeConduitM c p = fmap unpc <$> C.runConduit (C.yieldMany ini .| c .| C.sinkList)
  where ini = pipeO <$> p

conduitShuffle :: ConduitT a a IO ()
conduitShuffle = do
  l <- C.sinkList
  r <- randomRIO (0, length l * length l)
  C.yieldMany (permutations l !! r)

pipes :: (Trans a, HasPosition s a, HasNormal s a, Monad m)
      => [s] -> ConduitT (PipeC a) (PipeC a) m ()
pipes = pipesR id pipeSpline

pipesN :: (Trans a, HasPosition t a, HasNormal t a, Monad m)
       => [t] -> ConduitT (PipeC a) (PipeC a) m ()
pipesN = pipesR sf pipeSpline
  where
    sf (cs, trs) =
      let trs'' = toList $ sortMatch (\x y -> qd (x ^. _pos) (y ^. _pos))
                  (Seq.fromList trs) (outbound <$> cs)
          cs'' = toList $ sortMatch lenf (Seq.fromList cs) trs
          lenf x' y' = let x = x' ^. to outbound._pos; y = y' ^. _pos
                       in norm ((y^-^x) ^-^ project (x' ^. to outbound._n) (y ^-^ x))
      in (cs'', trs)

pipesR :: (Trans a, HasPosition s a, HasNormal s a, Monad m)
       => (([PipeC a], [s]) -> ([PipeC a], [s]))
       -> (forall t. (HasPosition t a, HasNormal t a) => t -> s -> PipeSegment a)
       -> [s] -> ConduitT (PipeC a) (PipeC a) m ()
pipesR ord g trs = do
  x <- C.take (length trs) .| C.sinkList
  let (x', trs') = ord (x, trs)
      newPipesegs = zipWith f x' trs'
  C.yieldMany newPipesegs
  where
    f (PipeC a@(Loc v p)) b = PipeC $ Loc v (p |> g (outbound a) b)
    f (PipeO n) b = PipeC $ pipel g n b

sortMatch :: (Num a, Ord a, Foldable r) => (s -> t -> a) -> Seq.Seq s -> r t -> Seq.Seq s
sortMatch cf s t =
  let go (news, olds) t' =
        let res = findNearestSeqF cf t' olds
        in case res of
             Nothing -> (news, olds)
             Just (!i, _, !ss) -> (news Seq.|> ss, Seq.deleteAt i olds)
  in fst $ foldl' go (Seq.empty, s) t


newtype Piper a = Piper { _laidPipes :: [Located (Pipe a)] }

makeLenses ''Piper

pipesRand :: (Trans a, MonadIO m, MonadState (Map.Map Int (Piper a)) m)
          => Mesh a -> (a, a) -> ConduitT (PipeC a) (PipeC a) m ()
pipesRand m1 r = C.mapM f
  where
    pf (PipeC a@(Loc v p)) b = PipeC $ Loc v (p |> pipeSpline (outbound a) b)
    pf (PipeO n) b = PipeC $ pipel pipeSpline n b
    key = hash m1
    f sh = do
      cl <- gets (Map.lookup key)
      pipes <- case cl of
                 -- Nothing -> modify (Map.insert key (Piper [] m1)) >> return []
                 Nothing -> modify (Map.insert key (Piper [])) >> return []
                 Just p -> return (p ^. laidPipes)
      let fC = faceCount m1
          notI pip = all k pipes
            -- ARBITRARY CONSTANT
            where k c' = minimum (qd <$> toPositions 0.1 c' <*> toPositions 0.1 pip) >= 0
          nnn = do
            f1i <- choose (0, fC - 1)
            nr <- choose r
            c1 <- head <$> silCircles (m1 ^. ix f1i) (nr, nr) 1
            return $ unpc $ pf sh (over (_localSpace._local) (rotateX pi) c1)
      newC <- liftIO . generate $ suchThat nnn notI
      at key._Just.laidPipes %= cons newC
      return . PipeC $ newC

pipesRand2 :: (Trans a, Hashable b, MonadIO m, MonadState (Map.Map Int (Piper a)) m)
           => b -> (b -> Gen (Located (Normal a)))
           -> ConduitT (PipeC a) (PipeC a) m ()
pipesRand2 m1 fun = C.mapM f
  where
    pf (PipeC a@(Loc v p)) b = PipeC $ Loc v (p |> pipeSpline (outbound a) b)
    pf (PipeO n) b = PipeC $ pipel pipeSpline n b
    key = hash m1
    f sh = do
      cl <- gets (Map.lookup key)
      pipes <- case cl of
                 Nothing -> modify (Map.insert key (Piper [])) >> return []
                 Just p -> return (p ^. laidPipes)
      let notI pip = all k pipes
            -- ARBITRARY CONSTANT
            where k c' = minimum (qd <$> toPositions 0.1 c' <*> toPositions 0.1 pip) >= 0
          nnn = unpc . pf sh <$> fun m1
      newC <- liftIO . generate $ suchThat nnn notI
      at key._Just.laidPipes %= cons newC
      return . PipeC $ newC

pipesRMesh :: (Trans a, MonadIO m, MonadState (Map.Map Int (Piper a)) m)
           => Mesh a -> (a, a) -> ConduitT (PipeC a) (PipeC a) m ()
pipesRMesh m1 r =
  let
    fun mm = do
      let fC = faceCount mm
      f1i <- choose (0, fC - 1)
      nr <- choose r
      c1 <- over (_localSpace._local) (rotateX pi) . head <$> silCircles (mm ^. ix f1i) (nr, nr) 1
      return $ Loc (P $ c1 ^. _pos) (Normal (c1 ^. _n))
  in pipesRand2 m1 fun

pipesRBounds :: (Trans a, Extent t a, MonadIO m, MonadState (Map.Map Int (Piper a)) m)
             => t -> (V3 a -> V3 a) -> ConduitT (PipeC a) (PipeC a) m ()
pipesRBounds (eoabb -> OABB r1 r2) f =
  let
    fun r = do
     v <- choose r
     return $ Loc (P v) (Normal (f v))
  in pipesRand2 (r1, r2) fun
