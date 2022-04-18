module Hasklee.Randomize
  ( HasRandomPoint(..)
  , randomize, randomizeR
  , randomizePositionOf
  , randomizeRotationOf
  , randomizeScaleOf
  , randomizeF, randomizeRF, randomizeRF2
  , irandomizeRF
  ) where

import Control.Lens hiding (transform)
import Control.Monad
import Control.Monad.IO.Class
import Linear hiding (rotate)
import System.Random
import Test.QuickCheck

import Hasklee.Mesh
import Hasklee.Transform
import Hasklee.Vertex


class HasRandomPoint t a | t -> a where
  randomPoint :: t -> Gen (V3 a)

instance (Random a, Trans a) => HasRandomPoint (Mesh a) a where
  randomPoint m =
    view _pos <$> (hLine <$> choose (0, 1) <*> choose (0, 1) <*> return m)


randomize :: (MonadIO m, Random a) => Traversal' s a -> s -> m s
randomize t = t (\_ -> liftIO randomIO)

randomizeR :: (MonadIO m, Random a) => Traversal' s a -> (a, a) -> s -> m s
randomizeR t r = t (\_ -> liftIO $ randomRIO r)

randomizePositionOf :: (MonadIO m, Num r, Random r, HasPosition a r)
                    => Traversal' s a -> V3 r -> s -> m s
randomizePositionOf t a = randomizeRF (t._pos) (a, negated a) (^+^)

randomizeRotationOf :: (MonadIO m, Random a, Trans a, HasLocalSpace b a)
                    => Traversal' s b -> V3 a -> a -> s -> m s
randomizeRotationOf t axis angle = (t._localSpace)
  (\a -> fmap ((\x r -> x <> rotateT (axisAngle axis r) ) a) (randomRIO (-angle, angle)))

randomizeScaleOf :: (MonadIO m, Trans r, Random r, Local a, Transformable a r)
                 => Traversal' s a -> (r, r) -> s -> m s
randomizeScaleOf t a = randomizeRF (t._local) a scaleU


randomizeF :: (MonadIO m, Random r)
           => Traversal' t a
           -> (r -> a -> a) -> t -> m t
randomizeF t f = t (liftM2 f randomIO . return)

randomizeRF :: (MonadIO m, Random r)
            => Traversal' t a
            -> (r, r) -> (r -> a -> a) -> t -> m t
randomizeRF t r f = t (liftM2 f (randomRIO r) . return)

randomizeRF2 :: (MonadIO m, Random r)
             => Traversal' t a
             -> (r, r) -> (r, r) -> (r -> r -> a -> a) -> t -> m t
randomizeRF2 t r1 r2 f = t (liftM3 f (randomRIO r1) (randomRIO r2) . return)

irandomizeRF :: (MonadIO m, Random r)
             => (Indexed i a (m b) -> s -> m t)
             -> (r, r) -> (i -> r -> a -> b) -> s -> m t
irandomizeRF t r f = itraverseOf t (\i -> liftM3 f (return i) (randomRIO r) . return)
