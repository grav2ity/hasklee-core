module Hasklee.Generate
  ( findNearest, findNearestSeqF
  , generateSuchThat, generateSuchThatMaybe
  , silCircles, silShapes
  , randomPiping, randomPiping'
  )
where

import Control.Lens hiding (transform)
import Control.Monad.State
import qualified Data.Sequence as Seq
import Diagrams.Located
import Linear
import Test.QuickCheck
import System.Random

import Hasklee
import Hasklee.Pipe
import Hasklee.Randomize


findNearest :: (Num a, Ord a, Num i, FoldableWithIndex i t, Metric r)
            => (s -> r a) -> r a -> t s -> Maybe (i, a, s)
findNearest f c bs =
  let findNearest' z = ifoldl' k (0, dd z, z)
        where k ni (!ii, !miD, !t) a =
                let newD = dd a
                in if newD < miD then (ni, newD, a) else (ii, miD, t)
      dd x = qd c (f x)
  in findNearest' <$> (bs ^? folded) <*> Just bs

findNearestSeqF :: (Num a, Ord a)
                => (s -> r -> a) -> r -> Seq.Seq s -> Maybe (Int, a, s)
findNearestSeqF f c bs =
  let findNearest' z = ifoldl' k (0, dd z, z)
        where k ni (!ii, !miD, !t) a =
                let newD = dd a
                in if newD < miD then (ni, newD, a) else (ii, miD, t)
      dd x = f x c
  in findNearest' <$> (bs ^? folded) <*> Just bs


generateSuchThat :: Gen s -> ([t] -> s -> Bool) -> (s -> t) -> Int -> Gen [t]
generateSuchThat genF condF postF n =
  let go n' =
        when (n' > 0) $ do
        cc <- get
        newC <- lift $ suchThat genF (condF cc)
        put (postF newC:cc)
        go (n' - 1)
  in execStateT (go n) []

generateSuchThatMaybe :: Gen s -> ([t] -> s -> Bool) -> (s -> t) -> Int -> Gen [t]
generateSuchThatMaybe genF condF postF n =
  let go n' =
        when (n' > 0) $ do
        cc <- get
        newC <- lift $ suchThatMaybe genF (condF cc)
        put (maybe cc (:cc) (postF <$> newC))
        go (n' - 1)
  in execStateT (go n) []


-- generate n non overlapping random circles .. within the space
-- bounded by m's edges
silCircles :: (Random a, Trans a, HasEdges t a,
               HasLocalSpace t a, HasRandomPoint t a, Transformable t a)
           => t -> (a, a) -> Int -> Gen [Shape a]
silCircles m r n =
  let genF = Circle <$> choose r <*> (translateT <$> randomPoint m)
      notI cc (Circle r1 t1) = all k cc
        where k (Circle r2 t2) = distance (t2 ^. _pos) (t1 ^. _pos) > r1 + r2
      condF cc c@(Circle r3 t3) =
        let spa = do
              t3l <- local t3
              ml <- local m
              return $ insideE ml t3l r3
        in notI cc c && overSpaceOf' spa m
      postF = set (_localSpace._TBN) (view (_localSpace._TBN) m)
  in generateSuchThat genF condF postF n

silShapes :: (Random a, Trans a, HasEdges t a,
              HasLocalSpace t a, HasRandomPoint t a, Transformable t a)
          => t -> (V3 a -> Gen (Shape a)) -> Int -> Gen [Shape a]
silShapes m f n =
  let genF = randomPoint m >>= f <&> set (_localSpace._TBN) (view (_localSpace._TBN) m)
      notI cc shape1 = all k cc
        where k shape2 =
                let
                  (Circle r1 t1) = ocircle shape1
                  (Circle r2 t2) = ocircle shape2
                in distance (t2 ^. _pos) (t1 ^. _pos) > r1 + r2
      condF cc shape =
        let spa = do
              t3l <- local shape
              ml <- local m
              return $ allOf (to toVertices.folded) (\x -> insideE ml x 0) t3l
        in notI cc shape && overSpaceOf' spa m
      postF = id
  in generateSuchThat genF condF postF n

randomPiping :: Trans a
              => Mesh a -> a -> Int
              -> Gen [Located (Pipe a)]
randomPiping m r n = fmap (view _1) <$> randomPiping' m (r, r) 1 0 n

-- ADD OPTION TO CUSOTMIZE pipeSpline
randomPiping' :: Trans a
             => Mesh a -> (a, a) -> a -> a -> Int
             -> Gen [(Located (Pipe a), Int, Int)]
randomPiping' m (minr, maxr) s s1 n =
  let genF = do
        f1i <- choose (0, fC - 1)
        f2i <- suchThat (choose (0, fC - 1)) (/= f1i)
        nr <- choose (minr*s, maxr*s)
        c1 <- head <$> silCircles (reverseMesh $ m ^. ix f1i) (nr, nr) 1
        c2 <- head <$> silCircles (reverseMesh $ m ^. ix f2i) (nr, nr) 1

        let
          [c1',c2'] = (\x -> translate <$> (\a -> s1 *^ view _n a) <*>
                        id $ (x ^._localSpace) ) <$> [c1,c2]
        return (pipel pipeLine c2 c2' |>
                pipeSpline c2' (over _local (rotateX pi) c1') |>
                pipeLine c1' c1, f2i, f1i)

      condF cc pip = all k cc
        -- ARBITRARY CONSTANT
        where k c' = minimum (qd <$> toPositions 0.1 (view _1 c') <*>
                              toPositions 0.1 (view _1 pip)) >= ((maxr*s*2)**2)
      postF = id
      fC = faceCount m
  in generateSuchThat genF condF postF n
