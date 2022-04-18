module Hasklee.Coords
  ( range
  , overBasis
  , circleF, ellipseF
  , ofCircle, ofEllipse, ofHyperbola, ofArc
  , arcL, arc
  , circleL, circle
  , ellipseL, ellipse
  , ngon
  , rectangleL, rectangle
  , squareL, square
  , triangleL, triangle
  , rightTriangleL, rightTriangle
  , isoscelesL, isosceles
  , rowL
  , lorenz
  ) where

import Control.Lens
import Data.Distributive
import Linear


circleF :: (Floating a, Applicative t)
        => ASetter' (t (a -> a)) (V2 (a -> a)) -> a -> a -> t a
circleF l r = overBasis l (ofCircle r)

ellipseF :: (Floating a, Applicative t)
         => ASetter' (t (a -> a)) (V2 (a -> a)) -> a -> a -> a -> t a
ellipseF l r1 r2 = overBasis l (ofEllipse r1 r2)


overBasis :: (Num a, Applicative t)
          => ASetter' (t (a -> a)) (r (a -> a)) -> r (a -> a) -> a -> t a
overBasis l f = distribute (set l f $ pure zero)

overRange :: (Enum a, Fractional a, Applicative t)
          => ASetter' (t (a -> a)) (r (a -> a)) -> r (a -> a) -> a -> a -> Int -> [t a]
overRange l f start end s = overBasis l f <$> range start end s


range :: (Enum a, Fractional a) => a -> a -> Int -> [a]
range start end n = [start, (start + d) .. end]
  where d = (end - start) / fromIntegral (n-1)

ofCircle :: Floating a => a -> V2 (a -> a)
ofCircle r = V2 (r *^ cos) (r *^ sin)

ofEllipse :: Floating a => a -> a -> V2 (a -> a)
ofEllipse r1 r2 = V2 (r1 *^ cos) (r2 *^ sin)

ofHyperbola :: Floating a => a -> V2 (a -> a)
ofHyperbola r = V2 (r *^ cosh) (r *^ sinh)

ofArc :: Floating a => a -> a -> a -> V2 (a -> a)
ofArc r s e = V2 (r *^ (cos . (\x -> s + x * (e - s))))
                 (r *^ (sin . (\x -> s + x * (e - s))))


arcL :: (Enum a, Fractional a, Applicative t)
     => ASetter' (t (a -> a)) (V2 (a -> a)) -> V2 (a -> a) -> a -> a -> Int -> [t a]
arcL = overRange

arc :: (Enum a, Fractional a, Applicative t, R2 t)
    => V2 (a -> a) -> a -> a -> Int -> [t a]
arc = arcL _xy

circleL :: (Enum a, Floating a, Applicative t)
        => ASetter' (t (a -> a)) (V2 (a -> a)) -> a -> Int -> [t a]
circleL l r s = overRange l (ofCircle r) start end s
  -- where start = (5/4)*pi
  where start = 0
        end = (start + (2*pi)) - (2*pi/ fromIntegral s)

circle :: (Enum a, Floating a, Applicative t, Additive t, R2 t) => a -> Int -> [t a]
circle = circleL _xy

ellipseL :: (Enum a, Floating a, Applicative t)
         => ASetter' (t (a -> a)) (V2 (a -> a)) -> a -> a -> Int -> [t a]
ellipseL l r1 r2 s = overRange l (ofEllipse r1 r2) start end s
  -- where start = (5/4)*pi
  where start = 0
        end = (start + (2*pi)) - (2*pi/ fromIntegral s)

ellipse :: (Enum a, Floating a, Applicative t, Additive t, R2 t)
        => a -> a -> Int -> [t a]
ellipse = ellipseL _xy

ngon :: (Enum a, Floating a, Applicative t, R2 t) => Int -> a -> [t a]
ngon s r = arc (ofCircle r) 0 (2*pi - (2*pi/ fromIntegral s)) (s - 1)


rectangleL :: (Additive t, Fractional a) => ASetter' (t a) (V2 a) -> a -> a -> [t a]
rectangleL l a b = uncurry f <$> [(-1, -1), (1, -1), (1, 1), (-1, 1)]
  where f x y = zero & l .~ V2 (x*a*0.5) (y*b*0.5)

rectangle :: (Additive t, R2 t, Fractional a) => a -> a -> [t a]
rectangle = rectangleL _xy

squareL :: (Additive t, R2 t, Fractional a) => ASetter' (t a) (V2 a) -> a -> [t a]
squareL a s = rectangleL a s s

square :: (Additive t, R2 t, Fractional a) => a -> [t a]
square = squareL _xy

triangleL :: (Additive t, Floating a, Fractional a)
          => ASetter' (t a) (V2 a) -> a -> [t a]
triangleL l a = uncurry f <$> [(-0.5*a, -h/3), (0.5*a, -h/3), (0, h*2/3)]
  where f x y = zero & l .~ V2 x y
        h = a * sqrt 3/2

triangle :: (Additive t, R2 t, Floating a, Fractional a) => a -> [t a]
triangle = triangleL _xy

rightTriangleL :: (Additive t, Floating a, Fractional a)
               => ASetter' (t a) (V2 a) -> a -> a -> [t a]
rightTriangleL l a b = uncurry f <$> [(0, 0), (a, 0), (0, b)]
  where f x y = zero & l .~ V2 x y

rightTriangle :: (Additive t, R2 t, Floating a, Fractional a) => a -> a -> [t a]
rightTriangle = rightTriangleL _xy

isoscelesL :: (Additive t, Floating a, Fractional a)
           => ASetter' (t a) (V2 a) -> a -> a -> [t a]
isoscelesL l a beta = uncurry f <$> [(-b, 0), (b, 0), (0, h)]
  where f x y = zero & l .~ V2 x y
        h = cos (beta*0.5) * a
        b = sin (beta*0.5) * a

isosceles :: (Additive t, R2 t, Floating a, Fractional a) =>  a -> a -> [t a]
isosceles = isoscelesL _xy

rowL :: (Additive t, Fractional a) => ASetter' (t a) a  -> a -> Int -> [t a]
rowL l1 s c =
  let fullW = fromIntegral c * s
      o = -fullW*0.5
      t = [ o + (s * fromIntegral x) | x <- [0,1..c] ]
      f s1 = zero & l1 .~ s1
  in f <$> t


lorenz :: Num a => a -> a -> a -> a -> V3 a -> V3 a
lorenz sigma rho beta h (V3 x0 y0 z0) = V3 x1 y1 z1
  where x1 = x0 + (sigma * (y0 - x0)) * h
        y1 = y0 + (x0 * (rho - z0) - y0) * h
        z1 = z0 + (x0 * y0 - beta * z0) * h
