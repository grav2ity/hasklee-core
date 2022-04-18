module Hasklee.Object.Mod
  ( key
  , slider
  , telescope, telescopeEx
  , rgbLights, rgbObject, rgbSliders
  ) where


import Control.Lens hiding (children, transform)
import Control.Monad.State
import Data.Tree
import Linear

import Hasklee.Attribute
import Hasklee.Colour as CN
import Hasklee.Mesh
import Hasklee.MeshMod
import Hasklee.Object
import Hasklee.Scene
import Hasklee.Spatial
import Hasklee.Transform
import Hasklee.Vertex


key :: Trans a => Traversal' (Object a) (Mesh a) -> Object a -> Object a
key l o =
  let
    keyCollider = dummyObj & attribute (ColliderAtr (o ^. l))
    o' = o
      & component Key
      & attribute IgnoreRayCastAtr
      & path "key" (keyPath o)
    (V3 _ ye _) = extents (rootLabel . unObject $ o)
    keyPath x = timedT 0 o3 <> timedT 0.3 (transform (aroundAxisT o' (V3 1 0 0) (pi*0.05)) o3)
      where o3 = x ^. _localSpace
            o' = translateY (ye * 0.5) (o3 ^. _pos)
  in attach o' keyCollider

slider :: Trans a
       => Traversal' (Object a) (Mesh a) -> (Object a -> Object a) -> Object a
       -> Object a
slider l f o = attach (over l ff o) (f knob)
  where m0 = o ^. l
        slitH = 0.8
        slitW = 0.05
        slitM = surfaceScale (V2 slitW slitH) m0
        ff = slit (V2 slitW slitH)
        mK = surfaceScale (V2 0.2 0.2) . set _center zero $ m0
        (V3 _ ye _) = extents mK
        knob = recenter (meshObj mK)
          & over _mesh (buttonize . closedMe)
          & path "slider" slidePath
          & component (Slider 0 (ye*10))
          & colour CN.red
          & translate (view _pos pS)
        slidePath = timedT 0 (translateT . view _pos $ pS) <> timedT 1 (translateT . view _pos $ pE)
        pS = vLine 0.5 0.1 slitM
        pE = vLine 0.5 0.9 slitM


telescope :: Trans a => V3 a -> Int -> a -> Object a -> NScene a (Object a)
telescope =
  let f o = translateT (v ^* negate (extentF v o))
        -- well which is ix 0 ?
        where v = o ^._mesh.singular (ix 0).to faceSpace._n
  in telescopeEx f

telescopeEx :: Trans a => (Object a -> Transform a) -> V3 a -> Int -> a -> Object a -> NScene a (Object a)
telescopeEx tf s n sensi o = do
  pre <- prefab0 . name "telePart" $ antennaUnfoldN (timedT 1 (transform (tf o) (o ^. _localSpace)) <> timedT 0 (o ^. _localSpace)) sensi o
  return $ over rootN (component' Stop) $ stackingDolls s n pre

antennaUnfoldN :: Trans b => HasAttributes t b => Transform b -> b -> t -> t
antennaUnfoldN t sensi = path "slider" t . component' (AntennaUnfold sensi)

stackingDolls :: Trans a => V3 a -> Int -> Object a -> Object a
stackingDolls s n o = foldl1 (flip attach) (replicate (n-1) t ++ [o])
  where t = o & _localSpace .~ scaleT s


rgbLights :: Num a => Object a -> StateT Int (NScene a) (Object a)
rgbLights x = do
  selfID <- lift newID
  actionID <- id <<+= 3
  return $ x
    & colour CN.black & rID selfID
    & actionR actionID "local k = ... ; self.gameObject.SetLightColorR(k)"
    & actionR (actionID + 1) "local k = ... ; self.gameObject.SetLightColorG(k)"
    & actionR (actionID + 2) "local k = ... ; self.gameObject.SetLightColorB(k)"

rgbObject :: Trans a => a -> Object a -> StateT Int (NScene a) (Object a)
rgbObject s x = do
  selfID <- lift newID
  actionID <- id <<+= 3
  let k = "(k * " ++ show s ++ ")"
  return $ x
    & rID selfID
    & colour CN.black
    & actionR actionID ("local k = ... ; self.gameObject.SetColorR" ++ k)
    & actionR (actionID + 1) ("local k = ... ; self.gameObject.SetColorG" ++ k)
    & actionR (actionID + 2) ("local k = ... ; self.gameObject.SetColorB" ++ k)

rgbSliders :: Trans a => a -> Object a -> NScene a (Object a)
rgbSliders s x = do
  selfID <- newID
  return (Hasklee.Object.Mod.slider (_mesh.ix 0) (f selfID) x)
    where
      k = "(k * " ++ show s ++ ")"
      f i x = x & rID i & colour CN.black &
        let mm = mod (i - 1) 3 in
          case mm of
            0 -> actionR i ("local k = ... ; self.gameObject.SetColorR" ++ k)
            1 -> actionR i ("local k = ... ; self.gameObject.SetColorG" ++ k)
            2 -> actionR i ("local k = ... ; self.gameObject.SetColorB" ++ k)
            _ -> undefined
