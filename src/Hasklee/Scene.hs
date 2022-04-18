module Hasklee.Scene
  ( NScene
  , newID, newObj, newPath
  , newTexturePath, newTextureByteS, newMaterial
  , nextid
  , prefab0, prefab1, prefab2, prefab3, prefab4
  , prefabF0, prefabF1, prefabF2, prefabF3, prefabF4
  , exportNormals, exportColors, exportUVs
  , hashMeshN, hashMeshes, hashCodes, hashTextures, hashMaterials
  ) where

import Control.Lens hiding (transform)
import Control.Monad.State
import qualified Data.ByteString as B (ByteString)
import Data.Hashable
import qualified Data.HashMap.Lazy as HMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import qualified Data.Text as T (Text)

import Hasklee.Attribute
import Hasklee.Mesh
import Hasklee.Object.Internal
import Hasklee.Transform
import Hasklee.Scene.Internal
import Hasklee.Spatial as Spatial
import Hasklee.Vertex


exportNormals :: Bool -> NScene a ()
exportNormals = assign (noptions.exportNormalsN)

exportUVs :: Bool -> NScene a ()
exportUVs = assign (noptions.exportUVsN)

exportColors :: Bool -> NScene a ()
exportColors = assign (noptions.exportColorsN)


hashMeshes :: Bool -> NScene a ()
hashMeshes = assign (hoptions.hashMeshesH)

hashCodes :: Bool -> NScene a ()
hashCodes = assign (hoptions.hashCodesH)

hashTextures :: Bool -> NScene a ()
hashTextures = assign (hoptions.hashTexturesH)

hashMaterials :: Bool -> NScene a ()
hashMaterials = assign (hoptions.hashMaterialsH)


newObj :: Object a -> NScene a ()
newObj m = nobjects %= flip snoc m

newPrefab :: (Hashable a, Trans a) => Object a -> NScene a (Prefab a)
newPrefab o = do
  o' <- hashMeshes' >=> hashCodes' $ o
  ii <- uses nprefabs Seq.length
  let o'' = over (each._attributes) unparams o'
  let pre = prefabi ii o''
  nprefabs %= flip snoc pre
  -- insidePrefab .= False
  return pre

prefabi :: Trans a => Int -> Object a -> Prefab a
prefabi i o = Prefab "" i o pmemo
  where pmemo = PrefabMemo
                (oabb $ toListOf vertices (toMesh $ set _localSpace originT o))

newID :: NScene a RealID
newID = use insidePrefab >>= \p -> if p then use nextid else nextid <<+= 1


newPath :: [DanceStep a] -> NScene a Int
newPath m = npaths <%= flip snoc m <&> flip (-) 1 . Seq.length

newTexturePath :: (Eq a, Hashable a) => T.Text -> NScene a Texture
newTexturePath = fmap TextureRef . newTextureH . TexturePath

newTextureByteS :: (Eq a, Hashable a) => B.ByteString -> NScene a Texture
newTextureByteS = fmap TextureRef . newTextureH . TextureData

newMaterial :: (Floating a, Hashable a, Ord a)
            => T.Text -> Maybe Texture -> MaterialOptions a -> NScene a (Material a)
newMaterial shader texture moptions =
  fmap MaterialRef . newMaterialH $ Material shader texture moptions


unparams :: Attribute a -> Attribute a
unparams RealIDAtr{} = mempty
unparams (ListAtr l) = foldMap unparams l
unparams (ComponentAtr l) = ComponentAtr $ filter (\case { ParamC _ -> False; _ -> True }) l
unparams a = a


prefab0 :: (Hashable a, Trans a)
        => Object a
        -> NScene a (Object a)
prefab0 o =
        newPrefab o >>= \p -> return $
        leaf $ InstanceObj $ Instance p o

prefab1 :: (Hashable a, Trans a)
        => (b -> Object a) -> b
        -> NScene a (b -> Object a)
prefab1 f b =
        newPrefab (f b) >>= \p -> return $
        \b' -> leaf $ InstanceObj $ Instance p (f b')

prefab2 :: (Hashable a, Trans a)
        => (b -> c -> Object a) -> b -> c
        -> NScene a (b -> c -> Object a)
prefab2 f b c =
        newPrefab (f b c) >>= \p -> return $
        \b' c' -> leaf $ InstanceObj (Instance p (f b' c'))

prefab3 :: (Hashable a, Trans a)
        => (b -> c -> d -> Object a) -> b -> c -> d
        -> NScene a (b -> c -> d -> Object a)
prefab3 f b c d =
        newPrefab (f b c d) >>= \p -> return $
        \b' c' d' -> leaf $ InstanceObj (Instance p (f b' c' d'))

prefab4 :: (Hashable a, Trans a)
        => (b -> c -> d -> e -> Object a) -> b -> c -> d -> e
        -> NScene a (b -> c -> d -> e -> Object a)
prefab4 f b c d e =
        newPrefab (f b c d e) >>= \p -> return $
        \b' c' d' e' -> leaf $ InstanceObj (Instance p (f b' c' d' e'))


prefabF0 :: (Hashable a, Trans a)
         => NScene a (Object a)
         -> NScene a (NScene a (Object a))
prefabF0 f =
         iprefab *> f <* oprefab >>=
         newPrefab >>= \p -> return $
         leaf . InstanceObj . Instance p <$> f

prefabF1 :: (Hashable a, Trans a)
         => (b -> NScene a (Object a)) -> b
         -> NScene a (b -> NScene a (Object a))
prefabF1 f b =
         iprefab *> f b <* oprefab >>=
         newPrefab >>= \p -> return $
         \b' -> leaf . InstanceObj . Instance p <$> f b'

prefabF2 :: (Hashable a, Trans a)
         => (b -> c -> NScene a (Object a)) -> b -> c
         -> NScene a (b -> c -> NScene a (Object a))
prefabF2 f b c =
         iprefab *> f b c <* oprefab >>=
         newPrefab >>= \p -> return $
         \b' c' -> leaf . InstanceObj . Instance p <$> f b' c'

prefabF3 :: (Hashable a, Trans a)
         => (b -> c -> d -> NScene a (Object a)) -> b -> c -> d
         -> NScene a (b -> c -> d -> NScene a (Object a))
prefabF3 f b c d =
         iprefab *> f b c d <* oprefab >>=
         newPrefab >>= \p -> return $
         \b' c' d' -> leaf . InstanceObj . Instance p <$> f b' c' d'

prefabF4 :: (Hashable a, Trans a)
         => (b -> c -> d -> e -> NScene a (Object a)) -> b -> c -> d -> e
         -> NScene a (b -> c -> d -> e -> NScene a (Object a))
prefabF4 f b c d e =
         iprefab *> f b c d e <* oprefab >>=
         newPrefab >>= \p -> return $
         \b' c' d' e' -> leaf . InstanceObj . Instance p <$> f b' c' d' e'
