module Hasklee.Scene.Internal where

import Control.Lens hiding (transform)
import Control.Monad.State
import Data.Hashable
import qualified Data.HashMap.Lazy as HMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Generics (Generic)

import Hasklee.Attribute
import Hasklee.Mesh
import Hasklee.Object.Internal
import Hasklee.Transform

data NOptions = NOptions { _exportNormalsN :: Bool
                         , _exportUVsN :: Bool
                         , _exportColorsN :: Bool
                         }
              deriving (Generic, Show)

makeLenses ''NOptions

eNOptions :: NOptions
eNOptions = NOptions False True False

data HOptions = HOptions { _hashMeshesH :: Bool
                         , _hashCodesH :: Bool
                         , _hashTexturesH :: Bool
                         , _hashMaterialsH :: Bool
                         }
              deriving (Generic, Show)

makeLenses ''HOptions

eHOptions :: HOptions
eHOptions = HOptions False False True True

data NStack a = NStack { _npaths :: Seq [DanceStep a]
                       , _nobjects :: Seq (Object a)
                       , _nprefabs :: Seq (Prefab a)
                       , _nmeshes :: HMap.HashMap (Mesh a) MeshRef
                       , _ncodes :: HMap.HashMap T.Text Int
                       , _nmaterials :: HMap.HashMap (Material a) Int
                       , _ntextures :: HMap.HashMap Texture Int
                       , _nmeshesC :: Int
                       , _ncodesC :: Int
                       , _nmaterialsC :: Int
                       , _ntexturesC :: Int
                       , _nextid :: RealID
                       , _noptions :: NOptions
                       , _hoptions :: HOptions
                       , _insidePrefab :: Bool
                       }

makeLenses ''NStack

eNStack :: NStack a
eNStack = NStack Seq.empty Seq.empty Seq.empty
          HMap.empty HMap.empty HMap.empty HMap.empty
          0 0 0 0 1 eNOptions eHOptions False

type NScene a = StateT (NStack a) IO


class MeshIO a b where
  meshIO :: a -> IO (Mesh b)

instance Trans a => MeshIO (NScene a o) a where
  meshIO o = foldOf (nobjects.folded.to mesh) <$> execStateT o eNStack

iprefab :: NScene a ()
iprefab = insidePrefab .= True

oprefab :: NScene a ()
oprefab = insidePrefab .= False


newMeshH :: (Eq a, Hashable a) => Mesh a -> NScene a MeshRef
newMeshH m = use nmeshesC >>= \c ->
  nmeshes %%= HMap.alterF (maybe (c, Just c) (\x -> (x, Just x))) m >>= \r ->
  when (r == c) (nmeshesC += 1) >> return r

newCodeH :: (Eq a, Hashable a) => T.Text -> NScene a Int
newCodeH m = use ncodesC >>= \c ->
  ncodes %%= HMap.alterF (maybe (c, Just c) (\x -> (x, Just x))) m >>= \r ->
  when (r == c) (ncodesC += 1) >> return r

newMaterialH :: (Floating a, Hashable a, Ord a) => Material a -> NScene a Int
newMaterialH m = use nmaterialsC >>= \c ->
  nmaterials %%= HMap.alterF (maybe (c, Just c) (\x -> (x, Just x))) m >>= \r ->
  when (r == c) (nmaterialsC += 1) >> return r

newTextureH :: (Eq a, Hashable a) => Texture -> NScene a Int
newTextureH m = use ntexturesC >>= \c ->
  ntextures %%= HMap.alterF (maybe (c, Just c) (\x -> (x, Just x))) m >>= \r ->
  when (r == c) (ntexturesC += 1) >> return r

hashMeshN :: (Hashable a, Trans a) => ObjectN a -> NScene a (ObjectN a)
hashMeshN (MeshObj m a) = (\x -> MeshRefObj x m a) <$> newMeshH m
hashMeshN (SolidObj m a) = let m' = toMesh m in (\x -> MeshRefObj x m' a) <$> newMeshH m'
hashMeshN o = return o


hashMeshes' :: (Hashable a, Trans a) => Object a -> NScene a (Object a)
hashMeshes' = each hashMeshN

hashCodes' :: (Trans a, Hashable a) => Object a -> NScene a (Object a)
hashCodes' = (each._attributes) codeRefAH
  where
    codeRefAH :: (Eq a, Hashable a) => Attribute a -> NScene a (Attribute a)
    codeRefAH = each f
      where f (ActionRR i (CodeString s)) = ActionRR i . CodeRef <$> newCodeH s
            f (ActionSS i (CodeString s)) = ActionSS i . CodeRef <$> newCodeH s
            f a = return a
            -- g (Csound (CodeString s)) = Csound . CodeRef <$> newCodeH s

hashMaterials' :: (Hashable a, Trans a) => Object a -> NScene a (Object a)
hashMaterials' = (each.hashA._MaterialAtr) f
  where
    f x@MaterialRef{} = return x
    -- maybe hash this one too?
    f x@MaterialName{} = return x
    f x = MaterialRef <$> newMaterialH x

hashTextures' :: (Hashable a, Trans a) => Object a -> NScene a (Object a)
hashTextures' = (each.hashA._MaterialAtr._Material._2._Just) f
  where
    f x@TextureRef{} = return x
    -- maybe hash this one too?
    f x@TexturePath{} = return x
    f x = TextureRef <$> newTextureH x


hashA :: (Hashable a, Trans a) => Traversal' (ObjectN a) (Attribute a)
hashA g a@InstanceObj{} = (_attributes.each.parameter) g a
hashA g a = (_attributes.each) g a
