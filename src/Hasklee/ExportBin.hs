{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}

module Hasklee.ExportBin
  ( BinIO(..)
  , encMesh
  , writeScene
  ) where

import qualified Algebra.Graph as AG
import Control.Arrow ((&&&))
import Control.Lens hiding (transform)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.Colour hiding (over)
import Data.Colour.SRGB.Linear
import Data.Default.Class
import Data.Foldable
import Data.Hashable
import qualified Data.HashMap.Lazy as HMap
import Data.List (sort, sortOn)
import Data.Maybe
import Data.Monoid (Sum(..), getSum)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tree
import GHC.Generics ((:+:), (:*:))
import qualified GHC.Generics as G
import Generic.Data
import Linear
import System.IO

import Hasklee.Attribute
import Hasklee.Mesh
import Hasklee.Object.Internal
import Hasklee.Scene
import Hasklee.Scene.Internal
import Hasklee.Transform
import Hasklee.Vertex


class Encode' f where
  encode' :: f a -> Builder

instance Encode' G.V1 where
  encode' x = case x of {}

instance Encode' G.U1 where
  encode' _ = mempty

instance (Encode' f, Encode' g) => Encode' (f :+: g) where
  encode' (G.L1 x) = encode' x
  encode' (G.R1 x) = encode' x

instance (Encode' f, Encode' g) => Encode' (f :*: g) where
  encode' (x G.:*: y) = encode' x <> encode' y

instance Encode c => Encode' (G.K1 i c) where
  encode' (G.K1 x) = encode x

instance Encode' f => Encode' (G.M1 i t f) where
  encode' (G.M1 x) = encode' x


class Encode a where
  encode :: a -> Builder
  default encode :: (G.Generic a, Encode' (G.Rep a)) => a -> Builder
  encode x = encode' (G.from x)

gencode :: (G.Generic a, Encode' (G.Rep a)) => a -> Builder
gencode = encode' . G.from


instance Encode Builder where
  encode = id

instance Encode B.ByteString where
  encode bs = int32LE (fromIntegral $ B.length bs) <> byteString bs

instance Encode Lazy.ByteString where
  encode bs = int32LE (fromIntegral $ Lazy.length bs) <> lazyByteString bs

instance Encode Bool where
  encode = word8 . fromIntegral . fromEnum

instance Encode Int where
  encode = int32LE . fromIntegral

instance Encode Float where
  encode = floatLE . realToFrac

instance Encode Double where
  encode = floatLE . realToFrac

instance Encode a => Encode (V3 a) where
  encode = foldMap encode

instance Encode a => Encode (V4 a) where
  encode = foldMap encode

instance Encode a => Encode (a, a)

instance Encode a => Encode (a, a, a)

instance Encode T.Text where
  encode s = int32LE (fromIntegral $ T.length s) <> T.encodeUtf8Builder s

instance Encode a => Encode [a] where
  encode s = int32LE (fromIntegral $ length s) <> foldMap encode s

instance Encode a => Encode (Seq.Seq a) where
  encode s = int32LE (fromIntegral $ length s) <> foldMap encode s

instance Encode a => Encode (Maybe a) where
  encode Nothing = encode False
  encode (Just x) = encode True <> encode x

instance Encode a => Encode (Quaternion a)

instance (Fractional a, Encode a) => Encode (Colour a) where
  encode (toRGB -> RGB r g b) = encode (V4 r g b 1)

instance (Trans a, Encode a) => Encode (AlphaColour a) where
  encode (toRGB . alphaToColour &&& alphaChannel -> (RGB r g b, a)) = encode (V4 r g b a)

instance (Encode a, Ord a) => Encode (AG.Graph a) where
  encode g = encGraph g encode

instance (Encode a, Trans a) => Encode (Transform a) where
  -- encode (changeHand -> Transform v q s) = word8 0 <> encode v <> encode q <> encode s
  encode (changeHand -> (Matrix m')) =
    word8 1 <> encode (m ^. column _w) <> encode (m ^. column _z) <>
    encode (m ^. column _y) <> encode (norm <$> transpose (m' ^. _m33))
    where m = m' ^. _m34
  encode m = encode . toM44 $ m


instance Encode Code where
  encode (CodeString s) = encode s
  encode (CodeRef i) = int16LE (fromIntegral i)

instance Encode Texture where
  encode x = (word8 . fromIntegral . gconIndex $ x) <> gencode x

instance Encode LightType where
  encode = int16LE . fromIntegral . fromEnum

instance Encode LoopType where
  encode = int16LE . fromIntegral . fromEnum

instance Encode JointMotionType where
  encode = int16LE . fromIntegral . fromEnum


instance (Encode a, Fractional a) => Encode (LightOptions a)

instance (Encode a, Trans a) => Encode (DanceStep a)

instance Encode a => Encode (DanceOptions a)

instance (Encode a, Trans a) => Encode (MaterialOptions a)

instance (Encode a, Trans a) => Encode (Material a)

instance Encode NOptions


instance Encode a => Encode (Mesh a) where
  encode = error "Encode Mesh should not be called directly."

instance (Encode a, Trans a) => Encode (Attribute a) where
  encode = error "Encode Attribute should not be called directly."

instance (Encode a, Trans a) => Encode (Component a) where
  encode = encComponent


attributeType :: Attribute a -> Int
attributeType (Param atr) = attributeType atr
attributeType (ActionRR _ CodeString{}) = 900
attributeType (ActionRR _ CodeRef{}) = 901
attributeType (ActionSS _ CodeString{}) = 902
attributeType (ActionSS _ CodeRef{}) = 903
attributeType x = gconIndex x

encAttributeType :: Attribute a -> Builder
encAttributeType = int16LE . fromIntegral . attributeType

encAttributePayLoad :: (Encode a, Trans a) => Attribute a -> Builder
encAttributePayLoad (Param a) = encAttributePayLoad a
encAttributePayLoad (VectorAtr s v) = encode s <> encode (changeHand v)
encAttributePayLoad (ColliderAtr m) = encMesh changeHand (encVertexUnity False False False) m
encAttributePayLoad (MaterialAtr (MaterialRef i)) = word8 0 <> int16LE (fromIntegral i)
encAttributePayLoad (MaterialAtr s@Material{}) = word8 1 <> encode s
encAttributePayLoad (MaterialAtr s@MaterialName{}) = word8 2 <> encode s
encAttributePayLoad StaticAtr = mempty
encAttributePayLoad IndexAtr{} = mempty
-- this should not happen??
encAttributePayLoad (ListAtr l) = error "FOO"
encAttributePayLoad x = gencode x

encAttribute :: (Encode a, Trans a) => Attribute a -> Builder
encAttribute (Param atr) = encAttribute atr
encAttribute (ListAtr l) = int16LE (fromIntegral $ length l) <> encAttribute' (ListAtr l)
encAttribute atr = int16LE 1 <> encAttribute' atr

encAttribute' :: (Encode a, Trans a) => Attribute a -> Builder
encAttribute' (ListAtr l) = foldMap encAttribute' (sort l)
encAttribute' a = encAttributeType a <> encAttributePayLoad a

componentType :: Component a -> Int
componentType (ParamC c) = componentType c
componentType x = gconIndex x

encComponentType :: Component a -> Builder
encComponentType = int16LE . fromIntegral . componentType

encComponentPayLoad :: (Encode a, Trans a) => Component a -> Builder
encComponentPayLoad (ParamC c) = encComponentPayLoad c
encComponentPayLoad (GraphPropagate i j k) = int32LE (fromIntegral i) <> int16LE (fromIntegral j) <> encode k
encComponentPayLoad x = gencode x

encComponent :: (Encode a, Trans a) => Component a -> Builder
encComponent c = encComponentType c <> encComponentPayLoad c


encIAttribute :: (Encode a, Trans a) => Object a -> Builder
encIAttribute o = lengt <> zz
  where
    f (k, a) = encTreeKey k <> fromMaybe mempty a
    ial = filter (\a -> case snd a of Just _ -> True; Nothing -> False) $
          over (each._2) encIAttribute' $ o' ^.. (_Wrapped'.itraversed.withIndex)
    o' = o & attribute (view (re parameter) $ TransformAtr (o ^. _localSpace))
    zz = int16LE (fromIntegral $ length ial) <> foldMap f ial
    lengt = int32LE (fromIntegral . Lazy.length . toLazyByteString $ zz)

encIAttribute' :: (Encode a, Trans a) => ObjectN a -> Maybe Builder
encIAttribute' (InstanceObj (Instance _ o)) = Just $ encode True <> encIAttribute o'
  where
    o' = o & attribute (view (re parameter) $ TransformAtr (o ^. _localSpace))
encIAttribute' o = foo $ toListOf (_attributes.parameter) o
  where
    foo [] = Nothing
    foo x = Just $ encode False <> foldMap encAttribute x

encTreeKey :: [Int] -> Builder
encTreeKey k =
  int16LE (fromIntegral $ length k) <> foldMap (int16LE . fromIntegral) k


encPrefab :: (Encode a, Trans a) => Prefab a -> Reader (NStack a) Builder
encPrefab (Prefab _ _ o _) = encObject o

encObject :: (Encode a, Trans a) => Object a -> Reader (NStack a) Builder
encObject o = do
  oo <- traverse encObjectN trl
  return $ int32LE (fromIntegral $ length trl) <> fold oo <> encTree (unObject o)
  where
    trl = toList . unObject $ o

encObjectN :: (Encode a, Trans a) => ObjectN a -> Reader (NStack a) Builder
encObjectN (DummyObj atr) = return $ word8 0 <> encAttribute atr
encObjectN (CullObj atr) = return $ word8 0 <> encAttribute atr
encObjectN (MeshObj m atr) = do
  mm <- encMeshUnity m
  return $ word8 1 <> mm <> encAttribute atr
encObjectN (SolidObj s atr) = do
  mm <- encMeshUnity (toMesh s)
  return $ word8 1 <> mm <> encAttribute atr
encObjectN (MeshRefObj m _ atr) =
  return $ word8 2 <> int16LE (fromIntegral m) <> encAttribute atr
encObjectN (InstanceObj (Instance (Prefab _ i _ _) o)) =
  return $ word8 55 <> int16LE (fromIntegral i) <> encIAttribute o


encGraph :: Ord a => AG.Graph a -> (a -> Builder) -> Builder
encGraph gr f =
  int32LE (fromIntegral $ length adj) <>
  foldMap (\(el, li) -> f el <> (int32LE (fromIntegral $ length li) <> foldMap f li)) adj
  where adj = AG.adjacencyList gr

encTree :: Tree a -> Builder
encTree (Node _ []) = int32LE 0
encTree t =
  let (Node _ as) = labelTree t
      (b, count) = foldMap (encTree' 0) as
  in int32LE (fromIntegral $ getSum count) <> b

labelTree :: Tree a -> Tree (Int, a)
labelTree t = evalState (traverse go t) 0
  where go x = get >>= (\i -> put (i + 1) >> return (i, x))

encTree' :: Int -> Tree (Int, a) -> (Builder, Sum Int)
encTree' i (Node (j, _) as) =
  (int32LE (fromIntegral i) <> int32LE (fromIntegral j), Sum 1) <>
  foldMap (encTree' j) as


encNStack :: (Encode a, Trans a) => Reader (NStack a) Builder
encNStack = do
  ns <- ask
  let
    meshes = fst <$> sortOn snd (HMap.toList (ns ^. nmeshes))
    codes = fst <$> sortOn snd (HMap.toList (ns ^. ncodes))
    textures = fst <$> sortOn snd (HMap.toList (ns ^. ntextures))
    materials = fst <$> sortOn snd (HMap.toList (ns ^. nmaterials))
  meshes' <- traverse encMeshUnity meshes
  prefabs <- traverse encPrefab (ns ^. nprefabs)
  objects <- traverse encObject (ns ^. nobjects)
  return
     $ encode (ns ^. noptions)
    <> encode meshes'
    <> encode codes
    <> encode textures
    <> encode materials
    <> encode (ns ^. npaths)
    <> encode prefabs
    <> encode objects


encVertexUnity :: RealFloat a => Bool -> Bool -> Bool -> Vertex a -> Builder
encVertexUnity en euv ec v =
  foldMap (floatLE . realToFrac)
  (v ^..
   (_pos.folded <>
    (if en then _n.folded else mempty) <>
    (if euv then _uv.folded else mempty) <>
    (if ec then _color.folded else mempty)
   )
  )

encFace :: Indices -> Builder
encFace (Indices a b c) = enc a <> enc b <> enc c
  where enc = word32LE . fromIntegral

encMeshUnity :: RealFloat a => Mesh a -> Reader (NStack a) Builder
encMeshUnity m = do
  en <- view (noptions.exportNormalsN)
  euv <- view (noptions.exportUVsN)
  ec <- view (noptions.exportColorsN)
  return $ encMesh changeHand (encVertexUnity en euv ec) m

encMesh :: RealFloat a => (Mesh a -> Mesh a) -> (Vertex a -> Builder) -> Mesh a -> Builder
encMesh pref vecf m =
  let ((verts, faces), (vc, fc)) = runState (encMesh' vecf (pref m)) (0, 0)
  in word32LE (fromIntegral vc) <> word32LE (fromIntegral fc) <> verts <> faces

encMesh' :: RealFloat a => (Vertex a -> Builder) -> Mesh a ->
            State (Int, Int) (Builder, Builder)
encMesh' _ EmptyMesh = return (mempty, mempty)
encMesh' vef (Triangle a b c) = do
  (vc, fc) <- get
  put (vc + 3, fc + 1)
  return (vef a <> vef b <> vef c, encFace $ Indices vc (vc+1) (vc+2))
encMesh' vef (Quad a b c d) = do
  (vc, fc) <- get
  put (vc + 4, fc + 2)
  return (vef a <> vef b <> vef c <> vef d,
          encFace (Indices vc (vc+1) (vc+3)) <>
          encFace (Indices (vc+3) (vc+1) (vc+2)))
encMesh' vef t@TriangleFan{} = encMesh' vef . toTriangleSeq $ t
encMesh' vef (TriangleSeq verts fcs) = do
  (vc, fc) <- get
  put (vc + length verts, fc + length fcs)
  return (foldMap vef verts,
          foldMap (encFace . (\(Indices ai bi ci) ->
                                   Indices (ai + vc) (bi + vc) (ci + vc))) fcs)
encMesh' vef (FlipSided m) = encMesh' vef (reverseMesh m)
encMesh' vef (DoubleSided m) = encMesh' vef $ m <> reverseMesh m
encMesh' vef (Closed m) = encMesh' vef m
encMesh' vef (TaggedMesh _ m) = encMesh' vef m
encMesh' vef (MeshSeq s) = fold <$> traverse (encMesh' vef) s


-- --================


preNS :: (Trans a, Hashable a) => b -> NScene a b
preNS r = do
  use (hoptions.hashMeshesH) >>= flip when
    (use nobjects >>= each hashMeshes' >>= assign nobjects) >>
    (use nprefabs >>= (each.prefabObj) hashMeshes' >>= assign nprefabs)
  use (hoptions.hashCodesH) >>= flip when
    (use nobjects >>= each hashCodes' >>= assign nobjects) >>
    (use nprefabs >>= (each.prefabObj) hashCodes' >>= assign nprefabs)
  use (hoptions.hashTexturesH) >>= flip when
    (use nobjects >>= each hashTextures' >>= assign nobjects) >>
    (use nprefabs >>= (each.prefabObj) hashTextures' >>= assign nprefabs)
  use (hoptions.hashMaterialsH) >>= flip when
    (use nobjects >>= each hashMaterials' >>= assign nobjects) >>
    (use nprefabs >>= (each.prefabObj) hashMaterials' >>= assign nprefabs)
  return r


class BinIO a where
  binIO :: a -> IO Builder


instance (Encode a, Hashable a, Trans a) =>
         BinIO (NScene a ()) where
  binIO o = runReader encNStack <$>
            execStateT (o >>= preNS) eNStack

instance (Encode a, Hashable a, Trans a) =>
         BinIO (NScene a (Object a)) where
  binIO o = runReader encNStack <$>
            execStateT (o >>= preNS >>= newObj) eNStack

instance (Encode a, Hashable a, Trans a, Default b) =>
         BinIO (b -> NScene a (Object a)) where
  binIO o = runReader encNStack <$>
            execStateT (o def >>= preNS >>= newObj) eNStack

instance (Encode a, Hashable a, Trans a, Default b, Default c) =>
         BinIO (b -> c -> NScene a (Object a)) where
  binIO o = runReader encNStack <$>
            execStateT (o def def >>= preNS >>= newObj) eNStack

instance (Encode a, Hashable a, Trans a, Default b, Default c, Default d) =>
         BinIO (b -> c -> d -> NScene a (Object a)) where
  binIO o = runReader encNStack <$>
            execStateT (o def def def >>= preNS >>= newObj) eNStack

writeScene :: BinIO a => String -> a -> IO ()
writeScene fileName a = do
  b <- binIO a
  withBinaryFile fileName WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    hSeek h AbsoluteSeek 0
    hPutBuilder h b
    hFlush h

ntest :: Trans a => NScene a (Object a) -> IO (Object a)
ntest n = evalStateT (n >>= preNS) eNStack
