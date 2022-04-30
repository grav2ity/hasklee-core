{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hasklee.Object.Internal where

import Control.DeepSeq (NFData)
import Control.Lens hiding (children, transform)
import Data.Binary (Binary)
import Data.Default.Class
import Data.Ix
import Data.Maybe
import qualified Data.Text as T
import Data.Tree
import Data.Tree.Lens
import GHC.Generics (Generic)
import Linear

import Hasklee.Attribute
import Hasklee.Mesh hiding (rotateL, rotateR)
import qualified Hasklee.Mesh as Mesh (rotateL, rotateR)
import Hasklee.Solid hiding (cubeS)
import Hasklee.Spatial as Spatial
import Hasklee.Subdiv
import Hasklee.Transform
import Hasklee.Vertex


type MeshRef = Int


-- Instance

data Instance a = Instance (Prefab a) (Object a)
                deriving (Binary, Generic, NFData, Show)

instance HasAttributes (Instance a) a where
  _attributes g (Instance o a) = Instance o <$> _attributes g a
  attributes g (Instance o a) = Instance o <$> attributes g a

instance HasLocalSpace (Instance a) a where
  _localSpace g (Instance o a) = Instance o <$> _localSpace g a

instance Trans a => HasPosition (Instance a) a where
  _pos = _localSpace._pos

instance Trans a => Transformable (Instance a) a where
  transform t o = o & _attributes._localSpace %~ transform t

instance Trans a => ToMesh (Instance a) a where
  toMesh (Instance o a) = transform (a ^. _localSpace) $ toMesh o

instance Trans a => Extent (Instance a) a where
  extent v (Instance Prefab{_prefabMemo} a) =
    let ex = transform (a ^. _localSpace) $ prefabOABB _prefabMemo
    in extent v ex


-- Prefab

data Prefab a = Prefab
                { _prefabName :: String
                , _prefabRef :: Int
                , _prefabObj :: Object a
                , _prefabMemo :: PrefabMemo a
                }
              deriving (Binary, Generic, NFData, Show)

newtype PrefabMemo a = PrefabMemo { prefabOABB :: OABB a }
                     deriving (Generic, Show)
                     deriving anyclass (Binary, NFData)

instance HasAttributes (Prefab a) a where
  _attributes g (Prefab s i a m) = (\x -> Prefab s i x m) <$> _attributes g a
  attributes g (Prefab s i a m) = (\x -> Prefab s i x m) <$> attributes g a

instance HasLocalSpace (Prefab a) a where
  _localSpace g (Prefab s i a m) = (\x -> Prefab s i x m) <$> _localSpace g a

instance Trans a => HasPosition (Prefab a) a where
  _pos = _localSpace._pos

instance Trans a => Transformable (Prefab a) a where
  transform t o = o & _attributes._localSpace %~ transform t

instance Trans a => ToMesh (Prefab a) a where
  toMesh Prefab{_prefabObj} = toMesh _prefabObj

instance Trans a => Extent (Prefab a) a where
  extent a Prefab{_prefabMemo} =
    let ex = prefabOABB _prefabMemo
    in extent a ex


-- Object

newtype Object a = Object { unObject :: Tree (ObjectN a) }
                 deriving (Generic, Show)
                 deriving anyclass (Binary, NFData)

instance Wrapped (Object a) where
  type Unwrapped (Object a) = Tree (ObjectN a)
  _Wrapped' = iso unObject Object

instance Semigroup (Object a) where
  (<>) = mappend

instance Monoid (Object a) where
  mempty = Object (Node (DummyObj mempty) [])
  mappend r l = Object (Node (DummyObj mempty) [unObject r, unObject l])
  mconcat = Object . Node (DummyObj mempty) . fmap unObject

instance Each (Object a) (Object a) (ObjectN a) (ObjectN a) where
  each = _Wrapped'.traversed

instance HasAttributes (Object a) a where
  _attributes = rootN._attributes
  attributes = rootN.attributes

instance HasLocalSpace (Object a) a where
  _localSpace = _Wrapped'.root._localSpace

instance Trans a => Local (Object a) where
  _local g t =
    (\x -> x & _localSpace .~ (t ^. _localSpace) <> (x ^. _localSpace)) <$>
    g (t & _localSpace .~ Origin)

instance Trans a => HasPosition (Object a) a where
  _pos = _localSpace._pos

instance Trans a => HasNormal (Object a) a where
  _n = _localSpace._n

instance Trans a => Transformable (Object a) a where
  transform t (Object r) = Object $ over root (transform t) r

instance Trans a => HasMesh (Object a) a where
  _mesh = rootN._mesh

instance Trans a => ToMesh (Object a) a where
  toMesh = foldMapOf (world.each) toMesh

instance Trans a => HasFaces (Object a) a where
  faces  = rootN._mesh.faces

instance Trans a => Extent (Object a) a where
  -- extent a o = maximum . fmap (extent a) . toListOf each $ worldSpace0 o
  extent v o = fromMaybe 0 mx
    where
      mx = maximumOf (world.each.to (extent v)) o

instance Trans a => HasCenter (Object a) a where
  _center g o =
    let c = toMesh o ^. _center
    in (\c' -> let d = c' - c in translate d o) <$> g c

type instance Index (Object a) = Int
type instance IxValue (Object a) = Object a

instance Trans a => Ixed (Object a) where
  ix i = _Wrapped'.branches.elementOf traversed i._Unwrapped'

instance Trans a => Tagged (Object a) (Object a) (Object a) (Object a) where
  tagged s g o = children (\o' -> if elemOf (attributes._TagAtr.to T.unpack)
                            s o' then g o' else pure o') o


rootN :: Lens' (Object a) (ObjectN a)
rootN = _Wrapped'.root

children :: Traversal' (Object a) (Object a)
children = _Wrapped'.branches.traversed._Unwrapped'

allAttributes :: Traversal' (Object a) (Attribute a)
allAttributes = each.attributes


leaf :: ObjectN a -> Object a
leaf a = Object (Node a [])

leaves :: [ObjectN a] -> Object a
leaves = mconcat . fmap leaf

attach :: Object a -> Object a -> Object a
attach (Object (Node l ls)) (Object r) = Object (Node l (r:ls))


meshObj :: (ToMesh t a, Default (MeshOptions t)) => t -> Object a
meshObj x = leaf (MeshObj (mesh x) mempty)

solidObj :: Trans a => Solid a -> Object a
solidObj (TransformedSolid s t) = transform t $ solidObj s
solidObj s = leaf (SolidObj s mempty)

dummyObj :: Object a
dummyObj = leaf (DummyObj mempty)

dummy :: Object a -> Object a
dummy _ = dummyObj


-- ObjectN

data ObjectN a = SolidObj (Solid a) (Attribute a)
               | MeshObj (Mesh a) (Attribute a)
               | MeshRefObj MeshRef (Mesh a) (Attribute a)
               | InstanceObj (Instance a)
               | DummyObj (Attribute a)
               | CullObj (Attribute a)
               deriving (Binary, Generic, NFData, Show)


instance Trans a => Eq (ObjectN a) where
  (==) a b =
    let aid = getID a
        bid = getID b
    in Just True == ((==) <$> aid <*> bid)

instance Each (ObjectN a) (ObjectN a) (ObjectN a) (ObjectN a) where
  each f a = f a

instance HasAttributes (ObjectN a) a where
  _attributes g (SolidObj a b) = SolidObj a <$> _attributes g b
  _attributes g (MeshObj a b) = MeshObj a <$> _attributes g b
  _attributes g (MeshRefObj a m b) = MeshRefObj a m <$> _attributes g b
  _attributes g (InstanceObj a) = InstanceObj <$> _attributes g a
  _attributes g (DummyObj a) = DummyObj <$> _attributes g a
  _attributes g (CullObj a) = CullObj <$> _attributes g a

  attributes = _attributes.attributes

instance HasLocalSpace (ObjectN a) a where
  _localSpace g (SolidObj s a) = SolidObj s <$> _localSpace g a
  _localSpace g (MeshObj s a) = MeshObj s <$> _localSpace g a
  _localSpace g (MeshRefObj s m a) = MeshRefObj s m <$> _localSpace g a
  _localSpace g (InstanceObj a) = InstanceObj <$> _localSpace g a
  _localSpace g (DummyObj a) = DummyObj <$> _localSpace g a
  _localSpace g (CullObj a) = CullObj <$> _localSpace g a

instance Trans a => Local (ObjectN a) where
  _local g t =
    (\x -> x & _localSpace .~ (t ^. _localSpace) <> (x ^. _localSpace)) <$>
    g (t & _localSpace .~ Origin )

instance Trans a => HasPosition (ObjectN a) a where
  _pos = _localSpace._pos

instance Trans a => Transformable (ObjectN a) a where
  transform t o = o & _localSpace %~ transform t

instance Trans a => HasMesh (ObjectN a) a where
  _mesh g (SolidObj s a) = (MeshObj ?? a) <$> g (toMesh s)
  _mesh g (MeshObj m a) = (MeshObj ?? a) <$> g m
  _mesh g (MeshRefObj _ m a) = (MeshObj ?? a) <$> g m
  -- _mesh g (InstanceObj a) = InstanceObj <$> _localSpace g a
  _mesh g (DummyObj a) = (MeshObj ?? a) <$> g EmptyMesh
  _mesh g (CullObj a) = (MeshObj ?? a) <$> g EmptyMesh

instance Trans a => ToMesh (ObjectN a) a where
  toMesh (SolidObj s a) = transform (a ^. _localSpace) (toMesh s)
  toMesh (MeshObj s a) = transform (a ^. _localSpace) s
  toMesh (MeshRefObj _ m a) = transform (a ^. _localSpace) m
  toMesh (InstanceObj a) = toMesh a
  toMesh DummyObj{} = EmptyMesh
  toMesh CullObj{} = EmptyMesh

instance Trans a => HasFaces (ObjectN a) a where
  faces  = _mesh.faces

instance Trans a => Extent (ObjectN a) a where
  extent v o@(SolidObj s _) = extentR v o s
  extent v o@(MeshObj s _) = extentR v o s
  extent v o@(MeshRefObj _ s _) = extentR v o s
  extent v (InstanceObj a) = extent v a
  extent _ DummyObj{} = 0
  extent _ CullObj{} = 0

instance Trans a => HasCenter (ObjectN a) a where
  _center g o =
    let c = transform (o ^. _localSpace) (o ^. _mesh._center)
    in (\c' -> let d = c' - c in translate d o) <$> g c

instance Trans a => Subdiv (Object a) a where
  type instance SubdivT (Object a) = Object a

  subdiv v = over _Wrapped' (objsubdivTr v)


nix :: Trans a => (Int, Int, Int) -> Traversal' (Object a) (Object a)
nix i f o@(Object (Node n ns)) = g $ getIndex (n ^. _attributes)
    where
      g (Just ai) =
        let i2 = Data.Ix.index ai i
        in Object . Node n <$> (ix i2._Unwrapped') f ns
      g _ = pure o

nixs :: Trans a => IndexedTraversal' (Int, Int, Int) (Object a) (Object a)
nixs f o@(Object (Node n _)) = g $ getIndex (n ^. _attributes)
    where
      ii = (!!) . Data.Ix.range
      g (Just ai) = reindexed (ii ai) nixs' f o
      g _ = pure o

nixs' :: Trans a => IndexedTraversal' Int (Object a) (Object a)
nixs' f o@(Object (Node n ns)) = g $ getIndex (n ^. _attributes)
    where
      g (Just _) = Object . Node n <$> (traversed._Unwrapped') f ns
      g _ = pure o

ncolumn :: Trans a => Int -> Traversal' (Object a) (Object a)
ncolumn i = nixs.indices (\(x, _, _) -> x == i)

nrow :: Trans a => Int -> Traversal' (Object a) (Object a)
nrow i = nixs.indices (\(_, y, _) -> y == i)

nslice :: Trans a => Int -> Traversal' (Object a) (Object a)
nslice i = nixs.indices (\(_, _, z) -> z == i)


objsubdivTr :: Trans b => V3 [b] -> Tree (ObjectN b) -> Tree (ObjectN b)
objsubdivTr v o' = case o' of
  (Node so@(SolidObj o _) ch) ->
    (Node (dummy so v) . (++ ch) .
     fmap (unObject . attribute att . solidObj) .
     subdiv v) o
  (Node so@(MeshObj o _) ch) ->
    (Node (dummy so v) . (++ ch) .
      fmap (`Node` []) . fmap ( recenter . (MeshObj ??) att) .
      toListOf faces . subdiv v) o
  o -> o
  where
    att = inheritedAttributes (o' ^. root)
    dummy o v = DummyObj $ TransformAtr (o ^. _localSpace) <> iAtr v
    iAtr (V3 x y z) = IndexAtr ((0,0,0), (mI x, mI y, mI z))
    mI i = max 0 (length i - 2)

inheritedAttributes :: (Trans a, HasAttributes s a) => s -> Attribute a
inheritedAttributes s =
  let
    att = s ^. attributes.filtered ff
    ff ColourAtr{} = True
    ff AlphaColourAtr{} = True
    ff SpecularAtr{} = True
    ff MaterialAtr{} = True
    ff (Param a) = ff a
    ff _ = False
  in att


liftMeshF :: (Trans a, Applicative f)
             => Traversal' (Object a) (Mesh a)
             -> (Object a -> f (Object a)) -> Object a -> f (Object a)
liftMeshF l f o =
  let (dMesh, eNode) = l <<.~ EmptyMesh $ o
      att = inheritedAttributes o
      newNode = reorient0 . recenter $ MeshObj dMesh att
  in attach eNode <$> f (leaf newNode)

liftMesh :: (Trans a, Integral i)
           => i -> (Object a -> Object a) -> Object a -> Object a
liftMesh i f = runIdentity . liftMeshF
                 (rootN._mesh. imx (fromIntegral i)) (Identity . f)


-- creates tons of EmptyMesh
liftFacesF :: (Trans a, Applicative f)
             => Traversal' (Mesh a) (Mesh a)
             -> (Object a -> f (Object a)) -> Object a -> f (Object a)
liftFacesF len f (Object (Node n ls)) =
  let li = toListOf (_mesh.faces.len) n
      li' = reorient0 . recenter . (MeshObj ?? att) <$> li
      att = inheritedAttributes n
      n' = _mesh.faces.len .~ EmptyMesh $ n
  in Object . Node n' . (++ ls) . fmap unObject <$>
     traverse (f . leaf) li'

liftFaces :: Trans a => Traversal' (Mesh a) (Mesh a) -> Object a -> Object a
liftFaces l = runIdentity . liftFacesF l Identity


attachOnSurface :: (Trans b, Extent t b, SpatialT t ~ Object b, Transformable t b)
                => Traversal' (Object b) (Mesh b) -> t -> Object b -> Object b
attachOnSurface l a o = attach o (onSurface (spatial a) (o ^. l))

attachOntoSurface :: (Trans b, Extent t b, SpatialT t ~ Object b, Transformable t b)
                  => Traversal' (Object b) (Mesh b) -> t -> Object b -> Object b
attachOntoSurface l a o = attach o (ontoSurface (spatial a) (o ^. l))


recenter :: (Trans a, HasLocalSpace t a, HasMesh t a) => t -> t
recenter t =
  let c = t ^._mesh._center._pos
  in t & _mesh %~ translate (-c) & _localSpace._local %~ translate c

reorient :: (Trans a, HasLocalSpace t a, HasMesh t a) => Transform a -> t -> t
reorient tr t =
  t & _mesh %~ transform (inv tr) & _localSpace._local %~ transform tr

reorient0 :: (Trans a, HasLocalSpace t a, HasMesh t a) => t -> t
reorient0 t = reorient (t ^. _mesh.to faceSpace.to m33_to_m44.to matrixT) t

rotateL :: Trans a => Object a -> Object a
rotateL = reorient0 . over _mesh Mesh.rotateL

rotateR :: Trans a => Object a -> Object a
rotateR = reorient0 . over _mesh Mesh.rotateR


world :: Trans a => Lens' (Object a) (Object a)
world g o = unworldSpace <$> g (worldSpace o)

worldSpace :: Trans a => Object a -> Object a
worldSpace (Object o) = Object (go Origin o)
    where
      go t (Node n ns) = let t' = t <> n ^. _localSpace
                         in Node (set _localSpace t' n) (go t' <$> ns)

unworldSpace :: Trans a => Object a -> Object a
unworldSpace (Object o) = Object (foldTree f o)
    where
      f a bs = Node a
               (over (each.root) (transform (inv (a ^. _localSpace))) bs)

_leaves :: Trans a => Traversal' (Object a) (Object a)
_leaves g o@(Object (Node n ns))
  | null ns  = g (leaf n)
  | otherwise = (children._leaves) g o


light :: Trans a => LightOptions a -> Object a
light lo = dummyObj & pointLight lo

cube :: Trans a => a -> Object a
cube a = solidObj (Cube a)

cubeS :: Trans a => a -> a -> a -> Object a
cubeS x y z = solidObj (CubeS (V3 x y z))

icosahedron :: Trans a => a -> Object a
icosahedron a = solidObj (Icosahedron a)

cylinder :: Trans a => a -> a -> Object a
cylinder a1 a2 = solidObj (Cylinder a1 a2)


makeLenses ''Prefab
