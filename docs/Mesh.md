# Mesh

Hasklee has a naive `Mesh` data with no type distinction between a face and a `Mesh`, and without any adjacency information. This makes the `Mesh` manipulation severely limited and a total `Mesh` overhaul is the first bullet point on the `Hasklee2` wish-list.

## Creating Mesh

You can create some primitives with:

```haskell
triangleMesh, triangleMe :: (Epsilon a, Floating a) => a -> Mesh a
squareMesh, squareMe :: (Epsilon a, Floating a) => a -> Mesh a
rectangleMesh, rectangleMe :: (Epsilon a, Floating a) => a -> a -> Mesh a

```

'Shapes', 'Solids' and 'Pipes' can be turned into 'Mesh' with a 'toMesh' function. This will use some default settings for a conversion.

Things that yield `Vertices` can be used with `weave` family functions to build a `Mesh`.

```haskell
weaveMeshSeq :: (Epsilon a, Floating a, Foldable f,
                 ToVertices b a, Default (VertexOptions b))
             => f b -> Mesh a
```

this is smiliar to extruding multiple times, so that

```haskell
let c = Circle 1 Origin
    s = flip translateZ c <$> [0,1..10]
    m = weaveMeshSeq s
```

will produce a 10-segment cylinder.

## Navigating Mesh

`Mesh` forms a Monoid and while some `Meshes` will end up being a haphazard concatenation of submeshes, the well-behaved mesh construction functions (`weave, subdivide`) will produce a nested `MeshSeq` data, that is, sort of a grid. You can access nth row of that grid with `imx n`, nth column with `mseq.imx n` and mth element in nth row with `imx n.imx m`   
(or the other way around)

```haskell
imx :: Num a => Int -> Traversal' (Mesh a) (Mesh a)
mseq :: Num a => Traversal' (Mesh a) (Mesh a)
```

Contrast that with `ix n` which will simply produce nth face, whatever that may happen to be.

## Modifying Mesh

`Closed Mesh` will replicate its first `Vertex` at the end, which is what many functions expect. If there is a face missing there is good change that `closedMesh` is needed.

```haskell
closedMe, closedMesh :: Mesh a -> Mesh a

```

Then there are self-explanatory `flipSided` and `doubleSided` functions:

```haskell
flipSided :: HasMesh t a => t -> t
doubleSided :: HasMesh t a => t -> t
```

'vmap' maps over all `Vertices`:

```haskell
vmap :: (Vertex a -> Vertex a) -> Mesh a -> Mesh a

```

