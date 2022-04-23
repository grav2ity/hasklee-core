# Objects

_To make it perfectly clear nobody here misses OOP_

The basic building block of `Hasklee` are `Objects`. These will usually have some mesh geometry, a `Transform`, some other `Attributes` (which are passive characteristics like colour and material) and `Components` that make up the `Object's` interactive behavior.

Each `Object` is in fact a tree of `Object Nodes`. The goal is to isolate the end user from this distinction as much as possible, but for now expect some chaotic inconsistencies.


## Creating Objects


`meshObj` creates an `Object` from _anything_ that can be converted to a `Mesh`:

```haskell
meshObj :: (ToMesh t a, Default (MeshOptions t)) => t -> Object a
```

if that _thing_ happens to be a `Solid` you might want to use `solidObj` to keep the `Solid` around for further operations (e.g. subdivision):

```haskell
solidObj :: Trans a => Solid a -> Object a
```

`dummyObj` does not have any visible geometry but is useful for creating hierarchy and positioning things like lights or sound sources


```haskell
dummyObj :: Object a
```

## Building Object Hierarchy

`Objects` form a monoid but each `<>` application introduces new `DummyObj` as the parent of the operands. Use `mconcat` to flock all elements under a single dummy.

`attach` sets the first one as the parent of the second:

```haskell
attach :: Object a -> Object a -> Object a
```

## Traversing Objects

To traverse direct children of the `Object's` root:
```haskell
children :: Traversal' (Object a) (Object a)
```

To traverse nodes with no children:
```haskell
_leaves :: Trans a => Traversal' (Object a) (Object a)
```

**`Object's Transform` is relative to its parent.**  
To access `World Space` use:

```haskell
world :: Trans a => Lens' (Object a) (Object a)
```

This has to be done before everything else like so:

```haskell
world.children
```

If you have subdivided an `Object`.  
To access a child by its subdivision index:

```haskell
nix :: Trans a => (Int, Int, Int) -> Traversal' (Object a) (Object a)
```

For an indexed traversal:

```haskell
nixs :: Trans a => IndexedTraversal' (Int, Int, Int) (Object a) (Object a)
```

## Face Lifting

The focus of `Hasklee` is interactivity. Mesh faces are not very interactive, `Objects` are. So we want to turn faces into new `Objects` and attach them as children.

`liftFaces` will do just that for all the targets of the traversal:

```haskell
liftFaces :: Trans a
          => Traversal' (Mesh a) (Mesh a)
          -> Object a -> Object a
```
It's a simplified version of:

```haskell
liftFacesF :: (Trans a, Applicative f)
           => Traversal' (Mesh a) (Mesh a)
           -> (Object a -> f (Object a)) -> Object a -> f (Object a)
```

`liftMeshF` is similar but it will gather all the targeted mesh into a single new object.

```haskell
liftMeshF :: (Trans a, Applicative f)
          => Traversal' (Object a) (Mesh a)
          -> (Object a -> f (Object a)) -> Object a -> f (Object a)
```

`liftFaces` targets root's mesh whereas `liftMesh` can pick out from the entire hierarchy.

## Object's Data

`_mesh` lens targets root's mesh while `toMesh` will return whole mesh in `World Space`.
