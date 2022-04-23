# Prefabs and instances

Please note that the term _instance_ refers to an instantiation (copy) of a `Prefab` and not to a Haskell instance.

If your scene has hundreds of same or similar `Objects` you might want to use `Prefabs`  to limit data size and speedup loading times (in theory).

If you build a self-contained interactive machine and turn it into a `Prefab` you can then make any number of copies and all should work correctly and independently of each other.

A  machine part that expects interaction with other `Objects` can be made into a `Prefab` as well and a bigger machine built from its copies.

## Pure Prefabs

The simplest case is a:

```haskell
prefab0 :: Trans a
        => Object a
        -> NScene a (Object a)
```

get it out of the Monad and use it like any other `Object`.

`Prafab` instances don't have to be identical but can be customized with different attributes. Use `prefab1` -  `prefab4` to turn a function taking any type of arguments into a prefabbed version.


```haskell
prefab1 :: Trans a
        => (b -> Object a) -> b
        -> NScene a (b -> Object a)
```

for example:

```haskell
  let mo = solidObj $ Cube 1
      -- customize colour
      f c = mconcat . rend0 $ replicateX 4 (colour' c mo)
  -- black as a placeholder
  pf <- prefab1 f CN.black
  newObj . mconcat . rend0 . catY $ pf <$> [CN.white, CN.gray, CN.black]

```

Inside a function that you want to use for a `Prefab` you need to use a `'` for all `Attributes` that you want to customize. Thus `colour'` instead of `colour`. This is one annoying caveat that I wasn't able to overcome.

**`component'` must be used inside prefabbed function**

You can do whatever you want in a prefabbed function as long as the resulting `Object` maintains a constant structure i.e. the same number of children connected in the same way.

It's possible to have nested `Prefabs`. Extending previous example:

```haskell
  let mo = solidObj $ Cube 1
      f c = mconcat . rend0 $ replicateX 4 (colour' c mo)
  pf <- prefab1 f CN.black
  let f2 c = mconcat . rend0 . catY $ pf <$> [CN.white, c, CN.black]
  pf2 <- prefab1 f2 CN.black
  newObj . mconcat . rend0 . catX $ pf2 <$> [CN.navy, CN.orange, CN.azure]
```


## Monadic Prefabs

If your `Object` does any interactivity or needs to be created inside a Monad for whatever reason use `prefabF0` - `prefabF4` functions:

```haskell
prefabF0 :: Trans a
         => NScene a (Object a)
         -> NScene a (NScene a (Object a))

prefabF1 :: Trans a
         => (b -> NScene a (Object a)) -> b
         -> NScene a (b -> NScene a (Object a))

```

This way you can also have randomized `Prefabs`.
