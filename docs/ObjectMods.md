# Object Modifiers

_more of a sales pitch_

`Object modifiers` are functions with a signature that would look something like:

```haskell
... -> Object a -> Object a

```

or if we think of a `Mesh` as a simple `Object`:

```haskell
... -> Mesh a -> Object a
```

That looks innocent enough but there is a lot that can happen.

In the [_Modern Harp_](https://youtu.be/mSCImsBpFeo) demo not only a single instrument is created but (with a couple of lines of code alteration) an `Object modifier` that turns any (reasonable) `Mesh` into a harp. Now imagine a library of hundred similar functions and a total creative pandemonium that would ensue.

## Mesh lock

One such modifier I was hell-bent on. It turns a simple `Mesh` into one that can be _unlocked_ and _unfolded_ ... or since one gif is worth something:

![mesh lock](https://s8.gifyu.com/images/meshlock.gif)

This turned out to be much more complex that I expected and the current implementation is not close to being correct, as it only allows for a single-width strip to be unfolded at a time.

## Antenna unfold

A more reasonable modifier that makes a stacking dolls version of an `Object` and allows to extend them with a mouse drag.

![spiral unfold](https://s8.gifyu.com/images/spiralunfold.gif)

It also sends an event message and it could be used to make an interactive radio that tunes in as its antenna is extended or a sun with extensible rays.

![dark sun](https://s8.gifyu.com/images/darksun.gif)

need a better lit version of that.
