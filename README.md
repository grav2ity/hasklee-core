# hasklee

Hasklee is a library for creating interactive 3D objects / scenes.

Its creations are brought to life trough external 3D engines. Currently only [Unity](https://unity.com/) is supported via [hasklee-unity](https://github.com/grav2itty/hasklee-unity) package.

Hasklee features:

- some pretty naive mesh construction facilities
- simple animation
- power of Csound via [csound-expression](https://github.com/spell-music/csound-expression) library
- interactive behavior with some built-in or custom components
- Lua scripting
- nested prefabs


Some [videos](https://www.youtube.com/playlist?list=PL5xs7Mc75HThPj_onhH3ozA--xa50NwO6).

Hasklee is a proof of concept toy and it cannot be easily integrated with existing
projects / different workflows.

That said, I’m currently attempting to make a ‘game’ with it.

<!-- ## hasklee-quick -->

<!-- So I've borrowed a couple of lines from [Reanimate](https://github.com/reanimate/reanimate) and it's now possible to have a quick 'live programming' session.  -->

<!-- ```console -->
<!-- $ git clone https://github.com/grav2itty/hasklee-quick.git -->
<!-- $ cd hasklee-quick -->
<!-- $ stack repl -->
<!-- :cmd haskleeLive -->
<!-- ``` -->

<!-- Edit and save Main.hs to have the result auto refreshed. -->
<!-- [HaskleeUnity](https://github.com/grav2itty/HaskleeUnity) must be running in play mode at the receiving end. -->

## Builidng

- Please use [Stack](https://www.haskellstack.org)
- Git clone into a new directory
    - [https://github.com/grav2ity/hasklee-core.git](https://github.com/grav2ity/hasklee-core.git)
    - (optional) [https://github.com/grav2ity/hasklee-examples.git](https://github.com/grav2ity/hasklee-examples.git)
- Get **stack.yaml** from [https://github.com/grav2ity/hasklee-stack.git](https://github.com/grav2ity/hasklee-stack.git)
  Uncomment **hasklee-examples** if present

## Usage

- Please refer to the [examples](https://github.com/grav2ity/hasklee-examples)
- Scenes in Hasklee are constructed inside `NScene` monad as

    ```haskell
    NScene r ()
    ```

    or

    ```haskell
    NScene r (Object r)
    ```

     with r being `Float` or `Double`

- use `binIO` to convert `NScene` into [`Builder`](https://hackage.haskell.org/package/bytestring-0.10.12.0/docs/Data-ByteString-Builder.html) or directly write to file with `writeScene`

## Draft Docs

* Intro
* [Objects](docs/Objects.md)
* [Object Modifiers](docs/ObjectMods.md)
* [Interactive behaviour](docs/Interactive.md)
* [Prefabs and instances](docs/Prefabs.md)
* [Mesh](docs/Mesh.md)
* [Lua scripting](docs/Lua.md)
* Transforms
* Scenes
* Attributes
* Components
* Sound
* Subdivision
* Shapes and Solids
* Pipes
* Randomizing
* Animation
