# hasklee

Hasklee is a library for creating interactive 3D objects / scenes.

Its creations are brought to life trough external 3D engines. Currently only [Unity](https://unity.com/) is supported via [HaskleeUnity](https://github.com/grav2itty/HaskleeUnity) package.


Hasklee features:

- some pretty naive mesh construction facilities
- simple animation
- power of Csound via [csound-expression](https://github.com/spell-music/csound-expression) library
- interactive behavior with some built-in or custom components
- Lua scripting
- nested prefabs

Hasklee is a proof of concept toy. It cannot be easily integrated with existing
projects / different workflows and it's rather unusable without some additional 'live programming' infrastructure.
(but see the [demos](https://www.youtube.com/watch?v=mSCImsBpFeo&list=PL5xs7Mc75HThPj_onhH3ozA--xa50NwO6) for an interactive Emacs session)

That said, I’m currently attempting to make a ‘game’ with it.


## Builidng

- Please use [Stack](https://www.haskellstack.org)
- Start new stack project
- Git clone into project's folder
    - [https://github.com/grav2itty/hasklee.git](https://github.com/grav2itty/hasklee.git)
    - (optional) [https://github.com/grav2itty/hasklee-examples.git](https://github.com/grav2itty/hasklee-examples.git)
- Get **stack.yaml** from [https://github.com/grav2itty/hasklee-stack.git](https://github.com/grav2itty/hasklee-stack.git)  
  Uncomment **hasklee-examples** if present


## Usage

- Please refer to the [examples](https://github.com/grav2itty/hasklee-examples)
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
* [Interactive behaviour](docs/Interactive.md)
* [Prefabs and instances](docs/Prefabs.md)
* [Mesh](docs/Mesh.md)
* Transforms
* Scenes
* Attributes
* Components
* Lua scripting
* Sound
* Subdivision
* Shapes and Solids
* Pipes
* Randomizing
* Animation
