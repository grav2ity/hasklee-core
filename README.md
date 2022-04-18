# hasklee

Hasklee is a library for creating interactive 3D objects / scenes.

Its creations are brought to life trough external 3D engines. Currently only [Unity](https://unity.com/) is supported via [HaskleeUnity](https://github.com/grav2ity/HaskleeUnity) package.


Hasklee features:

- some pretty naive mesh construction facilities
- simple animation
- power of Csound via [csound-expression](https://github.com/spell-music/csound-expression) library
- interactive behavior with some built-in or custom components
- Lua scripting
- nested prefabs

Hasklee is a proof of concept toy. It cannot be easily integrated with existing
projects / different workflows and it's rather unusable without some additional 'live programming' infrastructure.
(but see the [demos](https://www.youtube.com/watch?v=5WwlWrbiRDQ&list=PLWKxYNIEfPqi3Cst9RTsISGoeFYp5ZXQw) for an interactive Emacs session)

That said, I’m currently attempting to make a ‘game’ with it.


## Builidng

- Please use [Stack](https://www.haskellstack.org)
- Start new stack project
- Git clone into project's folder
    - [https://github.com/grav2ity/hasklee.git](https://github.com/grav2ity/hasklee.git)
    - (optional) [https://github.com/grav2ity/hasklee-examples.git](https://github.com/grav2ity/hasklee-examples.git)
- Get **stack.yaml** from [https://github.com/grav2ity/hasklee-stack.git](https://github.com/grav2ity/hasklee-stack.git)  
  Uncomment **hasklee-examples** if present


## Usage

- Please refer to the [examples](https://github.com/grav2ity/hasklee-examples)
- Scenes in Hasklee are constructed inside `NScene` monad as

        NScene r ()

    or

        NScene r (Object r)

     with r being `Float` or `Double`

- use `binIO` to convert `NScene` into [`Builder`](https://hackage.haskell.org/package/bytestring-0.10.12.0/docs/Data-ByteString-Builder.html) or directly write to file with `writeScene`
