# Lua scripting

`Game Object` roughly refers to the `Object's` real-time equivalent in the the external 3D engine.

Lua is currently used in four places: `actionS`, `actionR`, `luaAwake`, `luaStart`.

```haskell
actionS, actionS' :: HasAttributes t a => RealID -> String -> t -> t
actionR, actionR' :: HasAttributes t a => RealID -> String -> t -> t
luaAwake, luaAwake' :: HasAttributes t a => String -> t -> t
luaStart, luaStart' :: HasAttributes t a => String -> t -> t
```

For the use of `actionsS` and `actionR` see [Interactive behaviour](Interactive.md).

Use `luaAwake` and `luaStart` for any custom initialization routines. The difference between the two is similar to the difference between `Unity's` `Awake` and `Start` events. `luaStart` is called after all `Game Objects` have been created and all existing `luaAwake` scripts have been called. You can safely access other `Game Objects` in `luaStart`. This is not the case with `luaAwake`.

Every `Game Object` involved with scripting is associated with a unique Lua table that can be accessed with `self`.

You can use `self` to store any additional data.

## globals

* `luaObjects[n]` - Lua table by the `Game Object's` ID number

## actionS and actionR

Inside `actionS` and `actionR` you can use the following unqualified names:

* `sender` - for the Lua table of the `Game Object` sending the message/event
* `event` - for the string name of the event
* `args[1]`, `args[2]`, `args[n]` - for the nth additional argument

See [Components](Components.md) for event names and arguments.

## self.gameObject

`self.gameObject` and its shorter version `self.go` are the main entry points to `Game Objects'` data and methods. Since the stuff available was being added on my need-to-have basis there isn't that much to speak of. More can be easily adds and is coming soon.

The other solution would be to automatically expose `Unity's` interfaces en masse. This is not advised because of security issues, but the main reason for not doing that, is that it would directly pair all Lua code with `Unity`.

While the interfaces are very volatile and documentation is work in progress you can see the available stuff by browsing files in the [Proxies](https://github.com/grav2itty/HaskleeUnity/tree/main/Runtime/Proxies) folder of the [HaskleeUnity](https://github.com/grav2itty/HaskleeUnity) repo. [LuaControllerProxy](https://github.com/grav2itty/HaskleeUnity/blob/main/Runtime/Proxies/LuaControllerProxy.cs) in particular lists all the things that are directly available under `self.gameObject`.

