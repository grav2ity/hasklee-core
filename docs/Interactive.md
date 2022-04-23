# Interactive behaviour

## IDs

`newID` will give you an `ID` number that needs to be applied to an `Object` with `rID` function:

```haskell
newID :: NScene a RealID
rID :: HasAttributes t a => RealID -> t -> t
```

Most interactive `Objects` will need this.

`RealID` is just an `Int`.  
`IDs` start from 1.  
Peeking `nextid` is useful in some situations.

```haskell
      ni <- use nextid
```
## Components

Interactive behaviour is brought about by `Components.`

`Components` live in an external engine. They will most likely be written in C# or C++ (if `Hasklee` ever gets to supporting Unreal Engine) and can do whatever the programmer's heart desires. Most `Components`, however, will want to interact with other `Objects` and it's `Hasklee's` role to feed them `IDs`. 

There are two ways of wiring interactive behaviour.

#### First mode

First one includes Lua scripting and is appropriate for low cost events that do not target many `Objects` and do not need to run computations every (n-th) frame. This will include most of direct user interactions.

In this mode `Objects` themselves are marked as sending or receiving messages to/from a given `ID`.

Use `actionS` on an `Object` that is a sender i.e. has some `Component` that is programmed to send messages. It will send/execute Lua code on the target `ID`.

Use `actionR` on an `Object` that needs to receive some message. It will receive/execute Lua code whenever any message is sent its way from the source `ID`.

```haskell
actionS, actionS' :: HasAttributes t a => RealID -> String -> t -> t
actionR, actionR' :: HasAttributes t a => RealID -> String -> t -> t
```

**Note** that these do not work in tandem and you do not call `actionR` to receive messages sent by `actionS`, It's just more convenient to use one or the other.

#### Second mode

In the second wiring mode `Components` will do all the work on their own and by convention (i.e. way I was doing it so far) they will look for extra `ID` data in their containing `Object`. Use `rIDT` and `rIDGraph` to write an `ID` table and graph.

```haskell
rIDT, rIDT' :: HasAttributes t a => [RealID] -> t -> t
rIDGraph, rIDGraph' :: HasAttributes t a => AG.Graph Int -> t -> t
```
