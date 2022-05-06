module Hasklee.Attribute
  ( module Hasklee.Attribute.Data
  , module Hasklee.Colour
  , module Hasklee.Component
  , getID, getIndex
  , isNamed
  , attribute, attribute'
  , component, component'
  , customComponent, customComponent'
  , actionR, actionR'
  , actionS, actionS'
  , rID
  , rIDT, rIDT'
  , rIDGraph, rIDGraph'
  , cJointN
  , tag, tag'
  , name, name'
  , material, material'
  , shader, shader'
  , unlitColor, unlitColor'
  , colour, colour', acolour, acolour'
  , specular, specular'
  , pointLight, pointLight', spotLight, spotLight', dirLight, dirLight'
  , meshLight
  , csound, csoundA, csoundA'
  , playNote
  , graphPropagate
  , vectorAtr
  , path, path'
  , dance, dance'
  , danceInstance
  , record, record'
  , luaController
  , luaStart, luaStart', luaAwake, luaAwake'
  , luaComponent, luaComponent'
  , parameter
  ) where

import qualified Algebra.Graph as AG
import Control.Lens hiding (transform)
import Control.Monad.IO.Class
import Csound.Base (RenderCsd, renderCsdBy, setRates, setSilent)
import qualified Csound.Base ((<>))
import Data.Aeson
import Data.Default.Class
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import Linear

import Hasklee.Attribute.Data
import Hasklee.Colour hiding (over)
import Hasklee.Component
import Hasklee.Mesh
import Hasklee.Transform


-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--   WARNING
--   Lights and all the other things that have orientation but do
--   not depend on mesh to change hand
--   need to have some kind of change hand implemented in here
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!


getID :: HasAttributes t a => t -> Maybe RealID
getID =  preview (attributes._RealIDAtr)

getIndex :: Attribute a -> Maybe ((Int, Int, Int), (Int, Int, Int))
getIndex = preview (each._IndexAtr)

isNamed :: HasAttributes t a => T.Text -> t -> Bool
isNamed s = isJust . findOf (_attributes.each)
  (\case { NameAtr  s' -> s == s'; _ -> False })


attribute, attribute' :: HasAttributes t a => Attribute a -> t -> t
attribute a t = t & _attributes <>~ a
attribute' a t = t & _attributes <>~ view (re parameter) a

removeAttribute :: Eq a => Attribute a -> Attribute a -> Attribute a
removeAttribute b (ListAtr l) = ListAtr $ filter (/= b) l
removeAttribute b a = if a == b then mempty else a


addComponent :: HasAttributes t a => Component a -> t -> t
addComponent a t =
  if has (_attributes._ComponentAtr) t
  then (_attributes._ComponentAtr %~ cons a) t
  else attribute (ComponentAtr [a]) t

component, component' :: HasAttributes t a => Component a -> t -> t
component = addComponent
component' b = addComponent (view (re parameterC) b)

customComponent :: (Generic s, GetName (Rep s), ToJSON s, HasAttributes t a)
                => s -> t -> t
customComponent s =
  let name = T.pack . getName . GHC.Generics.from $ s
  in component (CustomComponent name (Data.Aeson.encode s))

customComponent' :: (Generic s, GetName (Rep s), ToJSON s, HasAttributes t a)
                => s -> t -> t
customComponent' s =
  let name = T.pack . getName . GHC.Generics.from $ s
  in component' (CustomComponent name (Data.Aeson.encode s))


actionR, actionR' :: HasAttributes t a => RealID -> String -> t -> t
actionR i s = attribute (ActionRR i (CodeString . T.pack $ s))
actionR' i s = attribute' (ActionRR i (CodeString . T.pack $ s))

actionS, actionS' :: HasAttributes t a => RealID -> String -> t -> t
actionS i s = attribute (ActionSS i (CodeString . T.pack $ s))
actionS' i s = attribute' (ActionSS i (CodeString . T.pack $ s))


rID :: HasAttributes t a => RealID -> t -> t
rID i = attribute (RealIDAtr i)

rIDT, rIDT' :: HasAttributes t a => [RealID] -> t -> t
rIDT i = attribute (RealIDTAtr i)
rIDT' i = attribute' (RealIDTAtr i)

rIDGraph, rIDGraph' :: HasAttributes t a => AG.Graph RealID -> t -> t
rIDGraph gr = component (IdGraphRC gr)
rIDGraph' gr = component' (IdGraphRC gr)


cJointN :: RealID -> Component a
cJointN a = ConfigurableJointN a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


tag, tag' :: HasAttributes t a => String -> t -> t
tag s = attribute (TagAtr . T.pack $ s)
tag' s = attribute' (TagAtr . T.pack $ s)

name, name' :: HasAttributes t a => String -> t -> t
name s = attribute (NameAtr . T.pack $ s)
name' s = attribute (NameAtr . T.pack $ s)

material, material' :: HasAttributes t a => String -> t -> t
material s = attribute (MaterialAtr $ MaterialName . T.pack $ s)
material' s = attribute' (MaterialAtr $ MaterialName . T.pack $ s)

shader, shader'
  :: HasAttributes t a
  => String -> Maybe Texture -> MaterialOptions a -> t -> t
shader s t opt = attribute (MaterialAtr $ Material (T.pack s) t opt)
shader' s t opt = attribute' (MaterialAtr $ Material (T.pack s) t opt)

unlitColor, unlitColor' :: Fractional a => HasAttributes t a => t -> t
unlitColor = shader "Unlit/Color" Nothing def
unlitColor' = shader' "Unlit/Color" Nothing def


colour, colour' :: (HasAttributes t a, ToColour c a) => c -> t -> t
colour c = attribute (ColourAtr (_colour c))
colour' c = attribute (Param (ColourAtr (_colour c)))

acolour, acolour' :: (HasAttributes t a, ToAlphaColour c a) => c -> t -> t
acolour c = attribute (AlphaColourAtr (_alphaColour c))
acolour' c = attribute (Param (AlphaColourAtr (_alphaColour c)))

specular, specular' :: (HasAttributes t a, ToColour c a) => c -> t -> t
specular c = attribute (SpecularAtr (_colour c))
specular' c = attribute (Param (SpecularAtr (_colour c)))


pointLight, pointLight' :: HasAttributes t a => LightOptions a -> t -> t
pointLight lo = attribute (LightAtr PointLight lo)
pointLight' lo = attribute (LightAtr PointLight lo)

spotLight, spotLight' :: HasAttributes t a => LightOptions a -> t -> t
spotLight lo = attribute (LightAtr SpotLight lo)
spotLight' lo = attribute (LightAtr SpotLight lo)

dirLight, dirLight' :: HasAttributes t a => LightOptions a -> t -> t
dirLight lo = attribute (LightAtr DirectionalLight lo)
dirLight' lo = attribute (LightAtr DirectionalLight lo)

meshLight, meshLight' :: HasAttributes t a => LightOptions a -> t -> t
meshLight lo = attribute (LightAtr MeshLight lo)
meshLight' lo = attribute (LightAtr MeshLight lo)


csound :: (RenderCsd a, MonadIO m) => a -> m T.Text
csound = liftIO . fmap T.pack . renderCsdBy (setRates 48000 32 Csound.Base.<> setSilent)

csoundA, csoundA' :: HasAttributes t a => T.Text -> t -> t
csoundA s = attribute (CsoundInline s)
csoundA' s = attribute' (CsoundInline s)

playNote :: String -> Int -> Int -> String
playNote name pitch volume = "self.csound:event('i " ++ show name ++ " 0 1 1 " ++ show pitch ++ " " ++ show volume ++ "')"

graphPropagate :: Int -> String
graphPropagate start = "self.go.graphPropagate.PropagateFrom(args[1], " ++ show start ++ ")"


vectorAtr, vectorAtr' :: HasAttributes t a => String -> V3 a -> t -> t
vectorAtr s t = attribute (VectorAtr (T.pack s) t)
vectorAtr' s t = attribute' (VectorAtr (T.pack s) t)


path, path'
  :: (Trans a, HasLocalSpace t a, HasAttributes s a)
  => String -> t -> s -> s
path s t = attribute $ toPath s (view _localSpace t)
path' s t = attribute' $ toPath s (view _localSpace t)

dance, dance'
  :: (Trans a, HasLocalSpace t a, HasAttributes s a)
  => String -> t -> DanceOptions a -> s -> s
dance s t op = attribute (toDance s (view _localSpace t)){dOptions = op}
dance' s t op = attribute' (toDance s (view _localSpace t)){dOptions = op}

danceInstance :: (Trans a, HasAttributes t a) => String -> DanceOptions a -> t -> t
danceInstance s o = attribute' (DanceInstance (T.pack s) o)

record, record'
  :: (Trans a, HasAttributes t a, HasLocalSpace t a, HasLocalSpace s a)
  => String -> DanceOptions a -> (t -> s) -> t -> t
record s op f t = dance s (f t) op t
record' s op f t = dance' s (f t) op t

toPath :: Trans a => String -> Transform a -> Attribute a
toPath s (TimedT t) =
  let
    l = Map.toList t
    trs = snd <$> l
    times = snd . mapAccumL (\a b -> (b, b - a)) 0 $ fst <$> l
  in PathAtr (T.pack s) $ zipWith DanceStep trs times
toPath s _ = PathAtr (T.pack s) []

toDance :: Trans a => String -> Transform a -> Attribute a
toDance s t =
  let (PathAtr s' t') = toPath s t
  in DanceAtr s' t' def


luaController :: HasAttributes t a => t -> t
luaController = attribute LuaController

luaStart, luaStart' :: HasAttributes t a => String -> t -> t
luaStart s = attribute (LuaCode "start" (T.pack s))
luaStart' s = attribute' (LuaCode "start" (T.pack s))

luaAwake, luaAwake' :: HasAttributes t a => String -> t -> t
luaAwake s = attribute (LuaCode "awake" (T.pack s))
luaAwake' s = attribute' (LuaCode "awake" (T.pack s))

luaComponent :: HasAttributes t a => String -> String -> t -> t
luaComponent s1 s2 = component (LuaComponent (T.pack s1) (T.pack s2))

luaComponent' :: HasAttributes t a => String -> String -> t -> t
luaComponent' s1 s2 = component' (LuaComponent (T.pack s1) (T.pack s2))


parameter :: Prism' (Attribute a) (Attribute a)
parameter = prism' to from
  where
    to :: Attribute a -> Attribute a
    to (RealIDAtr r) = RealIDAtr r
    to (Param a) = Param a
    to a = Param a

    from :: Attribute a -> Maybe (Attribute a)
    from r@RealIDAtr{} = Just r
    from (Param a) = Just a
    from (ListAtr l) =
      case catMaybes $ from <$> l of
        [] -> Nothing; x -> Just (ListAtr x)
    from (ComponentAtr l) =
      case catMaybes $ preview parameterC <$> l of
        [] -> Nothing; x -> Just (ComponentAtr x)
    from _ = Nothing

parameterC :: Prism' (Component a) (Component a)
parameterC = prism' to from
  where
    to :: Component a -> Component a
    to (ParamC a) = ParamC a
    to a = ParamC a

    from :: Component a -> Maybe (Component a)
    from (ParamC a) = Just a
    from _ = Nothing
