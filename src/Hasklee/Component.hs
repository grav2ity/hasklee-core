module Hasklee.Component where

import qualified Algebra.Graph as AG
import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import qualified Data.Text as T
import Linear

import Hasklee.Transform


class GetName f where
  getName :: f a -> String

instance Datatype t => GetName (D1 t f) where
  getName  = datatypeName


data Component a = GraphPropagate Int Int a
                 | IdGraphRC (AG.Graph Int)
                 | GraphConductor Int
                 | Drag a
                 | DragC a
                 | DragSelf a
                 | Bell
                 | StringPluck
                 | Button
                 | Key
                 | Click
                 | AntennaUnfold a
                 | Rigidbody Bool Bool Double
                 | ConfigurableJointN
                   { jointIDN :: Int
                   , jAnchor :: Maybe (V3 a)
                   , jAxis :: Maybe (V3 a)
                   , lMotionN :: Maybe (JointMotionType, JointMotionType, JointMotionType)
                   , lLimitN :: Maybe (Float, Float, Float)
                   , lSpringN :: Maybe (Float, Float)
                   , aMotionN :: Maybe (JointMotionType, JointMotionType, JointMotionType)
                   , aLimitXH :: Maybe (Float, Float, Float)
                   , aLimitXL :: Maybe (Float, Float, Float)
                   }
                 | FixedJointN Int
                 | Rotator (V3 a) (V3 a) a a Bool
                 | ParentRotator
                 | Translator (V3 a) a a Bool
                 | Slider a a
                 | Stop
                 | CameraNode (Transform a)
                 | Proximity Int (V3 a) a
                 | LuaComponent T.Text T.Text
                 | CustomComponent T.Text L.ByteString

                 | ParamC (Component a)
                 deriving (Binary, Eq, Generic, NFData, Show)

data JointMotionType = JointLocked | JointLimited | JointFree
               deriving (Binary, Enum, Eq, Generic, NFData, Show)


-- ORHPANS

instance Binary a => Binary (AG.Graph a)
