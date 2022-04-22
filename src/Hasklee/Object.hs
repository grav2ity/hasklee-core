module Hasklee.Object
  ( Object(..)
  , ObjectN(..)
  , rootN
  , children, _leaves
  , leaf, leaves
  , attach
  , meshObj, solidObj, dummyObj
  , dummy
  , nix, nixs
  , ncolumn, nrow, nslice
  , liftMeshF, liftMesh
  , liftFacesF, liftFaces
  , attachOnSurface, attachOntoSurface
  , recenter, reorient, reorient0
  , rotateL, rotateR
  , world
  , light
  , cube, cubeS, icosahedron, cylinder
  ) where

import Hasklee.Object.Internal
