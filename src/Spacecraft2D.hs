module Spacecraft2D ( Spacecraft2D(..)
                    , defaultSpacecraft2D
                    , startLeft
                    , startRight
                    , stopLeft
                    , stopRight
                    ) where

import RigidBodyDynamics
import Numeric.LinearAlgebra

data Spacecraft2D = Spacecraft2D { body           :: RigidBody
                                 } deriving (Show)

defaultSpacecraft2D = Spacecraft2D defaultRigidBody

-- FIXME: the {start,stop}{Left,Right} functions are pretty hacky
-- come up with a better system

startLeft :: Spacecraft2D -> Spacecraft2D
startLeft s = s { body = addInfluence leftThruster (body s) }
 where
    leftThruster = Thruster (2 |> [(-(w/2)), 0]) (2 |> [0,1])
    w = width . shape . body $ s

startRight :: Spacecraft2D -> Spacecraft2D
startRight s = s { body = addInfluence rightThruster (body s) }
 where
    rightThruster = Thruster (2 |> [w/2, 0]) (2 |> [0,1])
    w = width . shape . body $ s

stopLeft :: Spacecraft2D -> Spacecraft2D
stopLeft s = s { body = removeInfluence leftThruster (body s) }
 where
    leftThruster = Thruster (2 |> [(-(w/2)), 0]) (2 |> [0,1])
    w = width . shape . body $ s

stopRight :: Spacecraft2D -> Spacecraft2D
stopRight s = s { body = removeInfluence rightThruster (body s) }
 where
    rightThruster = Thruster (2 |> [w/2, 0]) (2 |> [0,1])
    w = width . shape . body $ s
