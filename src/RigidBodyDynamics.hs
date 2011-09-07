module RigidBodyDynamics ( Influence(..)
                         , Shape(..)
                         , defaultShape
                         , vertices
                         , RigidBody(..)
                         , defaultRigidBody
                         , addInfluence
                         , removeInfluence
                         , stepTime
                         ) where

import Data.List (delete, foldl')
import Numeric.LinearAlgebra

data Influence = Thruster { poa :: Vector Double -- point of application
                          , vec :: Vector Double -- force vector
                          } deriving (Show, Eq)
               -- | Tractor { } -- pulls a point of application to a point of origin
               -- | Gravity { } -- Tractor, but gets lesser with distance
               -- | RubberBand { } -- Tractor, but gets greater with distance

data Shape = Rectangle { width  :: Double
                       , height :: Double
                       } 
           | Circle    { radius :: Double
                       } 
           | Polygon   { points :: [Vector Double]
                       }
                       deriving (Show)

defaultShape = Rectangle 40 10

vertices :: Shape -> [Vector Double]
vertices (Rectangle w h) = [2 |> [(-halfW), halfH],
                            2 |> [halfW, halfH],
                            2 |> [halfW, (-halfH)],
                            2 |> [(-halfW), (-halfH)]]
 where
    halfW = w/2
    halfH = h/2

data RigidBody = RigidBody { mass            :: Double
                           , centerMass      :: Vector Double
                           , orientation     :: Double  -- in radians, east = 0, north = pi/2
                           , linVelocity     :: Vector Double
                           , linAcceleration :: Vector Double
                           , angVelocity     :: Double
                           , angAcceleration :: Double
                           , shape           :: Shape
                           , influences      :: [Influence]
                           } deriving (Show)

defaultRigidBody = RigidBody { mass = 1
                             , centerMass = 2 |> [0,0]
                             , orientation = 0
                             , linVelocity = 0
                             , linAcceleration = 0
                             , angVelocity = 0
                             , angAcceleration = 0
                             , shape = defaultShape
                             , influences = []
                             }

momentOfInertia :: RigidBody -> Double
momentOfInertia body = momentOfInertia' (mass body) (shape body)

momentOfInertia' :: Double -> Shape -> Double
momentOfInertia' m (Rectangle w h) = m * (w**2 + h**2) / 12

addInfluence :: Influence -> RigidBody -> RigidBody
addInfluence i b = b { influences = i : (influences b) }

removeInfluence :: Influence -> RigidBody -> RigidBody
removeInfluence i b = b { influences = delete i (influences b) }

stepTime :: Double -> RigidBody -> RigidBody
stepTime t body = newBody { centerMass  = r
                          , orientation = theta
                          , linVelocity = v
                          , angVelocity = omega
                          , linAcceleration = 0
                          , angAcceleration = 0
                          }
 where
    newBody = foldl' applyInfluence body (influences body)
    r0 = centerMass newBody
    v0 = linVelocity newBody
    a = linAcceleration newBody
    theta0 = orientation newBody
    omega0 = angVelocity newBody
    alpha = angAcceleration newBody
    -- This isn't too complicated, so we can solve it analytically, instead of
    -- numerically.
    r = (scale (t**2/2) a) + scale t v0 + r0
    v = (scale t a) + v0
    theta = (t**2/2)*alpha + t*omega0 + theta0
    omega = t*alpha + omega0

-- perpendicular dot product: 2D equivalent of the cross product
-- rotate u (pi/2) radians and take the dot product with it and v
-- Alternatively, if u and v are extended to 3-dimensional vectors, perpDot is
-- the z value of the cross product of u and v
u `perpDot` v = (u @> 0) * (v @> 1) - (u @> 1) * (v @> 0)

-- rotate a given vector by a given angle
rotate v theta = (2><2) [cos theta, (-sin theta), sin theta, cos theta] <> v

-- intuitively it should be Influence -> RigidBody -> RigidBody, but then I'd
-- have to flip it in the foldl' above
applyInfluence :: RigidBody -> Influence -> RigidBody
applyInfluence body (Thruster p v) = body { linAcceleration = newLinAccel
                                          , angAcceleration = newAngAccel
                                          }
 where
    a = linAcceleration body
    m = mass body
    alpha = angAcceleration body
    mi = momentOfInertia body
    newLinAccel = a + scale (1/m) (rotate v (orientation body))
    newAngAccel = alpha + (p `perpDot` v)/mi
