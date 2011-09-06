module Renderer ( rinit
                , drawRectangle
                ) where

import Numeric.LinearAlgebra
import Graphics.Rendering.OpenGL

rinit :: Integral a => a -> a -> IO ()
rinit screenWidth screenHeight = do
    -- don't think these next three are strictly necessary
    clearColor $= Color4 0 0 0 0
    clearDepth $= 1.0
    viewport   $= (Position 0 0, Size (fromIntegral screenWidth) (fromIntegral screenHeight))
    
    matrixMode $= Projection
    loadIdentity
    -- make screen coordinates go up in the up direction
    ortho 0 (fromIntegral screenWidth) 0 (fromIntegral screenHeight) (-1) 1    -- should be ortho2D?

drawRectangle :: Vector Double -> Double -> Double -> Double -> IO ()
drawRectangle center w h o = do
    let cmx = center @> 0
        cmy = center @> 1
        halfWidth = w / 2
        halfHeight = h / 2
        vUL = Vertex3 unsafeCoerce (-halfWidth) unsafeCoerce (-halfHeight) 0 :: Vertex3 GLdouble
        vUR = Vertex3 halfWidth (-halfHeight) 0 :: Vertex3 GLdouble
        vLR = Vertex3 halfWidth halfHeight 0 :: Vertex3 GLdouble
        vLL = Vertex3 (-halfWidth) halfHeight 0 :: Vertex3 GLdouble
        rot = rToD o
        zaxis = Vector3 0 0 1

    matrixMode $= Modelview 0   -- why 0?
    loadIdentity
    
    clear [ColorBuffer]
    translate $ (Vector3 cmx cmy 0 :: Vector3 GLdouble)
    rotate rot zaxis
    renderPrimitive Quads $ do
        color $ (Color3 1 0 0 :: Color3 GLdouble)
        vertex vLL
        vertex vUL
        color $ (Color3 0 1 0 :: Color3 GLdouble)
        vertex vUR
        vertex vLR
 where
    rToD a = a * (180/pi)
