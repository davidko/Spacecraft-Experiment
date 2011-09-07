{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeOperators #-}

module Main where

{-import Prelude hiding ((.), id)
import Data.Label
import Control.Category-}

import Numeric.LinearAlgebra

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL

-- Local modules

import Timer
import qualified Renderer as Rend
import RigidBodyDynamics
import Spacecraft2D

data AppConfig = AppConfig { screen :: Surface }

data AppData = AppData { fps        :: Timer
                       , spacecraft :: Spacecraft2D
                       }

type AppEnv = ReaderT AppConfig (StateT AppData IO)

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

getSpacecraft :: MonadState AppData m => m Spacecraft2D
getSpacecraft = liftM spacecraft get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS timer = modify $ \s -> s { fps = timer }

putSpacecraft :: MonadState AppData m => Spacecraft2D -> m ()
putSpacecraft s = modify $ \x -> x { spacecraft = s }

screenWidth = 640
screenHeight = 480

-- hack awaits! 

rmod :: RealFrac a => a -> a -> a
rmod x d = x - d * (fromIntegral . floor) (x / d)

clampSpacecraft s = s { body = clampBody (body s) }
 where
    clampBody b = b { centerMass = clampCenterMass (centerMass b) }
    clampCenterMass cm = 2 |> [clampX (cm @> 0), clampY (cm @> 1)]
    clampX x = x `rmod` (fromIntegral screenWidth)
    clampY y = y `rmod` (fromIntegral screenHeight)

-- environment initialization
initEnv :: IO AppConfig
initEnv = do
    screen <- setVideoMode screenWidth screenHeight screenBpp [HWSurface, DoubleBuf, OpenGL]
    Rend.rinit screenWidth screenHeight
    setCaption appName []

    return $ AppConfig screen
 where
    appName = "Physics Simulator"
    screenBpp = 32

loop :: AppEnv ()
loop = do
    -- FPS handling
    getFPS >>= liftIO . start >>= putFPS

    quit <- whileEvents $ \event -> do
        case event of
            (KeyDown (Keysym key _ _)) -> do
                case key of
                    SDLK_LEFT   -> do
                        getSpacecraft >>= return . startLeft >>= putSpacecraft
                    SDLK_RIGHT  -> do
                        getSpacecraft >>= return . startRight >>= putSpacecraft
                    _           -> return ()
            (KeyUp (Keysym key _ _)) -> do
                case key of
                    SDLK_LEFT   -> do
                        getSpacecraft >>= return . stopLeft >>= putSpacecraft
                    SDLK_RIGHT  -> do
                        getSpacecraft >>= return . stopRight >>= putSpacecraft
                    _           -> return ()
            _   -> return ()

    -- apply force here
    let interval = fromIntegral msecsPerFrame / 1000.0
    spacecraft <- getSpacecraft
    b <- return $ body spacecraft
    -- ugly hack
    (putSpacecraft . clampSpacecraft) $ spacecraft { body = stepTime interval b }

    -- Check FPS, delay if necessary
    fps <- getFPS
    liftIO $ do
        ticks <- getTimerTicks fps
        when (ticks < msecsPerFrame) $ do    -- FIXME if this is not true, then... that'd be bad
            delay $ msecsPerFrame - ticks

    -- render
    spacecraft <- getSpacecraft
    liftIO $ Rend.render spacecraft
    liftIO $ glSwapBuffers
    
    unless quit loop
 where
    msecsPerFrame = fromIntegral $ 1000 `div` framesPerSecond
    framesPerSecond = 30

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit    -> return True
        NoEvent -> return False
        _       -> do
            act event
            whileEvents act

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main = withInit [InitEverything] $ do
    env <- initEnv
    timer <- start defaultTimer
    runLoop env (AppData timer startSpacecraft)
 where
    startSpacecraft = Spacecraft2D { body = defaultRigidBody { centerMass = 2 |> [startX,startY] } }
    startX = fromIntegral screenWidth / 2
    startY = fromIntegral screenHeight / 2
