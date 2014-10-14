{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ConstraintKinds      #-}

module Main where

import Control.Monad.Except
import Control.Monad.State hiding (state)
import Cove.Assets
import Cove.Errors
import Cove.Geometry
import Cove.Input
import Cove.Utilities
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Word
import Graphics.UI.SDL.Types
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import qualified Cove as Cove

---- Config ----

windowData :: (String, Int, Int)
windowData = ("lesson14", 640, 480)

initialState :: World
initialState = World { appState = Running, frame = 0 }


data AppState = Running | Exiting deriving Eq
data World = World { appState :: AppState, frame :: Int }


---- Application ----

main :: IO ()
main = withSDLContext windowData $ \renderer ->
    print "cont"
    withAssets renderer ["./assets/walk.png"] $ \assets -> do
        print "ass"
        let inputSource = pollEvent `into` updateState
        let pollDraw = inputSource ~>~ drawState renderer assets
        runStateT (repeatUntilComplete pollDraw) initialState


fullWindow :: SDL.Rect
fullWindow = toRect 0 0 screenWidth screenHeight


drawState :: SDL.Renderer -> [Asset] -> World -> IO ()
drawState renderer assets (World Running frameValue) = withBlankScreen renderer $ do
    let currentFrame = getFrameAtTime frameValue
    let ImageAsset texture' = head assets
    let spriteRect = toRect 0 0 192 (192 :: Int)

    print "drawuing"
    with2 (getMask currentFrame) (spriteRect `centredOn` fullWindow) (SDL.renderCopy renderer texture')

    where getMask :: Int -> SDL.Rect
          getMask x = toRect (x * 48) 0 48 48
          frameCount = 8
          getFrameAtTime t = 8 `stepsPerSecond` t `mod` frameCount

drawState _ _ _ = return ()


stepsPerSecond :: Int -> Int -> Int
stepsPerSecond ms s = (s * ms) `div` 60


withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    _ <- SDL.renderClear renderer
    _ <- operation
    SDL.renderPresent renderer


updateState :: Input -> World -> World
updateState (Just (SDL.QuitEvent _ _)) state = state { appState = Exiting }
updateState _ state = state { frame = frame state + 1 }


repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \world -> unless (appState world == Exiting) $ repeatUntilComplete game


