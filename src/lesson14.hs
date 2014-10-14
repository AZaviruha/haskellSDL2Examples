import qualified Cove
import Cove.Utilities
import Control.Monad

import qualified  Graphics.UI.SDL as SDL
import Cove.Entity


data World = World { gameover :: Bool, scene :: Cove.Entity }

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 600

brawlerSpriteAt :: Int -> Cove.Sprite
brawlerSpriteAt i = Cove.Sprite {
    src = "./assets/walk.png",
    spriteX = 48 * i,
    spriteY = 0,
    spriteW = 48,
    spriteH = 48 }

brawler :: Cove.Entity
brawler = Cove.spriteToEntity (brawlerSpriteAt 0) `Cove.scaledBy` 4 `Cove.centredIn` (0, 0, screenWidth, screenHeight)


superFunc :: SDL.Renderer -> FilePath -> IO (FilePath, Cove.Asset)
superFunc renderer path = liftM ((,) path) (Cove.loadAsset renderer path)


main :: IO ()
main = Cove.withSDLContext ("Lesson 14", screenWidth, screenHeight) $ \renderer -> do
    -- SDL.renderSetLogicalSize renderer 320 240

    let assetPaths = ["./assets/walk.png"]

    assets <- mapM (superFunc renderer) assetPaths

    let inputSource = Cove.pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer assets
    let initialState = World { gameover = False, scene = brawler }

    Cove.run (untilM gameover pollDraw) initialState


updateState :: Cove.Input -> World -> World
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state


drawState :: SDL.Renderer -> [(FilePath, Cove.Asset)] -> World -> IO ()
drawState renderer assets world = do
    ticks <- SDL.getTicks
    let frame = stepsPerSecond (fromIntegral ticks) 8
    let newEnt = updateEntity frame (scene world)

    Cove.drawEntityTree renderer assets newEnt


updateEntity :: Int -> Cove.Entity -> Cove.Entity
updateEntity frame (Image (x, y) (w, h) sprite) = Image (x, y) (w, h) newSprite
    where newSprite = brawlerSpriteAt frame


stepsPerSecond :: Int -> Int -> Int
stepsPerSecond ms s = f `mod` s
    where f = (s * ms) `div` 1000
