{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Types
import Control.Monad.State hiding (state)
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Word
import GHC.Int


---- Config ----

lessonTitle :: String
lessonTitle = "lesson19"

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480


---- Application ----

main :: IO ()
main = do
    initializeSDL [SDL.initFlagVideo, SDL.initFlagGameController] >>= catchRisky
    initializeSDLImage [Image.InitPNG] >>= catchRisky

    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    window <- createWindow lessonTitle >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated, SDL.rendererFlagPresentVSync] >>= catchRisky

    asset <- loadTexture renderer "./assets/arrow.png" >>= catchRisky
    gameController <- SDL.gameControllerOpen 0
    if gameController == nullPtr then fail "no controller found" else print "yay!"

    disableEventPolling [SDL.eventTypeControllerAxisMotion, SDL.eventTypeJoyAxisMotion]

    let initialState = World { gameover = False, controller = gameController, target = (screenWidth `div` 2, screenHeight `div` 2) }

    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer [asset]
    runStateT (repeatUntilComplete pollDraw) initialState

    SDL.gameControllerClose gameController
    freeAssets [asset]
    SDL.destroyRenderer renderer
    SDL.destroyWindow window

    SDL.quit
    Image.quit


data Key = Q | W | E | A | S | D | N
data ColourProperty = Red | Green | Blue | Alpha
data World = World { gameover :: Bool, controller :: SDL.GameController, target :: (CInt, CInt) }
type Input = Maybe SDL.Event
type Asset = (SDL.Texture, CInt, CInt)


fullWindow :: SDL.Rect
fullWindow = toRect 0 0 screenWidth screenHeight


disableEventPolling :: [Word32] -> IO ()
disableEventPolling = mapM_ (`SDL.eventState` 0)


drawState :: SDL.Renderer -> [Asset] -> World -> IO ()
drawState renderer assets state = withBlankScreen renderer $ do
    inputState <- getControllerState (controller state)
    with2 mask (position inputState) $ \mask' position' ->
        SDL.renderCopyEx renderer texture mask' position' degrees' nullPtr SDL.rendererFlipNone

    where (texture, w, h) = head assets
          sprite = toRect 0 0 w h
          mask = sprite
          position grrr = (sprite `centredOn` fullWindow) `moveBy` (superScale grrr)
          degrees' = 0


superScale :: (Double, Double) -> (Int, Int)
superScale = pairMap $ floor . (*) 200


pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)


getControllerState :: SDL.GameController -> IO (Double, Double)
getControllerState controller = do
    xValue <- getAxisState controller 0
    yValue <- getAxisState controller 1

    let range = hypotenuse xValue yValue
    let deadZone = 8000 ^ 2

    let carpetValue = if range < deadZone
        then (0, 0)
        else pairMap ssscale (xValue, yValue)

    return carpetValue
    where ssscale x = fromIntegral x / 32768


hypotenuse :: (Num a) => a -> a -> a
hypotenuse a b = a ^ 2 + b ^ 2


getAxisState :: SDL.GameController -> SDL.GameControllerAxis -> IO Int
getAxisState controller index = do
    axis <- SDL.gameControllerGetAxis controller index
    return $ fromIntegral axis


within :: (Ord a) => a -> (a, a) -> Bool
within x (lower, upper)
    | upper < lower     = within x (upper, lower)
    | otherwise         = lower <= x && upper > x


withBlankScreen :: SDL.Renderer -> IO a -> IO ()
withBlankScreen renderer operation = do
    SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear renderer
    operation
    SDL.renderPresent renderer


updateState :: Input -> World -> World
updateState (Just (SDL.QuitEvent _ _)) state = state { gameover = True }
updateState _ state = state


repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \state -> unless (gameover state) (repeatUntilComplete game)


---- Initialization ----

initializeSDL :: [Word32] -> IO (Risky CInt)
initializeSDL flags = do
    result <- SDL.init $ foldl (.|.) 0 flags
    return $ if result < 0 then Left "SDL could not initialize!" else Right result


initializeSDLImage :: [Image.InitFlag] -> IO (Risky CInt)
initializeSDLImage flags = do
    result <- Image.init $ Image.initFlagsToC flags
    return $ if result < 0 then Left "SDL_image could not initialize!" else Right result


createWindow :: String -> IO (Risky SDL.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
    window <- SDL.createWindow title SDL.windowPosUndefined SDL.windowPosUndefined screenWidth screenHeight SDL.windowFlagShown
    return $ if window == nullPtr then Left "Window could not be created!" else Right window


createRenderer :: SDL.Window -> CInt -> [Word32] -> IO (Risky SDL.Renderer)
createRenderer window index flags = do
    renderer <- SDL.createRenderer window index $ foldl (.|.) 0 flags
    return $ if renderer == nullPtr then Left "Renderer could not be created!" else Right renderer


setHint :: String -> String -> IO (Risky Bool)
setHint hint value = do
    result <- withCAString2 hint value SDL.setHint
    return $ if not result then Left "Warning: Linear texture filtering not enabled!" else Right result


---- Teardown ----

freeAssets :: [Asset] -> IO ()
freeAssets = mapM_ (SDL.destroyTexture . first)
    where first (a, _, _) = a


---- Surfacing & Texture Loading ----

loadSurface :: String -> IO (Risky (Ptr SDL.Surface))
loadSurface path = do
    surface <- withCAString path Image.load
    return $ if surface == nullPtr then Left ("Unable to load image " ++ path ++ "!") else Right surface


convertSurface :: Ptr Surface -> Ptr PixelFormat -> Word32 -> IO (Risky (Ptr SDL.Surface))
convertSurface surface format flags = do
    optimizedSurface <- SDL.convertSurface surface format flags
    return $ if optimizedSurface == nullPtr then Left "Unable to optimize image!" else Right optimizedSurface


loadTexture :: SDL.Renderer -> String -> IO (Risky Asset)
loadTexture renderer path = do
    loadedSurface <- loadSurface path >>= catchRisky
    let applyToSurface = flip applyToPointer loadedSurface

    width <- applyToSurface SDL.surfaceW
    height <- applyToSurface SDL.surfaceH
    pixelFormat <- applyToSurface SDL.surfaceFormat
    key <- SDL.mapRGB pixelFormat 0 0xFF 0xFF

    SDL.setColorKey loadedSurface 1 key
    newTexture <- createTextureFromSurface renderer loadedSurface >>= catchRisky

    SDL.freeSurface loadedSurface
    return $ if newTexture == nullPtr then Left "failed to load texture image" else Right (newTexture, width, height)


createTextureFromSurface :: SDL.Renderer -> Ptr SDL.Surface -> IO (Risky Texture)
createTextureFromSurface renderer surface = do
    result <- SDL.createTextureFromSurface renderer surface
    return $ if result == nullPtr then Left "Unable to create texture" else Right result


renderTexture :: SDL.Renderer -> SDL.Texture -> SDL.Rect -> SDL.Rect -> IO CInt
renderTexture renderer texture renderMask renderQuad = with2 renderMask renderQuad $ SDL.renderCopy renderer texture


---- Geometry ----

instance Num (CInt, CInt) where
   (ax, ay) + (bx, by) = (ax + bx, ay + by)
   (ax, ay) - (bx, by) = (ax - bx, ay - by)
   (ax, ay) * (bx, by) = (ax * bx, ay * by)
   abs (x, y) = (abs x, abs y)
   signum (x, y) = (signum x, signum y)
   fromInteger a = (fromInteger a, 0)


toRect :: (Integral a) => a -> a -> a -> a -> SDL.Rect
toRect x y w h = SDL.Rect { rectX = fromIntegral x, rectY = fromIntegral y, rectW = fromIntegral w, rectH = fromIntegral h }


moveTo :: (Integral a1, Integral a2) => SDL.Rect -> (a1, a2) -> SDL.Rect
moveTo rect (x, y) = rect { rectX = fromIntegral x, rectY = fromIntegral y }


moveBy :: (Integral a) => SDL.Rect -> (a, a) -> SDL.Rect
moveBy shape (dx, dy) = shape { rectX = rectX shape + fromIntegral dx, rectY = rectY shape + fromIntegral dy }


movePointBy :: (Integral a1, Integral a2) => (CInt, CInt) -> (a1, a2) -> (CInt, CInt)
movePointBy (ox, oy) (dx, dy) = (ox + fromIntegral dx, oy + fromIntegral dy)


centredOn :: SDL.Rect -> SDL.Rect -> SDL.Rect
centredOn inner outer = inner `moveBy` (centreOf outer - centreOf inner)


centreOf :: SDL.Rect -> (CInt, CInt)
centreOf shape = (x, y)
    where x = rectX shape + rectW shape `div` 2
          y = rectY shape + rectH shape `div` 2


toSDLPoint :: (CInt, CInt) -> SDL.Point
toSDLPoint (x, y) = SDL.Point { pointX = x, pointY = y }


---- Event Handling ----

pollEvent :: IO Input
pollEvent = alloca $ \pointer -> do
    status <- SDL.pollEvent pointer

    if status == 1
        then peek pointer >>= print >> maybePeek peek pointer
        else return Nothing


---- Error Handling ----

type Risky a = Either String a


catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return


logWarning :: Risky Bool -> IO Bool
logWarning = either (\x -> print x >> return False) return


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)


---- Utils ----

with2 :: (Storable a, Storable b) => a -> b -> (Ptr a -> Ptr b -> IO c) -> IO c
with2 a b f = with a $ \a' -> with b (f a')


withCAString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
withCAString2 a b f = withCAString a $ \a' -> withCAString b $ f a'


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer


infixl 4 ~>>
(~>>) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m b
(~>>) m f = m >>= lift . f


infixl 4 ~>~
(~>~) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m a
(~>~) m f = m >>= pass (lift . f)

pass :: (Monad m) => (a -> m b) -> a -> m a
pass f v = f v >> return v


into :: (Monad m, MonadTrans t, MonadState b (t m)) => m a -> (a -> b -> b) -> t m b
into source f = lift source >>= modify . f >> get
