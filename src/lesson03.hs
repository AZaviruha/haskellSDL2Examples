import qualified Cove
import Cove.Utilities
import Control.Monad

import qualified  Graphics.UI.SDL as SDL
import Cove.Entity


windowTitle :: String
windowTitle = "Lesson 03"

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480


main :: IO ()
main = Cove.withSDLContext (windowTitle, screenWidth, screenHeight) $ \renderer -> do

    let assetPaths = ["./assets/walk.png"]

    assets <- mapM (superFunc renderer) assetPaths

    let inputSource = Cove.pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer assets
    let initialState = World { gameover = False, scene = brawler }

    Cove.run (untilM gameover pollDraw) initialState




    screenSurface <- SDL.getWindowSurface window
    imageSurface <- loadBitmap "./assets/x.bmp" >>= either throwSDLError return

    repeatUntil sdlQuit $ SDL.blitSurface imageSurface nullPtr screenSurface nullPtr >> SDL.updateWindowSurface window

    SDL.freeSurface imageSurface


initializeSDL :: [Word32] -> IO (Risky ())
initializeSDL flags = do
    initSuccess <- SDL.init $ foldl (.|.) 0 flags
    return $ if initSuccess < 0 then Left "SDL could not initialize!" else Right ()


createWindow :: String -> IO (Risky SDL.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
    window <- SDL.createWindow title SDL.windowPosUndefined SDL.windowPosUndefined screenWidth screenHeight SDL.windowFlagShown
    return $ if window == nullPtr then Left "Window could not be created!" else Right window


loadBitmap :: String -> IO (Risky (Ptr SDL.Surface))
loadBitmap path = do
    surface <- withCAString path SDL.loadBMP
    return $ if surface == nullPtr then Left ("Unable to load image " ++ path ++ "!") else Right surface


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)


repeatUntil :: IO Bool -> IO a -> IO ()
repeatUntil quitClause operation = do
    operation
    isQuitting <- quitClause
    unless isQuitting $ repeatUntil quitClause operation


sdlQuit :: IO Bool
sdlQuit = liftM isQuitEvent pollEvent


isQuitEvent :: Maybe SDL.Event -> Bool
isQuitEvent Nothing = False
isQuitEvent (Just event) = case event of
    SDL.QuitEvent _ _ -> True
    _ -> False


pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pointer -> do
    status <- SDL.pollEvent pointer

    if status == 1
        then maybePeek peek pointer
        else return Nothing


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer
