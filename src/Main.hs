{-# LANGUAGE FlexibleContexts #-}

module Main where

import Basic
import Board
import Game
import Snake

import Data.Word

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL as SDL

data MessageDir = MessageDir {
    quitMessage     :: Bool
}

data AppConfig = AppConfig {
    screen      :: Surface,
    gameScreen  :: Surface,
    messageDir  :: MessageDir
}

type AppState = StateT GameState IO
type AppEnv = ReaderT AppConfig AppState

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getGameScreen :: MonadReader AppConfig m => m Surface
getGameScreen = liftM gameScreen ask

handleInput :: (MonadIO m, MonadState GameState m) => Event -> m ()
handleInput (KeyDown (Keysym key _ _)) =
                    case key of
                        SDLK_a     -> liftIO $ setCaption "asdf" []
                        SDLK_q     -> liftIO $ pushEvent Quit
                        SDLK_DOWN  -> changeDirection South
                        SDLK_UP    -> changeDirection North
                        SDLK_LEFT  -> changeDirection West
                        SDLK_RIGHT -> changeDirection East
                        _          -> return ()
                where
                    changeDirection direction = do
                        gameState <- get
                        snakeState <- snakeState `liftM` get
                        put gameState {snakeState = changeSnakeDirection snakeState direction }

handleInput _ = return ()

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit      -> return True
        NoEvent   -> return False
        _         -> do
            act event
            whileEvents act

main = withInit [InitEverything] $ do

    (env, snakeState) <- initEnv

    runLoop env snakeState


initEnv :: IO (AppConfig, GameState)
initEnv = do
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    gameScreen <- createRGBSurface [SWSurface] gameScreenWidth gameScreenHeight screenBpp 0 0 0 0
    setCaption "Haskell Snake" []

    colorWhite <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
    colorBlack <- (mapRGB . surfaceGetPixelFormat) screen 0x0f 0x0f 0x0f

    paintBoard gameScreen

    let msgDir = MessageDir False 

    applePosition <- getRandomApple initialSnakePosition
    tick <- getTicks -- ms since SDL init (program start)

    return (AppConfig screen gameScreen msgDir,
            initialGameState {applePosition = applePosition
                             ,lastSnakeMove = tick }) -- timerState


runLoop :: AppConfig -> GameState -> IO ()
runLoop = evalStateT . runReaderT loop

loop :: AppEnv ()
loop = do
        quit <- whileEvents $ handleInput

        screen      <- screen `liftM` ask
        gameScreen  <- gameScreen `liftM` ask
        messageDir  <- messageDir `liftM` ask

        gameState <- get
        let ap = applePosition gameState
        let ss = snakeState gameState
        let sp = position ss

        drawGame

	let lvl = level gameState
        if checkCollision sp
            then error $ "Game over.  You got to level " ++ show lvl ++ "."
            else return ()

        tick <- liftIO getTicks

	let initialDelay = 100 -- ms
	let levelSpeedup = 10*(fromIntegral $ min (level gameState) 10)
	let currentDelay = initialDelay - levelSpeedup
        if (tick - (lastSnakeMove gameState) > currentDelay) then
            if snakeEatsApple sp ap
                then do
                    newApplePosition <- liftIO $ getRandomApple (position (newSnakeState ss))
                    -- start new level after apple is eaten
                    put initialGameState {
                            snakeState = newSnakeState ss
                           ,applePosition = newApplePosition
                           ,level = (level gameState) + 1}
                    
                else put (moveSnakeGameState gameState tick)
            else return ()

        unless quit loop
    where
        newSnakeState ss = ss {
                                len = ((len ss) + 1) }
        moveSnakeGameState gameState tick =
                        gameState{ snakeState = (moveSnake (snakeState gameState))
                                  ,lastSnakeMove = tick }

drawGame = do
    screen      <- screen `liftM` ask
    gameScreen  <- gameScreen `liftM` ask
    gameState   <- get

    liftIO $ do
        paintBoard gameScreen
        paintApple gameScreen (applePosition gameState)
        paintSnake gameScreen (snakeState gameState)

        blitSurface gameScreen Nothing screen Nothing

        SDL.flip screen
