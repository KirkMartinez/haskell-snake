module Game where

import Basic
import Board
import Snake

import Data.Word

import Control.Monad.State
import Control.Monad.Reader
import Graphics.UI.SDL as SDL

screenWidth :: Int
screenWidth   = 1024
screenHeight :: Int
screenHeight  = 1024
screenBpp :: Int
screenBpp     = 32
gameScreenWidth :: Int
--gameScreenWidth   = 540
gameScreenWidth = screenWidth
gameScreenHeight :: Int
--gameScreenHeight  = 400
gameScreenHeight = screenHeight

rectWidth :: Int
rectWidth     = gameScreenWidth `div` numRectsX
rectHeight :: Int
rectHeight    = gameScreenHeight `div` numRectsY

data GameState = GameState {
     snakeState         :: SnakeState,
     enemySnakeState    :: SnakeState,
     applePosition      :: Point,
     board              :: Board,
     level              :: Int,
     lastSnakeMove      :: Word32
}

initialGameState = GameState {snakeState = initialSnakeState
			     ,enemySnakeState = initialEnemySnakeState
                             ,applePosition = Point 0 0
                             ,board = initialBoard
                             ,level = 1
                             ,lastSnakeMove = 0}

rectFromPoint :: Point -> Maybe Rect
rectFromPoint (Point x y) = 
                Just (Rect ((x - 1)*rectWidth )
                           ((y - 1)*rectHeight )
                           (rectWidth )
                           (rectHeight ))

rects = [rectFromPoint (Point x y) | x <- [1..numRectsX], y <- [1..numRectsY]]

{-|
  'mapM_' is used to map a function over a list, this must return
   an IO monad sequence instead of a list, like the usual map does.
   'mapM_' is different from 'mapM' in that it does not collect the results
   of mapped computation (see also 'sequence' and 'sequence_' in the
   "Control.Monad" documentation).
-}
paintRects gameScreen color = liftIO $ mapM_ (\rect -> fillRect gameScreen rect color) rects

paintBoard :: Surface -> IO ()
paintBoard gameScreen = do
                       colorWhite <- (mapRGB . surfaceGetPixelFormat) gameScreen 0xff 0xff 0xff
                       paintRects gameScreen colorWhite
                       return ()

paintApple :: Surface -> Point -> IO ()
paintApple gameScreen applePosition = do
                       colorRed <- (mapRGB . surfaceGetPixelFormat) gameScreen 0xff 0x00 0x00
                       fillRect gameScreen (rectFromPoint applePosition) colorRed
                       return ()

paintSnakePiece :: Surface -> Color -> Maybe Rect -> IO ()
paintSnakePiece gameScreen color rect = do
                           theColor <- (mapRGB . surfaceGetPixelFormat) gameScreen (colorRed color) (colorGreen color) (colorBlue color)
                           fillRect gameScreen rect theColor
                           return ()

paintSnake :: Surface -> SnakeState -> Color -> IO ()
paintSnake gameScreen snakeState color =
                      mapM_ (paintSnakePiece gameScreen color . rectFromPoint) (position snakeState)

