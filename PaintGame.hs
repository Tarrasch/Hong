module PaintGame
  (drawState)
  where

import GameState
import Constants
import Test.QuickCheck
import Control.Monad
import Graphics.SOE.Gtk

----------------- Quickstart -----------------

-- Test running this function to see a static image of the game

test ::  IO ()
test
  = runGraphics (
    do w <- openWindowEx "Does it look good?" (Just (0,0)) (Just (xWin,yWin))
              drawBufferedGraphic Nothing
       drawState w startState
       spaceClose w
    )


spaceClose :: Window -> IO ()
spaceClose w
  = do k <- getKey w
       if k==' ' || k == '\x0'
          then closeWindow w
          else spaceClose w


----------------- Types -----------------

type Pixel = (Int, Int)

----------------- Coord <--> Pixel -----------------

-- example: Coordinate (0, 0) is pixel (X/2, Y/2)

pixelToCoordinate :: ScaleFactor -> (Int, Int) -> (ABC, ABC)
pixelToCoordinate sf (x, y) = let x' = (fromIntegral x    / sf) - 50
                                  y' = (fromIntegral (-y) / sf) + 50
                              in  (x', y')

coordinateToPixel :: ScaleFactor -> (ABC, ABC) -> (Int, Int)
coordinateToPixel sf (x, y) = let x' = round $ (50 + x) * sf
                                  y' = round $ (50 - y) * sf
                              in  (x', y')

-- A property that checks that the above functions works as they should
propPTCCTP :: (Int, Int) ->  ScaleFactor -> Property
propPTCCTP (i1, i2) f' = f > 0 ==> x0 == x1 && y0 == y1
    where (x0, y0) = (i1 % 10000, i2 % 10000)
          f        = min 100.0 f'
          (x1, y1) = coordinateToPixel f $ pixelToCoordinate f (x0, y0)
          (%)      = mod

----------------- Functions -----------------

-- The convention used here is that drawState uses the other functions,
-- and that drawState supplies an approriate point (often middle-point)
-- of the shape that the helper-functions are drawing.

-- Takes a window and the current state of the game and draws it.
drawState :: Window -> State -> IO()
drawState win s = do (w, h) <- getWindowSize win
                     let sf = getScaleFactor w h
                     clearWindow win
                     mapM_ (drawInWindow win)
                        [drawBall sf (ballPos s),
                         drawLeftPaddle  sf (leftPaddle  s),
                         drawRightPaddle sf (rightPaddle s),
                         drawFloor sf,
                         drawCeiling sf]

drawBall :: ScaleFactor -> (ABC, ABC) -> Graphic
drawBall sf (x, y) = withColor ballColor $ ellipse pbl ptr
  where r   = ballRadius
        bl  = (x - r, y - r) -- bottom left
        tr  = (x + r, y + r) -- top right
        pbl = coordinateToPixel sf bl -- as pixel
        ptr = coordinateToPixel sf tr -- as pixel


drawPaddle :: ScaleFactor -> (ABC, ABC) -> Graphic
drawPaddle sf (x, y) = withColor paddleColor $ drawRegion $ createRectangle pbl ptr
  where bl  = (x              , y - paddleHalfHeight)
        tr  = (x + paddleWidth, y + paddleHalfHeight)
        pbl = coordinateToPixel sf bl
        ptr = coordinateToPixel sf tr

drawLeftPaddle, drawRightPaddle :: ScaleFactor -> ABC -> Graphic
drawLeftPaddle  sf y = drawPaddle sf (-planeHalfWidth - paddleWidth, y)
drawRightPaddle sf y = drawPaddle sf ( planeHalfWidth              , y)



drawBorder :: ScaleFactor -> ABC -> Graphic
drawBorder sf y = withColor borderColor $ drawRegion $ createRectangle pbl ptr
  where bl  = (-planeHalfWidth, y)
        tr  = ( planeHalfWidth, y + borderHeight)
        pbl = coordinateToPixel sf bl
        ptr = coordinateToPixel sf tr

drawFloor, drawCeiling :: ScaleFactor -> Graphic
drawFloor   sf = drawBorder sf (-planeHalfHeight - borderHeight)
drawCeiling sf = drawBorder sf ( planeHalfHeight               )



