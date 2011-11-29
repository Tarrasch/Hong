module Constants where

-- A Module containing all constants in the game.

import Graphics.SOE.Gtk

----------------- Types -----------------

-- Since we want the game to be completely scaleable by simply
-- adjusting the xWin/yWin values without needing to change anything
-- else, we introduce a standard-length-dimension. We say that from
-- leftmost to rightmost of the screen, there is 100 ABC, it's also
-- 100 ABC heightwise.
-- RENAME LATER
type ABC = Float

-- Explained below.
type ScaleFactor = Float

----------------- Window -----------------

-- Width and height of the windows, in pixels
xWin, yWin :: Int
xWin = 500
yWin = 500


-- Basics of scaling.
--
-- The scaling factor is by default
--
-- f = (xWin / 100) == (yWin / 100 * windowProportions) ( ~= 6)
--
-- Where 100 comes from how 1 ABC is defined.
--
-- Furtheremore, we've decided to not unproportionize our scaling
-- (by that we mean to scale one axis more than the other)
-- So when calculating the scaling factor, we take care so that
-- the scaling factor f satisfies
--
-- f = min (X / 100) (Y / 100 * windowProportions)
--
-- X and Y are the window dimensions. If the window hasn't been resized
-- they are xWin and yWin.

-- If WindowProportions wasn't 1, the ball wouldn't be a cirle.
windowProportions :: Float
windowProportions = 1

-- To stress the right usage of the scaling system, we define ...
-- getScaleFactor
-- returns the scaling factor given the current window dimension
getScaleFactor :: Int -> Int -> ScaleFactor
getScaleFactor w h = let v1 = (fromIntegral w / 100)
                         v2 = (fromIntegral h / 100 * windowProportions)
                     in  min v1 v2
----------------- Constants -----------------

-- the radius of the ball in pixels
ballRadius :: ABC
ballRadius = 0.15

-- This value is only graphical, because when this value is increased,
-- the paddle is only expanded to the left (for the left paddle), so
-- it won't alter the laws when collision-computing, since the rightmost
-- "line" (again for the left paddle) will still be the same.
paddleWidth :: ABC
paddleWidth = 0.05

-- This value is obviusly not only graphical.
paddleHalfHeight :: (Num a, Fractional a) => a
paddleHalfHeight = 0.25


-- The distance between the left paddle and right paddle.
--
-- To be precise: The distance between the rightmost of the left paddle
-- and the leftmost of the right paddle. (also see paddleWidth)
planeHalfWidth :: Num a => a
planeHalfWidth = 2

-- This is obviously not only graphical
planeHalfHeight :: ABC
planeHalfHeight = 2

-- The "floor and ceiling"'s height, this is just a graphical value, just like paddleWidth
borderHeight :: ABC
borderHeight = 0.05


----------------- Colours -----------------

-- These values are purely graphical
ballColor, paddleColor, borderColor :: Color
ballColor   = Yellow
paddleColor = Blue
borderColor = Red


----------------- Gameplay & Controls -----------------

-- The speed of the paddles when the player moves them up/down
paddleSpeedCoefficient :: Float
paddleSpeedCoefficient = 0.2

-- The balls startSpeeds
startSpeedXY :: (Float, Float)
startSpeedXY = (1.2, 0.6)

-- The ball will always have at least this absolute velocity
-- after getting hit by a paddle.
minimumYVelocity :: Float
minimumYVelocity = 0.2

----------------- Derived Constants -----------------

-- This section contain constants that are only depending on other constants

-- This value should be positive, or else the constants above
-- are really badly set.
absHighestPaddlePoint = planeHalfHeight - paddleHalfHeight


