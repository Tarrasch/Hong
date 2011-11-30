module Constants where

-- A Module containing all constants in the game.

import Graphics.SOE.Gtk

----------------- Constants -----------------

-- the radius of the ball in pixels
ballRadius :: Float
ballRadius = 0.15

-- This value is only graphical, because when this value is increased,
-- the paddle is only expanded to the left (for the left paddle), so
-- it won't alter the laws when collision-computing, since the rightmost
-- "line" (again for the left paddle) will still be the same.
paddleWidth :: Float
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
planeHalfHeight :: Float
planeHalfHeight = 2

-- The "floor and ceiling"'s height, this is just a graphical value, just like paddleWidth
borderHeight :: Float
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
startSpeedX :: Float
startSpeedX = 0.5

startSpeedY :: Float
startSpeedY = 0.2

-- The ball will always have at least this absolute velocity
-- after getting hit by a paddle.
minimumYVelocity :: Float
minimumYVelocity = 0.2

----------------- Derived Constants -----------------

-- This section contain constants that are only depending on other constants

-- This value should be positive, or else the constants above
-- are really badly set.
absHighestPaddlePoint = planeHalfHeight - paddleHalfHeight

----------------- Other -----------------

-- | A small number compared to the other values
epsilon :: Float
epsilon = 0.001
