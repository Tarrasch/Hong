-- A Module containing all constants in the game.
module HongConstants where

import Graphics.SOE.Gtk

----------------- Play area metrics -----------------

-- This value is obviusly not only graphical.
paddleHalfHeight :: (Num a, Fractional a) => a
paddleHalfHeight = 0.25

-- Half the distance between the left paddle and right paddle.
--
-- To be precise: The distance between the rightmost of the left paddle
-- and the leftmost of the right paddle. (also see paddleWidth)
planeHalfWidth :: Num a => a
planeHalfWidth = 2

-- This is obviously not only graphical
planeHalfHeight :: Float
planeHalfHeight = 2


----------------- Purely graphical constants -----------------

-- We try to keep as many constants purely graphical to simplify logic

-- This value is only graphical, because when this value is increased,
-- the paddle is only expanded to the left (for the left paddle), so
-- it won't alter the laws when collision-computing, since the rightmost
-- "line" (again for the left paddle) will still be the same.
paddleWidth :: Float
paddleWidth = 0.05

-- The "floor and ceiling"'s height, this is just a graphical value
borderHeight :: Float
borderHeight = 0.05

ballRadius :: Float
ballRadius = 0.15

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

----------------- Derived Constants -----------------

-- This section contain constants that are only depending on other constants

-- This value should be positive!
highestPaddlePoint = planeHalfHeight - paddleHalfHeight

----------------- Other -----------------

-- | A small number compared to the other values
epsilon :: Float
epsilon = 0.001
