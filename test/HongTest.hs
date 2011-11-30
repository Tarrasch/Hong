-- | This module tests the Hong game, all tests are pure (no side effects).
--   The reason why the tests take some seconds to run is simply that
--   we must have a high sampling rate in order to ensure to logic is
--   correct, and thus we also must look at many samples which takes time.
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module HongTest (specs) where

import Test.Hspec
import Test.Hspec.HUnit ()

import Fal (runBehavior)
import GameState
import UserControl
import Constants

run :: State -> [State]
run s0 = runBehavior (pong' s0 uc) (repeat Nothing, [0, 0.001..])

ever :: (State -> Bool) -> [State] -> Bool
ever f ss = any f $ take 100000 ss

outside :: State -> Bool
outside s = abs (xPosition s) > (planeHalfWidth  + epsilon)
         || abs (yPosition s) > (planeHalfHeight + epsilon)

alwaysInside :: [State] -> Bool
alwaysInside = not . ever outside

specs :: [Spec]
specs = describe "Hong"
  [ it "tests the test framework" True

  , let ss = run startState{ xVelocity = 0 }
    in describe "bounces on walls" [
      it "always have ball inside" $ alwaysInside ss
    , it "bounces roof"  $ ever (\s -> yVelocity s < 0) ss
    , it "bounces floor" $
          ever (\s -> yPosition s < 0 && yVelocity s > 0) ss
    ]
  , let ss = run startState{ yVelocity = 0 }
    in describe "bounces on paddles" [
      it "keeps bouncing" $ alwaysInside ss
    , it "bounces on right paddle" $ ever (\s -> xVelocity s < 0) ss
    , it "bounces on left  paddle" $
          ever (\s -> xPosition s < 0 && xVelocity s > 0) ss
    ]
  , let s0 = startState { yVelocity = 0
                        , leftPaddle = 1000
                        , rightPaddle = (-1000) }
    in describe "avoids bounces when paddle not present" [
      let ss = run s0{ xVelocity = 1 }
      in  it "no bounce right paddle" $
             ever (\s -> xPosition s > 2*planeHalfWidth) ss
    , let ss = run s0{ xVelocity = (-1) }
      in  it "no bounce on left paddle" $
             ever (\s -> xPosition s < (-2)*planeHalfWidth) ss
    ]
  ]
