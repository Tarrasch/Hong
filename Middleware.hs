-- | Middleware for FAL Behaviors
module Middleware (
    timeMiddleware
    )
  where

import Fal

timeMiddleware :: Behavior a -> Behavior a
timeMiddleware (Behavior beh) =
    Behavior $ \(mus, ts) -> beh (mus, runBehavior timeFlowBehavior (mus, ts))

timeFlowBehavior :: Behavior Time
timeFlowBehavior = ts
  where
    ts = magicIntegral ds
    ds = magicDelay * factor * pauseFactor
    factor = 1 `stepAccum` (pressed '-' ->> (/1.2)
                        .|. pressed '+' ->> (*1.2))
    pauseFactor = 1 `stepAccum` pressed 'P' ->> (1-)

magicDelay :: Behavior Time
magicDelay = Behavior $ \(_, ts) -> 0 : zipWith (-) (tail ts) ts

magicIntegral :: Behavior Time -> Behavior Time
magicIntegral (Behavior beh) = Behavior $ \(mus, ts) -> beh (mus, scanl (+) 0 ts)

unsafeDelay (Behavior beh) =
    Behavior $ \(mus, ts) -> beh (mus, 0 : tail ts)
