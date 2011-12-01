-- | Middleware for FAL Behaviors
module Middleware (
    timeMiddleware
    )
  where

import Fal

-- | Middleware for alternating time with scaling, pausing and reversing
timeMiddleware :: Behavior a -> Behavior a
timeMiddleware (Behavior beh) =
    Behavior $ \(mus, ts) -> beh (mus, runBehavior timeFlowBehavior (mus, ts))

timeFlowBehavior :: Behavior Time
timeFlowBehavior = ts
  where
    ts = magicIntegral ds
    ds = magicDifference * factor * pauseFactor * backwardsFactor
    factor = 1 `stepAccum` (pressed '-' ->> (/1.2)
                        .|. pressed '+' ->> (*1.2))
    pauseFactor = 1 `stepAccum` pressed 'P' ->> (1-)
    backwardsFactor = 1 `stepAccum` pressed 'B' ->> (*(-1))

magicDifference :: Behavior Time
magicDifference = Behavior $ \(_, ts) -> 0 : zipWith (-) (tail ts) ts
                    -- the "0 :" is required to preserve causality

magicIntegral :: Behavior Time -> Behavior Time
magicIntegral (Behavior beh) = Behavior $ \(mus, t0:ts) -> scanl (+) t0 $ beh (mus, ts)

unsafeDelay (Behavior beh) =
    Behavior $ \(mus, ts) -> beh (mus, 0 : tail ts)
