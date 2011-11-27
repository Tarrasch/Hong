-- | I add nice helper functions here that should already be in Fal
--   but unfortunately is not.
module Fal (
    module Fal.Fal
  , filterEvent
  , pressed
  )
  where

import Fal.Fal

-- | Filters events to trigger only for specified condition
filterEvent :: (a -> Bool) -> Event a -> Event a
filterEvent f (Event fe) = Event $ map myGuard . fe
  where myGuard (Just v) | f v = Just v
        myGuard mv             = Nothing

-- | This event gets triggered when given key is pressed
pressed :: Char -> Event ()
pressed c = filterEvent (==c) key ->> ()
