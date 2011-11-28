-- | I add nice helper functions here that should already be in Fal
--   but unfortunately is not.
module Fal (
    module Fal.Fal
  , filterEvent
  , pressed
  , released
  , repictimate
  , isHeld
  )
  where

import Graphics.SOE.Gtk hiding (Region, Event)
import Fal.Fal
import Fal.Picture (Picture)
import Fal.Animation (picToGraphic)

-- | Filters events to trigger only for specified condition
filterEvent :: (a -> Bool) -> Event a -> Event a
filterEvent f (Event fe) = Event $ map myGuard . fe
  where myGuard (Just v) | f v = Just v
        myGuard mv             = Nothing

-- | This event gets triggered when given key is pressed
pressed, released :: Char -> Event ()
pressed  c = filterEvent (==c) key         ->> ()
released c = filterEvent (==c) keyReleased ->> ()

-- | True when button is held down
isHeld :: Char -> Behavior Bool
isHeld c =
   lift0 False `untilB` pressed  c ->>
   lift0 True  `untilB` released c ->>
   isHeld c

-- | Reactimate a Behavior of Pictures
repictimate :: String -> Behavior Picture -> IO ()
repictimate title beh = reactimate title (lift1 picToGraphic beh)

-- | "low level" Event for when Key is released on keyboard
keyReleased :: Event Char
keyReleased = Event (\(uas,_) -> map getkey uas)
      where getkey (Just (Key ch False)) = Just ch
            getkey _                     = Nothing
