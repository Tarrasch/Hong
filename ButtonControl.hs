module ButtonControl where

-- | The SOE library tells information of when a button is pushed down
--   and when a button is released. We are interested in which intervals
--   a button is down, but we can't comfortly query the SOE-library
--   for that information.
--
--   Instead, we register when buttons are pushed and released, that way
--   we can query the data-structure developed in this module if a button
--   is down or not. Or more practically, we convert a ButtonControl
--   to a UserControl.

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import GameState


type ButtonControl = Set Char

-- These functions hopefully have names better describing
-- what we want to achieve.
enters, leaves :: Char -> ButtonControl -> ButtonControl
enters   = Set.insert
leaves   = Set.delete

isPushed :: Char -> ButtonControl -> Bool
isPushed = Set.member

confirm :: Ord a => Set a -> a -> b -> Maybe b
confirm set a b = guard (Set.member a set) >> Just b

nothingPressedYet :: ButtonControl
nothingPressedYet = Set.empty


-- This function takes what buttons is pressed on the keyboard and
-- returns a UserControl, which is the type the game-state
-- functions are working with.
toControls :: ButtonControl -> UserControl
toControls bc =
  UserControl {
    playerLeft  = msum ['w' ><> Up, 's' ><> Down],
    playerRight = msum ['o' ><> Up, 'l' ><> Down],
    restartGame = isPushed 'r' bc
  }
  where (><>) = confirm bc
