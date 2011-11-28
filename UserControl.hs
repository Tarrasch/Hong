module UserControl where

import Fal
import Control.Monad (guard, mplus)

data UpOrDown = Up | Down
type PaddleControl = Maybe UpOrDown

-- all the users input will be converted to ...
data UserControl =
  UserControl { playerLeft  :: PaddleControl
              , playerRight :: PaddleControl
              }

-- | Behavior of a UserControl when using default keys
uc :: Behavior UserControl
uc = lift2 UserControl (pc 'w' 's') (pc 'o' 'l')

-- | Behavior of a PaddleControl with given up/down keys
pc :: Char -> -- | Up key
      Char -> -- | Down key
      Behavior PaddleControl
pc uk dk = lift2 mplus (mf uk Up) (mf dk Down)
  where mf k v = lift1 ((>> Just v) . guard) (isHeld k)
