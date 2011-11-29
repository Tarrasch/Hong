module UserControl where

import Fal
import Control.Monad (guard, mplus)

type PaddleControl = Float

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
pc uk dk = (mf uk 1.0) + (mf dk (-1.0))
  where mf k v = lift1 (\b -> if b then v else 0.0) (isHeld k)
