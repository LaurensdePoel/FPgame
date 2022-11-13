module Input where

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key)
import Graphics.Gloss.Interface.IO.Interact (KeyState (..))
import Model

-- | Handle user input
input :: Event -> GameState -> IO GameState
input event gs = return (inputKey event gs)

-- | This function filters for key Up and Down events and adds or deletes the corresponding key in a data set
inputKey :: Event -> GameState -> GameState
inputKey (EventKey k Down _ _) gs = gs {pressedKeys = S.insert k (pressedKeys gs)}
inputKey (EventKey k Up _ _) gs = gs {pressedKeys = S.delete k (pressedKeys gs)}
inputKey _ gs = gs -- Otherwise keep the same

-- | Executes passed function on specified key "
singleKeyPress :: Key -> GameState -> (GameState -> GameState) -> GameState
singleKeyPress key gs@GameState {pressedKeys = _pressedKeys} f
  | S.member key _pressedKeys = f gs {pressedKeys = S.delete key _pressedKeys}
  | otherwise = gs

-- | remove all active keys from the data set
emptyKeys :: S.Set Key
emptyKeys = S.empty