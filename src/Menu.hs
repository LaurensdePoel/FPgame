module Menu where

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact
import Model

pauseMenu :: GameState -> GameState
pauseMenu gs@Game {status = _status} = gs {status = toggleStatus}
  where
    toggleStatus :: Status
    toggleStatus = case _status of
      InMenu -> InGame
      InGame -> InMenu

checkMenuInput :: GameState -> GameState
checkMenuInput gs@Game {pressedKeys = _pressedKeys, menu = _menu}
  | upPressed = moveMenu fieldUp gs {pressedKeys = S.delete (Char 'w') _pressedKeys}
  | downPressed = moveMenu fieldDown gs {pressedKeys = S.delete (Char 's') _pressedKeys}
  | otherwise = gs
  where
    upPressed :: Bool
    upPressed = S.member (Char 'w') _pressedKeys || S.member (SpecialKey KeyUp) _pressedKeys

    downPressed :: Bool
    downPressed = S.member (Char 's') _pressedKeys || S.member (SpecialKey KeyDown) _pressedKeys

moveMenu :: ([Field] -> [Field]) -> GameState -> GameState
moveMenu fieldFunc gs@Game {menu = _menu, pressedKeys = _pressedKeys} =
  gs {menu = let fieldState = menu gs in fieldState {fields = fieldFunc (fields fieldState)}}

-- Place first element last
fieldDown :: [Field] -> [Field]
fieldDown [] = []
fieldDown (x : xs) = xs ++ [x]

fieldUp :: [Field] -> [Field]
fieldUp [] = []
fieldUp xs = last xs : init xs

-- checkMenuInput :: GameState -> GameState
-- checkMenuInput gs@Game {pressedKeys = _pressedKeys, menu = _menu} = gs {menu = moveFieldBaseOnKey _pressedKeys _menu}

-- menuInput gs@Game {menu = _menu} = singleKeyPress (Char 's')

-- moveFieldBasedOnKey :: key
--moveFieldBaseOnKey :: Eq key => key -> Menu -> Menu
-- moveFieldBaseOnKey :: S.Set Key -> Menu -> Menu
-- moveFieldBaseOnKey activekeys menu@Menu {fields = _fields}
--   | S.member (Char 's') activekeys = menu {fields = moveFieldDown _fields}
--   | S.member (Char 'w') activekeys = menu {fields = moveFieldUp _fields}
--   | otherwise = menu

-- moveFieldBaseOnKeyUp :: GameState -> GameState
-- moveFieldBaseOnKeyUp gs@Game {menu = _menu, pressedKeys = _pressedKeys} =
--   gs {menu = let fieldState = menu gs in fieldState {fields = moveFieldUp (fields fieldState)}}

-- moveFieldBaseOnKeyDown :: GameState -> GameState
-- moveFieldBaseOnKeyDown gs@Game {menu = _menu, pressedKeys = _pressedKeys} =
--   gs {menu = let fieldState = menu gs in fieldState {fields = moveFieldDown (fields fieldState)}}

-- moveFieldDown xs = tail xs ++ [head xs]
