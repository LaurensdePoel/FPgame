module Menu where

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact
import Model

-- Toggles the status in the GameState.
pauseMenu :: GameState -> GameState
pauseMenu gs@Game {status = _status} = gs {status = toggleStatus}
  where
    toggleStatus :: Status
    toggleStatus = case _status of
      InMenu -> InGame
      InGame -> InMenu

-- When status is InMenu check if one the keys is pressed and handles the given function and removes key from pressed keys to prevent recalls
checkMenuInput :: GameState -> GameState
checkMenuInput gs@Game {pressedKeys = _pressedKeys, menu = _menu}
  | pressed (Char 'w') = moveMenu fieldUp gs {pressedKeys = S.delete (Char 'w') _pressedKeys}
  | pressed (SpecialKey KeyUp) = moveMenu fieldUp gs {pressedKeys = S.delete (SpecialKey KeyUp) _pressedKeys}
  | pressed (Char 's') = moveMenu fieldDown gs {pressedKeys = S.delete (Char 's') _pressedKeys}
  | pressed (SpecialKey KeyDown) = moveMenu fieldDown gs {pressedKeys = S.delete (SpecialKey KeyDown) _pressedKeys}
  | pressed (Char 'd') = nextMenuV2 gs {pressedKeys = S.delete (Char 'd') _pressedKeys}
  | pressed (SpecialKey KeyRight) = nextMenu gs {pressedKeys = S.delete (SpecialKey KeyRight) _pressedKeys}
  | pressed (SpecialKey KeyEnter) = nextMenu gs {pressedKeys = S.delete (SpecialKey KeyEnter) _pressedKeys}
  | pressed (Char 'a') = previousMenu gs {pressedKeys = S.delete (Char 'a') _pressedKeys}
  | pressed (SpecialKey KeyLeft) = previousMenu gs {pressedKeys = S.delete (SpecialKey KeyLeft) _pressedKeys}
  | pressed (SpecialKey KeyDelete) = previousMenu gs {pressedKeys = S.delete (SpecialKey KeyDelete) _pressedKeys}
  | pressed (SpecialKey KeyBackspace) = previousMenu gs {pressedKeys = S.delete (SpecialKey KeyBackspace) _pressedKeys}
  | otherwise = gs
  where
    pressed :: Key -> Bool
    pressed key = S.member key _pressedKeys

-- This function changes the selected field in the current menu
-- ([Field] -> [Field]) argument is a function that alters the order of the Field in the list.
-- The order of the fields determense which currend field is selected.
moveMenu :: ([Field] -> [Field]) -> GameState -> GameState
moveMenu fieldFunc gs@Game {menu = _menu, pressedKeys = _pressedKeys} =
  gs {menu = let fieldState = menu gs in fieldState {fields = fieldFunc (fields fieldState)}}

-- Select field above current one or loop around
fieldUp :: [Field] -> [Field]
fieldUp [] = []
fieldUp xs = last xs : init xs

-- Select field Below current one or loop around
fieldDown :: [Field] -> [Field]
fieldDown [] = []
fieldDown (x : xs) = xs ++ [x]

-- Load the child menu as current menu if there isn't a child menu do nothing and return current menu
nextMenu :: GameState -> GameState
nextMenu gs@Game {menu = _menu} = gs {menu = check (subMenu $ head (fields _menu))}
  where
    check newMenu = case newMenu of
      Menu {} -> newMenu
      NoMenu -> _menu
      NoMenuButFunction f -> _menu

nextMenuV2 :: GameState -> GameState
nextMenuV2 gs@Game {menu = _menu} = newMenu (subMenu $ head (fields _menu))
  where
    newMenu newMenu = case newMenu of
      Menu {} -> gs {menu = newMenu}
      NoMenu -> gs
      NoMenuButFunction f -> f gs

-- Load the parent menu as current menu if there isn't a parent menu do nothing and return current menu
previousMenu :: GameState -> GameState
previousMenu gs@Game {menu = _menu} = gs {menu = check (returnMenu _menu)}
  where
    check newMenu = case newMenu of
      Menu {} -> newMenu
      NoMenu -> _menu
      NoMenuButFunction f -> _menu
