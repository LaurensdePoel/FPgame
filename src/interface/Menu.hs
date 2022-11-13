module Menu where

import Config as C
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact
import Model

-- | Toggles the status in the 'GameState'.
pauseMenu :: GameState -> GameState
pauseMenu gs = gs {status = toggleStatus}
  where
    toggleStatus :: Status
    toggleStatus = case status gs of
      InMenu -> InGame
      InGame -> InMenu

-- | Load the child menu as current menu if there isn't a child menu do nothing and return current menu
nextMenu :: GameState -> GameState
nextMenu gs@GameState {menu = _menu} = newMenu (subMenu $ head (fields _menu))
  where
    newMenu _newMenu = case _newMenu of
      Menu {} -> gs {menu = _newMenu}
      NoMenu -> gs
      NoMenuButFunction f -> f gs

-- | Load the parent menu as current menu if there isn't a parent menu do nothing and return current menu
previousMenu :: GameState -> GameState
previousMenu gs@GameState {menu = _menu} = gs {menu = check (returnMenu _menu)}
  where
    check newMenu = case newMenu of
      Menu {} -> newMenu
      NoMenu -> _menu
      NoMenuButFunction _ -> _menu

-- | This function changes the selected field in the current menu.
-- (['Field'] -> ['Field']) argument is a function that alters the order of the 'Field' in the list.
-- The order of the fields determense which currend field is selected.
moveMenu :: ([Field] -> [Field]) -> GameState -> GameState
moveMenu fieldFunc gs@GameState {menu = _menu, pressedKeys = _pressedKeys} =
  gs {menu = let fieldState = menu gs in fieldState {fields = fieldFunc (fields fieldState)}}

-- | Select field above current one or loop around.
fieldUp :: [Field] -> [Field]
fieldUp [] = []
fieldUp xs = last xs : init xs

-- | Select field Below current one or loop around.
fieldDown :: [Field] -> [Field]
fieldDown [] = []
fieldDown (x : xs) = xs ++ [x]

-- | When status is 'InMenu' check if one the keys is pressed and handles the given function
-- and removes key from pressed keys to prevent recalls.
checkMenuInput :: GameState -> GameState
checkMenuInput gs@GameState {pressedKeys = _pressedKeys}
  | pressed (Char 'w') = moveMenu fieldUp gs {pressedKeys = S.delete (Char 'w') _pressedKeys}
  | pressed (SpecialKey KeyUp) = moveMenu fieldUp gs {pressedKeys = S.delete (SpecialKey KeyUp) _pressedKeys}
  | pressed (Char 's') = moveMenu fieldDown gs {pressedKeys = S.delete (Char 's') _pressedKeys}
  | pressed (SpecialKey KeyDown) = moveMenu fieldDown gs {pressedKeys = S.delete (SpecialKey KeyDown) _pressedKeys}
  | pressed (SpecialKey KeyEnter) = nextMenu gs {pressedKeys = S.delete (SpecialKey KeyEnter) _pressedKeys}
  | pressed (SpecialKey KeyDelete) = previousMenu gs {pressedKeys = S.delete (SpecialKey KeyDelete) _pressedKeys}
  | pressed (SpecialKey KeyBackspace) = previousMenu gs {pressedKeys = S.delete (SpecialKey KeyBackspace) _pressedKeys}
  | pressed (SpecialKey KeyEsc) = previousMenu gs {pressedKeys = S.delete (SpecialKey KeyEsc) _pressedKeys}
  | otherwise = gs
  where
    pressed :: Key -> Bool
    pressed key = S.member key _pressedKeys

-- | Generates a list of fields
generateMenuFields :: [(String, Menu)] -> [Field]
generateMenuFields = foldl createField []
  where
    createField :: [Field] -> (String, Menu) -> [Field]
    createField rest (text', subMenu') = rest ++ [Field {fieldName = text', fieldPosition = (xOffset, yOffset), subMenu = subMenu'}]
      where
        xOffset, yOffset :: Float
        xOffset = (-0.5) * (fromIntegral (length text') * C.menuTextCharacterOffset) -- not working properly
        yOffset = C.menuTextStartHeight + C.menuTextOffset + menuTextOffset * fromIntegral (length rest)

-- | Creates a menu
createMenu :: String -> Picture -> Menu -> [(String, Menu)] -> Menu
createMenu title background prevMenu menuFields = Menu {header = title, menuBackground = background, fields = generateMenuFields menuFields, returnMenu = prevMenu}