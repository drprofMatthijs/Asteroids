-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . render

render :: GameState -> Picture
render gstate = Pictures [draw (player gstate), draw (bullets gstate)]
-- om meerdere dingen te tekenen, gebruik pictures
-- pictures is een functie die een lijst van pictures omzet in een picture
-- dus pictures [draw (player gstate), draw (asteroid gstate)]