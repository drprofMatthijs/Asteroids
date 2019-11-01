-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Set as S

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | secs == 20
  = -- Spawn asteroids and do all actions in a step
    return $ checkDir $ increaseSpaceCounter secs $ increaseUpCounter secs $ gstate { elapsedTime = elapsedTime gstate + secs,
                                                                                      asteroids = move ((Asteroid baseSpeed 1 20 1 (-220, 0)) : asteroids gstate) }
  | otherwise
  = -- Do all actions in a step
    return $ checkDir $ moveBullets $ spaceBar secs $ increaseUpCounter secs $ gstate { elapsedTime = elapsedTime gstate + secs }

increaseUpCounter :: Float -> GameState -> GameState
increaseUpCounter secs gstate 
  | S.member (SpecialKey KeyUp) (keys gstate) = gstate { player    = move player' }
  | otherwise                                 = gstate { player    = move player'' }
    where player'  = (player gstate) { upCounter = upCounter (player gstate) + secs }
          player'' = (player gstate) { upCounter = 0 }

moveBullets :: GameState -> GameState
moveBullets gstate | null kogels = gstate
                   | otherwise   = gstate { bullets = move kogels }
            where kogels = bullets gstate

spaceBar :: Float -> GameState -> GameState
spaceBar secs gstate 
    | S.member (SpecialKey KeySpace) (keys gstate) = gstate {bullets = shoot (player gstate) gstate}
    | otherwise                                    = gstate

increaseSpaceCounter :: Float -> GameState -> GameState
increaseSpaceCounter secs gstate 
  | S.member (SpecialKey KeySpace) (keys gstate) = gstate { spaceCounter = spaceCounter gstate + secs }
  | otherwise                                    = gstate { spaceCounter = 0 }

checkDir :: GameState -> GameState
checkDir gstate | S.member (SpecialKey KeyRight) (keys gstate) &&
                  S.member (SpecialKey KeyLeft) (keys gstate)  = gstate
                | S.member (SpecialKey KeyRight) (keys gstate) = gstate { player = changeDir True (player gstate) }
                | S.member (SpecialKey KeyLeft) (keys gstate)  = gstate { player = changeDir False (player gstate) }
                | otherwise                                    = gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey k Down _ _) gstate
  = -- If the user presses a character key, add it to the set of pressed keys
    gstate { keys = S.insert k (keys gstate) }
inputKey (EventKey k Up _ _) gstate
  = -- If the user releases w, delete it from the currently pressed down keys
    gstate { keys = S.delete k (keys gstate) }
inputKey _ gstate = gstate -- Otherwise keep the same