-- | This module contains the data types
--   which represent the state of the game
module Model where

  import qualified Data.Set as S
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import Debug.Trace
  
  data InfoToShow = ShowNothing
                  | ShowANumber Int 
                  | ShowAChar   Char 
  
  
  
  data GameState = GameState {
                     infoToShow   :: InfoToShow
                   , elapsedTime  :: Float
                   , keys         :: S.Set Key
                   , spaceCounter :: Float
                   , player       :: Player
                   , asteroids    :: [Asteroid]
                   , bullets      :: [Bullet]
                   }
  
  data Player = Player {
                     statusp :: Status
                   , health :: Health
                   , sizep :: PlaySize
                   , positionp :: Position
                   , directionp :: Direction
                   , speedp :: Speed
                   , accel :: Acceleration
                   , upCounter    :: Float
                   }
  data Ufo = Enemy {
                   positionu :: Position
                    }
  
  data Asteroid = Asteroid {
                     speeda :: Speed
                   , sizea :: AstSize
                   , directiona :: Direction
                   , statusa :: Status
                   , positiona :: Position
                    }
  data Bullet = Bullet {
                     sizeb :: BullSize
                   , speedb :: Speed  
                   , positionb :: Position
                   , directionb :: Direction
                    }
  type Speed = Float

  baseSpeed :: Float
  baseSpeed = 5
  
  type AstSize = Int
  
  type BullSize = Int
  
  type PlaySize = Int
  
  type Direction = Int
  
  type Status = Int
  
  type Health = Int
  
  type Points = Int
  
  type Position = (Float, Float)
  
  type Acceleration = Float
  
  class Draw a where 
    draw :: a -> Picture
  
  instance (Draw a) => Draw [a] where
    draw a = Pictures (map draw a)

  instance Draw Asteroid where
    draw asteroid = color yellow $ circleSolid $ fromIntegral ((sizea asteroid) * 5)
  
  instance Draw Player where
    draw player = color white $ line [point1,point2,point3,point1]
      where (x,y) = positionp player
            point1 = newPoint (x,y) ((x-20),(y-20)) (directionp player)
            point2 = newPoint (x,y) (x,(y+20)) (directionp player)
            point3 = newPoint (x,y) ((x+20),(y-20)) (directionp player)

  newPoint :: (Float,Float) -> (Float,Float) -> Int -> (Float,Float)
  newPoint (x,y) (x',y') direction = (x3,y3)
    where radians = (fromIntegral direction) * 0.0174533
          (x2,y2) = (x'-x,y'-y)
          (xN,yN) = ((x2 * cos(radians)) - (y2 * sin(radians)),(x2 * sin(radians)) + (y2 * cos(radians)))
          (x3,y3) = (-1*(xN+x),yN+y)
  
  instance Draw Bullet where 
    draw bullet = translate x y $ color green $ circleSolid 4 
       where (x,y) = positionb bullet
  
  instance Draw Ufo where 
    draw ufo = translate x y $ color red $ circleSolid 8
      where (x,y) = positionu ufo
  

  class Move a where
    move :: a -> a
  
  instance (Move a) => Move [a] where
    move a = map move a

  instance Move Player where
    move player = player { positionp = positionp' }
      where positionp' | x' > 220 || x' < -220 = (-1*x', y')
                       | y' > 220 || y' < -220 = (x', -1*y')
                       | otherwise             = (x', y')
            x' = x+xChange
            y' = y+yChange
            (x,y) = positionp player
            (xChange, yChange) = changePos player
            counter = upCounter player

  instance Move Asteroid where
    move asteroid = asteroid { positiona = positiona' }
      where positiona' | x' > 220 || x' < -220 = (-1*x', y')
                       | y' > 220 || y' < -220 = (x', -1*y')
                       | otherwise             = (x', y')
            x' = x+xChange
            y' = y+yChange
            (x,y) = positiona asteroid
            (xChange, yChange) = changePos asteroid

  instance Move Bullet where
    move bullet = bullet { positionb = positionb' }
      where positionb' = (x', y')
            x' = x+xChange
            y' = y+yChange
            (x,y) = positionb bullet
            (xChange, yChange) = changePos bullet

  class ChangePos a where
    changePos :: a -> (Float,Float)

  instance ChangePos Player where
    changePos player | speed > baseSpeed = (xChange', yChange')
                    | otherwise         = (0,0)
      where yChange'  | speed < 10        = yChange speed direction
                      | otherwise         = yChange 10 direction
            xChange'  | speed < 10        = xChange speed direction
                      | otherwise         = xChange 10 direction
            speed     = changeSpeed counter (speedp player) (accel player)
            direction = directionp player
            counter   = upCounter player

  instance ChangePos Asteroid where 
    changePos asteroid = (xChange', yChange')
      where yChange'  = yChange speed direction
            xChange'  = xChange speed direction
            speed     = speeda asteroid
            direction = directiona asteroid

  instance ChangePos Bullet where
    changePos bullet = (xChange', yChange')
      where yChange'  = yChange speed direction
            xChange'  = xChange speed direction
            speed     = speedb bullet
            direction = directionb bullet

  xChange :: Float -> Int -> Float
  xChange speed direction = speed * sin(-1*(fromIntegral direction) * pi / (fromIntegral 180))

  yChange :: Float -> Int -> Float
  yChange speed direction = speed * cos(-1*(fromIntegral direction) * pi / (fromIntegral 180))

  changeDir :: Bool -> Player -> Player
  changeDir keyRight player | newDir < 360 = player { directionp = newDir }
                            | otherwise    = player { directionp = newDir - 360}
    where newDir | keyRight  = directionp player + 10
                 | otherwise = directionp player - 10

  -- | change speed based on the counter and acceleration
  changeSpeed :: Float -> Float -> Float -> Float
  changeSpeed counter speed accel | counter /= 0 = speed + counter*accel
                                  | otherwise    = speed - accel*0.0001

  {-instance Move Asteroid where
    move asteroid = asteroid { positiona += speeda asteroid }
  
  instance Move Bullet where
    move bullet = bullet { positionb += speedb bullet }
    
  instance Move Ufo where
    move ufo = ufo { positionu += speedu ufo }-}
  
  class Shoot a where
    shoot :: a -> GameState -> [Bullet]
    
  instance Shoot Player where
    shoot player gstate = newBullet : bullets gstate
      where newBullet = Bullet 30 4 (-1*x,y) (-1*(directionp player))
            (x,y)     = positionp player
  {-instance Shoot Ufo where
    shoot ufo gstate = gstate { bullets = newBullet : bullet }
      where newBullet = Bullet 4 (positionu ufo) (directionu ufo)-}
  
  initialState :: GameState
  initialState = GameState ShowNothing 0 S.empty 0 (Player 1 100 1 (0,0) 0 baseSpeed 10 0) [] []