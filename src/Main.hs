{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


data World = World {
    shoot :: Bool,
    turning :: Float,
    force :: Vector,
    player :: Player
}

data Player = Player {
    x :: Float,
    y :: Float,
    facing :: Vector,
    heading :: Vector,
    picture :: Picture,
    power :: Float
}

main :: IO ()
main =
    play window black 60 initWorld render handleInputs update

window :: Display
window = InWindow "Asteroids" (sWd, sHt) (0, 0)

sWd :: Int
sWd = 640

sHt :: Int
sHt = 480

initWorld = World {
    turning = 0.0,
    force = (0.0, 0.0),
    shoot = False,
    player = initPlayer
}

initPlayer :: Player
initPlayer = Player {
    x = 1.0,
    y = 1.0,
    facing = (0.0, 1.0),
    power = 5.0,
    heading = (0.0, 0.0),
    picture = (Scale 10.0 10.0 (playerPicture))
}

playerPicture :: Picture
playerPicture = Color white $ pictures [ circle 0.5
                                       , leftSect
                                       , rightSect
                                       , bottomSect]
    where
        leftSect = line [((-0.75), (0.9)), (1.5, 0)]
        rightSect = line [((-0.75), (-0.9)), (1.5, 0)]
        bottomSect = line [((-0.25), (-0.75)), ((-0.25), 0.75)]

render game =
    Pictures [renderPlayer (player game)
            , dgHead (player game)
            , dgFace (player game)
            , debugText (player game)
            , debugText2 game
    ]
    where
        renderPlayer Player{..} = translate x y $ rotate ( radToDeg $ argV facing * (-1) ) picture
        dgHead Player{..} = translate x y $ color (makeColor 1 0 1 1) $ line [(0, 0), heading]
        dgFace Player{..} = translate x y $ color (makeColor 0 1 0 1) $ line [facing, mulSV 10 facing]
        debugText Player{..} = translate (-100) (-50) $ color (makeColor 1 1 1 1) $ Scale 0.1 0.1 $ Text (show x ++ " " ++ show y)
        debugText2 World{..} = translate (-100) (-25) $ color (makeColor 1 1 1 1) $ Scale 0.1 0.1 $ Text (show turning ++ " " ++ show force) 

degToVec = unitVectorAtAngle . degToRad . argV

handleInputs (EventKey (SpecialKey KeySpace) _ _ _) World{..} = World { turning = turning, force = force, shoot = True, player = player }

handleInputs (EventKey (SpecialKey KeyRight) Down _ _) World{..} = World { turning = -1.0, force = force, shoot = shoot, player = player }
handleInputs (EventKey (SpecialKey KeyRight) Up _ _) World{..} = World { turning = 0.0, force = force, shoot = shoot, player = player }

handleInputs (EventKey (SpecialKey KeyLeft) Down _ _) World{..} = World { turning = 1.0, force = force, shoot = shoot, player = player }
handleInputs (EventKey (SpecialKey KeyLeft) Up _ _) World{..} = World { turning = 0.0, force = force, shoot = shoot, player = player }

handleInputs (EventKey (SpecialKey KeyUp) Down _ _) World{..} = World { turning = turning, force = facing player, shoot = shoot, player = player }
handleInputs (EventKey (SpecialKey KeyUp) Up _ _) World{..} = World { turning = turning, force = (0.0, 0.0), shoot = shoot, player = player }
handleInputs _ world = world

update :: Float -> World -> World
update dt World{..} =
    World {
        turning = turning,
        force = force,
        shoot = False,
        player = updatePlayer player turning force
    }
    where
        updatePlayer Player{..} trn frc = Player {
                x = clmp ( x + (fst heading) * dt ) ( fromIntegral sWd ),
                y = clmp ( y + (snd heading) * dt ) ( fromIntegral sHt ),
                facing = rotateV ( trn * dt * 5 ) facing,
                power = power,
                heading = heading + mulSV power frc,
                picture = picture
            }

clmp x scrn
    | abs x > scrn / 2 = negate x
    | otherwise = x

