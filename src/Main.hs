{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


data World = World {
    bullets :: [Bullet],
    shoot :: Bool,
    turning :: Float,
    force :: Vector,
    pushing :: Float,
    player :: Player
}

data Player = Player {
    x :: Float,
    y :: Float,
    facing :: Vector,
    heading :: Vector,
    picture :: Picture,
    power :: Float,
    friction :: Float
}

data Bullet = Bullet {
    pos :: Vector,
    dir :: Vector,
    life :: Float
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
    bullets = [],
    turning = 0.0,
    force = (0.0, 0.0),
    shoot = False,
    pushing = 0.0,
    player = initPlayer
}

initPlayer :: Player
initPlayer = Player {
    x = 1.0,
    y = 1.0,
    facing = (0.0, 1.0),
    power = 5.0,
    heading = (0.0, 0.0),
    picture = (Scale 10.0 10.0 (playerPicture)),
    friction = 0.5
}

bulletPicture :: Picture
bulletPicture = Color white $ circleSolid 1

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

handleInputs (EventKey key state _ _) World{..} = World {
        bullets = bullets,
        turning = trn key state turning,
        force = facing player,
        shoot = sht key state,
        player = player,
        pushing = psh key state pushing
    }
    where
        trn (SpecialKey KeyRight) Down _ = -1.0
        trn (SpecialKey KeyRight) Up _ = 0
        trn (SpecialKey KeyLeft) Down _ = 1.0
        trn (SpecialKey KeyLeft) Up _ = 0
        trn _ _ turning = turning
        psh (SpecialKey KeyUp) Down _ = 1.0
        psh (SpecialKey KeyUp) Up _ = 0.0
        psh _ _ push = push
        sht (SpecialKey KeySpace) Down = True
        sht _ _ = False
handleInputs _ world = world

update :: Float -> World -> World
update dt World{..} =
    World {
        bullets = map (\Bullet{..} -> Bullet {
                pos = pos + dir,
                dir = dir,
                life = life - dt
            }) bullets,
        turning = turning,
        force = force,
        shoot = False,
        pushing = pushing,
        player = updatePlayer player turning force pushing
    }
    where
        updatePlayer Player{..} trn frc psh = Player {
                x = clmp ( x + (fst heading) * dt ) ( fromIntegral sWd ),
                y = clmp ( y + (snd heading) * dt ) ( fromIntegral sHt ),
                facing = rotateV ( trn * dt * 5 ) facing,
                power = power,
                heading = heading_ psh heading power frc friction,
                picture = picture,
                friction = friction
            }
            where
                heading_ p h pow frc fric
                    | p == 1.0 = mulSV p (h + mulSV pow frc)
                    | otherwise = roundVector $ mulSV (1 - dt * fric) h

roundVector vec
    | magV vec < 1 = (0, 0)
    | otherwise = vec

clmp x scrn
    | abs x > scrn / 2 = negate x
    | otherwise = x

