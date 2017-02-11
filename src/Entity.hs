module Entity where

type Position = (Float, Float)
type Force = (Float, Float)
type Radius = Float
type Heading = Float


type Velocity = (Float, Float)
type Index = Int

data Entity =
        Asteroid Index Position Radius Heading
    |   Ship Index Position Radius Heading

instance Eq Entity where
 a1 == a2 = entityIx a1 == entityIx a2

instance Ord Entity where
 compare a1 a2 = compare ( actorIx a1 ) ( actorIx a2 )

actorIx :: Actor -> Index
actorIx actor
    = case actor of
        Asteroid ix _ _ _ -> ix
        Ship     ix _ _ _ -> ix
