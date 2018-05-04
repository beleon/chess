module V2 where

data V2 a = V2
    { xVal :: a
    , yVal :: a
    } deriving Show

type V2I = V2 Int

instance Num a => Num (V2 a) where
    (V2 x y) + (V2 u v) = V2 (x+u) (y+v)
    (*) = undefined
    abs (V2 x y) = V2 (abs x) (abs y)
    signum (V2 x y) = V2 (signum x) (signum y)
    fromInteger = undefined
    negate (V2 x y) = V2 (-x) (-y)

up :: V2I
up = V2 0 1

down :: V2I
down = V2 0 (-1)

left :: V2I
left = V2 (-1) 0

right :: V2I
right = V2 1 0

validSquare :: V2I -> Bool
validSquare (V2 x y) = x >= 0 && y >= 0 && x < 8 && y < 8
