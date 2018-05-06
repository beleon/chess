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

instance Eq a => Eq (V2 a) where
    (V2 a b) == (V2 u v) = a == u && b == v

instance Ord a => Ord (V2 a) where
    (V2 a b) `compare` (V2 u v) = case compare a u of
                                    EQ -> compare b v
                                    a  -> a

up :: V2I
up = V2 0 1

down :: V2I
down = V2 0 (-1)

left :: V2I
left = V2 (-1) 0

right :: V2I
right = V2 1 0

scale :: V2I -> Int -> V2I
scale (V2 u v) x = V2 (x*u) (x*v)

squaresInBetween :: V2I -> V2I -> [V2I]
squaresInBetween v1 v2 = let dv@(V2 u v) = v2 - v1
                             l = max (abs u) (abs v)
                             d = signum dv
                         in [v1 + (scale d x) | x <- [1..l-1]]
