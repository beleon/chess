module Model.Util where

import Linear (V2(..))

type V2I = V2 Int

validSquare :: V2I -> Bool
validSquare (V2 x y) = x >= 0 && y >= 0 && x < 8 && y < 8

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = error "Index out of bounds"
replace 0 e (x:xs) = e:xs
replace n e (x:xs)
    | n < 0     = error "Negative index"
    | otherwise = x:replace (n-1) e xs

squaresInBetween :: V2I -> V2I -> [V2I]
squaresInBetween v1 v2 = let dv@(V2 u v) = v2 - v1
                             l = max (abs u) (abs v)
                             d = signum dv
                         in [v1 + (scale d x) | x <- [1..l-1]]

scale :: V2I -> Int -> V2I
scale (V2 u v) x = V2 (x*u) (x*v)

up :: V2I
up = V2 0 1

down :: V2I
down = V2 0 (-1)

left :: V2I
left = V2 (-1) 0

right :: V2I
right = V2 1 0
