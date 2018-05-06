module Util where

import V2

validSquare :: V2I -> Bool
validSquare (V2 x y) = x >= 0 && y >= 0 && x < 8 && y < 8

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = error "Index out of bounds"
replace 0 e (x:xs) = e:xs
replace n e (x:xs)
    | n < 0     = error "Negative index"
    | otherwise = x:replace (n-1) e xs
