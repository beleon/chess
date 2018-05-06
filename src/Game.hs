module Game where

import Data.List (intercalate)

import Player
import V2
import Util

type Move = (V2I, V2I)

data Game = Game
    { whitePlayer     :: Player
    , blackPlayer     :: Player
    , history         :: [Move]
    , whitePlayerTurn :: Bool
    }

initGame :: Game
initGame = Game initWhitePlayer initBlackPlayer [] True

player :: Game -> Player
player g = if whitePlayerTurn g then whitePlayer g else blackPlayer g

enemy :: Game -> Player
enemy g = if whitePlayerTurn g then blackPlayer g else whitePlayer g

move :: Game -> Move -> Either String Game
move = undefined

printBoard :: [V2I] -> String
printBoard = intercalate "\n" . reverse . foldl (\s (V2 x y) -> replace y (replace x '#' (s!!y)) s) (replicate 8 $ replicate 8 ' ')
