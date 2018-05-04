module Player where

import Data.Map

import V2
import Piece

data Player = Player
    { pieces         :: Map V2I Piece
    , closeRookMoved :: Bool
    , farRookMoved   :: Bool
    , kingMoved      :: Bool
    , passant        :: V2I
    }

isInCheck :: Player -> Player -> Bool
isInCheck = undefined

isCheckMate :: Player -> Player -> Bool
isCheckMate = undefined

validTargets :: V2I -> Player -> Player -> [V2I]
validTargets = undefined

initWhitePlayer :: Player
initWhitePlayer = undefined

initBlackPlayer :: Player
initBlackPlayer = undefined
