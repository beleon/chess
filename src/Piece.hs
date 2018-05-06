module Piece where

import V2
import Util

data Piece = Q | K | N | R | P | B deriving (Eq, Show)

toPiece :: Char -> Maybe Piece
toPiece 'q' = Just Q
toPiece 'k' = Just K
toPiece 'n' = Just N
toPiece 'r' = Just R
toPiece 'p' = Just P
toPiece 'b' = Just B
toPiece _ = Nothing

targetList :: Piece -> Bool -> V2I -> [[V2I]]
targetList K _               = targetListKing
targetList Q _               = targetListQueen
targetList R _               = targetListRook
targetList B _               = targetListBishop
targetList N _               = targetListKnight
targetList P whitePlayerTurn = targetListPawn whitePlayerTurn

targetListKing :: V2I -> [[V2I]]
targetListKing v = map (filter validSquare)
                     [ [v + up]
                     , [v + down]
                     , [v + left]
                     , [v + right]
                     , [v + up + right]
                     , [v + up + left]
                     , [v + down + right]
                     , [v + down + left]
                     ]

targetListQueen :: V2I -> [[V2I]]
targetListQueen v = targetListRook v ++ targetListBishop v

targetListRook :: V2I -> [[V2I]]
targetListRook (V2 x y) = [ [ V2 x' y | x' <- [x-1,x-2..0] ]
                          , [ V2 x' y | x' <- [x+1,x+2..7] ]
                          , [ V2 x y' | y' <- [y-1,y-2..0] ]
                          , [ V2 x y' | y' <- [y+1,y+2..7] ]
                          ]

targetListBishop :: V2I -> [[V2I]]
targetListBishop (V2 x y) = let (ld, lu) = unzip [ ( V2 x' (y + x' - x)
                                                   , V2 x' (y - x' + x)
                                                   ) | x' <- [x-1,x-2..0] ]
                                (ru, rd) = unzip [ ( V2 x' (y + x' - x)
                                                   , V2 x' (y - x' + x)
                                                   ) | x' <- [x+1..7] ]
                            in map (filter validSquare) [ld, lu, ru, rd]

targetListKnight :: V2I -> [[V2I]]
targetListKnight v = map (filter validSquare)
                       [ [ v + up + up + left       ]
                       , [ v + up + up + right      ]
                       , [ v + left + left + down   ]
                       , [ v + left + left + up     ]
                       , [ v + down + down + left   ]
                       , [ v + down + down + right  ]
                       , [ v + right + right + down ]
                       , [ v + right + right + up   ]
                       ]

-- Important: algorithm that filters target list expects diagonal moves last.
targetListPawn :: Bool -> V2I -> [[V2I]]
targetListPawn whitePlayerTurn v@(V2 m n)
  = let d = if whitePlayerTurn then up else down
        b = whitePlayerTurn && n == 1 || not whitePlayerTurn && n == 6
    in map (filter validSquare)
         [ [v + d] ++ (if b then [ v + d + d ] else [])
         , [v + d + left]
         , [v + d + right]
         ]

canAttack :: Piece -> Bool -> V2I -> V2I -> Bool
canAttack K _               = canAttackKing
canAttack Q _               = canAttackQueen
canAttack R _               = canAttackRook
canAttack B _               = canAttackBishop
canAttack N _               = canAttackKnight
canAttack P whitePlayerTurn = canAttackPawn whitePlayerTurn

canAttackKing :: V2I -> V2I -> Bool
canAttackKing (V2 a b) (V2 u v) = abs (a-u) < 2 && abs (b-v) < 2
                                  && not (a == u && b == v)

canAttackQueen :: V2I -> V2I -> Bool
canAttackQueen v1 v2 = canAttackBishop v1 v2 || canAttackRook v1 v2

canAttackRook :: V2I -> V2I -> Bool
canAttackRook (V2 a b) (V2 u v) = (a == u || b == v)
                                  && not (a == u && b == v)
canAttackBishop :: V2I -> V2I -> Bool
canAttackBishop (V2 a b) (V2 u v) = abs (a-u) == abs (b-v)
                                    && not (a == u && b == v)

canAttackKnight :: V2I -> V2I -> Bool
canAttackKnight (V2 a b) (V2 u v) = let x = abs (a-u)
                                        y = abs (b-v)
                                    in min x y == 1 && max x y == 2

canAttackPawn :: Bool -> V2I -> V2I -> Bool
canAttackPawn isWhitePlayer (V2 a b) (V2 u v)
  = let d = if isWhitePlayer then 1 else -1
    in (v-b) == d && abs (a-u) == 1
