module Piece where

import V2

data Piece = Q | K | N | R | P | B deriving Show

toPiece :: Char -> Maybe Piece
toPiece 'q' = Just Q
toPiece 'k' = Just K
toPiece 'n' = Just N
toPiece 'r' = Just R
toPiece 'p' = Just P
toPiece 'b' = Just B
toPiece _ = Nothing

targetList :: Piece -> V2I -> [[V2I]]
targetList K = targetListKing
targetList Q = targetListQueen
targetList R = targetListRook
targetList B = targetListBishop
targetList N = targetListKnight
targetList P = targetListPawn

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
-- Return pawn moves for white player. Filter algorithm will flip on black
-- player turn.
targetListPawn :: V2I -> [[V2I]]
targetListPawn v = map (filter validSquare)
                     [ [ v + up, v + up + up ]
                     , [ v + up + left       ]
                     , [ v + up + right      ]
                     ]
