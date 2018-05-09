module Model.Game where

import Data.List (intercalate)
import Linear (V2(..))
import qualified Data.Map as M
import Data.Tuple (swap)

import Model.Player
import Model.Piece
import Model.Util
import qualified Model.Player as Pl

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
move g (v1, v2) =
  let (self, other) = (if iwt then id else swap) (wp, bp)
      wp = whitePlayer g
      bp = blackPlayer g
      iwt = whitePlayerTurn g
      wcm = isCheckMate wp bp
      bcm = isCheckMate bp wp
      islp = M.member v1 $ pieces self
      isvm = elem v2 $ validTargets v1 self other
  in if islp && not wcm && not bcm && isvm
     then let np = unsafeMove v1 v2 self other
              h' = (v1,v2):history g
              iwt' = not iwt
              (wp', bp') = (if iwt then id else swap) np
          in Right $ Game wp' bp' h' iwt'
     else Left $ "Illegal Move: " ++ show v1 ++ "," ++ show v2

printBoard :: [V2I] -> String
printBoard = intercalate "\n" . reverse . foldl (\s (V2 x y) -> replace y (replace x '#' (s!!y)) s) (replicate 8 $ replicate 8 ' ')

putPiece :: Bool -> V2I -> Piece -> Game -> Game
putPiece iw v p g =
  let wp' = (if iw then (Pl.putPiece v p) else id) $ whitePlayer g
      bp' = (if not iw then (Pl.putPiece v p) else id) $ blackPlayer g
  in g {whitePlayer=wp', blackPlayer=bp'}
