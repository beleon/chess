module Model.Player where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List
import Data.Maybe (fromJust)
import Linear(V2(..))

import Model.Piece
import Model.Util

data Player = Player
    { pieces         :: Map V2I Piece
    , isWhite        :: Bool
    , closeRookMoved :: Bool
    , farRookMoved   :: Bool
    , kingMoved      :: Bool
    , passant        :: Maybe V2I
    }

isInCheck :: Player -> Player -> Bool
isInCheck self other
  = let (kv, _) = fromJust $ find (\(_, p) -> p == K) $ M.toList $ pieces self
    in isUnderAttack kv self other

isCheckMate :: Player -> Player -> Bool
isCheckMate self other = isInCheck self other
                         && (all null $ map (\v -> validTargets v self other)
                                            $ M.keys $ pieces self)

-- Generate valid target list.
validTargets :: V2I -> Player -> Player -> [V2I]
validTargets v@(V2 m n) self other
  = let sp = pieces self
        op = pieces other
        iw = isWhite self
        p  = sp ! v
        tl = targetList p iw v
    in if p == P
       then let d = if iw then up else down
                tp v = pure v == fmap (+d) (passant other)
                (x:r) = tl
                x' = takeWhile (\v -> not (M.member v sp || M.member v op)) x
                r' = map (filter (\v -> M.member v op || tp v)) r
            in concat (x':r')
       else let btake []     = []
                btake (x:xs) = if M.member x sp then []
                               else if M.member x op then [x]
                                    else x:btake xs
                ica nv = uncurry isInCheck $ unsafeMove v nv self other
                kua = isUnderAttack v self other
                cr = not $ closeRookMoved self
                fr = not $ farRookMoved self
                ks = not $ kingMoved self
                (cs, fs) = (v + right + right, v + left + left)
                (crv, frv) = (cs + right, fs + left + left)
                cm = squaresInBetween v (cs + right)
                fm = squaresInBetween v (fs + left)
                occ v = M.member v sp || M.member v op
                cstlm = if p == K && ks && not kua
                        then (if cr
                                 && (all (not . occ) $ squaresInBetween v crv)
                                 && pure R == M.lookup crv sp
                                 && all (not . flip
                                               (flip isUnderAttack self) other)
                                        cm
                              then [cs] else [])
                             ++
                             (if fr
                                 && (all (not . occ) $ squaresInBetween v frv)
                                 && pure R == M.lookup frv sp
                                 && all (not . flip
                                               (flip isUnderAttack self) other)
                                        fm
                              then [fs] else [])
                        else []
            in filter (not . ica) (concat $ map btake tl) ++ cstlm

isUnderAttack :: V2I -> Player -> Player -> Bool
isUnderAttack v@(V2 m n) self other
  = let sp = pieces self
        op = pieces other
        iw = isWhite self
        as (v2, p) = canAttack p iw v2 v
                     && (not (p == Q || p == B || p == R)
                         || (let b = squaresInBetween v2 v
                             in all (flip M.notMember sp) b
                                && all (flip M.notMember op) b))
    in any as $ M.toList op

unsafeMove :: V2I -> V2I -> Player -> Player -> (Player, Player)
unsafeMove from@(V2 a b) to self other
  = let sp = pieces self
        op = pieces other
        iw = isWhite self
        blp = iw && b == 1 || not iw && b == 6
        blr = iw && b == 0 || not iw && b == 7
        d = if iw then up else down
        p = sp ! from
        sp' = M.insert to p $ M.delete from sp
        op' = M.delete to op
        self' = if p == P && blp && scale d 2 == to - from
                then self {passant=Just to}
                else self {passant=Nothing}
        self'' = if p == K
                 then self' {kingMoved=True}
                 else self'
        self''' = if p == R && blr
                  then if a == 0
                       then self'' {farRookMoved=True}
                       else if a == 7
                            then self'' {closeRookMoved=True}
                       else self''
                  else self''
        self'''' = self''' {pieces=sp'}
        other' = other {pieces=op'}
    in (self'''', other')

move :: V2I -> V2I -> Player -> Player -> Either String (Player, Player)
move from to self other = if elem to $ validTargets from self other
                          then Right $ unsafeMove from to self other
                          else Left "Illegal move!"

initWhitePlayer :: Player
initWhitePlayer =
  let y = 0
      ps = [(V2 x (y+1), P) | x <- [0..7]]
           ++ [(V2 x y, R) | x <- [0,7]]
           ++ [(V2 x y, N) | x <- [1,6]]
           ++ [(V2 x y, B) | x <- [2,5]]
           ++ [(V2 3 y, Q)]
           ++ [(V2 4 y, K)]
  in Player (M.fromList ps) True False False False Nothing

initBlackPlayer :: Player
initBlackPlayer =
  let y = 7
      ps = [(V2 x (y-1), P) | x <- [0..7]]
           ++ [(V2 x y, R) | x <- [0,7]]
           ++ [(V2 x y, N) | x <- [1,6]]
           ++ [(V2 x y, B) | x <- [2,5]]
           ++ [(V2 3 y, Q)]
           ++ [(V2 4 y, K)]
  in Player (M.fromList ps) False False False False Nothing
