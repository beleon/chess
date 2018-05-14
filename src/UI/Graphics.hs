{-# LANGUAGE OverloadedStrings #-}
module UI.Graphics where

import SDL
import Linear (V4(..))
import Control.Monad (unless)
import GHC.Word
import Control.Monad
import Data.List
import Foreign.C.Types
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple (swap)

import Model.Util
import Model.Player
import Model.Piece (Piece)
import qualified Model.Piece as Pc
import Model.Game
import qualified Model.Game as G
import UI.Color

initUI :: Game -> IO ()
initUI g = do
  initializeAll
  window <- createWindow "Chess" $ defaultWindow {windowInitialSize = V2 800 800}
  renderer <- createRenderer window (-1) defaultRenderer
  drawBoard renderer
  drawPieces renderer g
  present renderer
  appLoop renderer g Nothing False

appLoop :: Renderer -> Game -> Maybe V2I -> Bool -> IO ()
appLoop renderer g c s = do
  events <- pollEvents
  let closeWindow event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          WindowClosedEvent (WindowClosedEventData window) -> True
          _ -> False
      toInd (V2 x y) = V2 (fromIntegral x `div` 100) (7 - (fromIntegral y `div` 100))
      clickedSquare event =
        case eventPayload event of
          MouseButtonEvent mouseEvent -> if mouseButtonEventButton mouseEvent == ButtonLeft  && mouseButtonEventMotion mouseEvent == Released then (pure $ (\(P v) -> v) $ mouseButtonEventPos mouseEvent) else Nothing
          _ -> Nothing
      close = any closeWindow events
      rawClick = listToMaybe $ catMaybes $ map clickedSquare events
      click = fmap toInd rawClick
      (self, other) = (if whitePlayerTurn g then id else swap) (whitePlayer g, blackPlayer g)
      isMove = not s && click /= Nothing && c /= Nothing
      isLegalMove = isMove && elem (fromJust click) (validTargets (fromJust c) self other)
      legalSelect = fromMaybe False $ fmap (\(V2 x y) -> x >= 200 && x <= 600 && y >= 350 && y <= 450) rawClick
      c' = if isMove || s && not s' then Nothing else if click == Nothing then c else if M.member (fromJust click) (pieces self) then click else Nothing
      (err, g') = if not s && c /= Nothing && click /= Nothing
                  then (either (flip (,) g . Just) ((,) Nothing) $ G.move g (fromJust c, fromJust click))
                  else (Nothing, g)
      s' = if click == Nothing then s else if s && legalSelect then False else s || (isLegalMove && M.lookup (fromJust c) (pieces self) == Just Pc.P && ((whitePlayerTurn g && (\(V2 _ y) -> y == 7) (fromJust click)) || (not (whitePlayerTurn g) && (\(V2 _ y) -> y == 0) (fromJust click))))
      g'' = if s && legalSelect then let p = case (\(V2 x _) -> x) (fromJust rawClick) of
                                               a | a >= 500 -> Pc.N
                                                 | a >= 400 -> Pc.B
                                                 | a >= 300 -> Pc.R
                                                 | a >= 200 -> Pc.Q
                                     in G.putPiece (not (whitePlayerTurn g')) (snd $ head $ history g') p g'
                                else g'
  fromMaybe (pure ()) $ fmap putStrLn err
  if not s' && c == Nothing && click /= Nothing && M.member (fromJust click) (pieces self) then drawBoard renderer >> drawPickIndicator renderer (fromJust click) >> drawPieces renderer g'' >> drawMoveIndicators renderer (validTargets (fromJust click) self other) >> present renderer else pure ()
  if not s' && (err /= Nothing || (c /= Nothing && click /= Nothing)) then drawBoard renderer >> drawPieces renderer g'' >> present renderer else pure ()
  if s' && not s then drawBoard renderer >> drawPieces renderer g'' >> drawPieceSelection (whitePlayerTurn g) renderer >> present renderer else pure ()
  if s && not s' then drawBoard renderer >> drawPieces renderer g'' >> present renderer else pure ()
  unless close $ appLoop renderer g'' c' s'

drawPieceSelection :: Bool -> Renderer -> IO ()
drawPieceSelection iw r = do
  rendererDrawColor r $= V4 200 200 200 255
  fillRect r $ Just $ Rectangle (P $ V2 200 350) (V2 400 100)
  rendererDrawColor r $= V4 255 200 200 255
  if iw then (rendererDrawColor r $= V4 255 255 255 255) else (rendererDrawColor r $= V4 0 0 0 255)
  drawPiece' r (V2 200 350) Pc.Q
  drawPiece' r (V2 300 350) Pc.R
  drawPiece' r (V2 400 350) Pc.B
  drawPiece' r (V2 500 350) Pc.N

drawBoard :: Renderer -> IO ()
drawBoard r = do
  let rects = [[Rectangle (P (V2 x y)) $ V2 (x+100) (y+100) | x <- [0,100..700]] | y <- [0,100..700]]
      (_, c) = mapAccumL (\a b -> (tail a, zip a b)) (cycle [light, dark]) rects :: ([V4 Word8], [[(V4 Word8, Rectangle CInt)]])
  forM_ (concat c) (\(col, rec) ->
             (rendererDrawColor r $= col)
             >> (fillRect r $ Just $ rec))

drawPickIndicator :: Renderer -> V2I -> IO ()
drawPickIndicator r (V2 x y) = do
  let c = if even (y + x `mod` 2) then dark + V4 20 20 20 0 else light - V4 20 20 20 0
  rendererDrawColor r $= c
  fillRect r $ Just $ Rectangle (P $ V2 (fromIntegral $ x * 100) (fromIntegral $ (7 - y) * 100)) (V2 100 100)

drawMoveIndicators :: Renderer -> [V2I] -> IO ()
drawMoveIndicators r mvs = do
  rendererDrawColor r $= V4 0 255 0 255
  forM_ mvs (\(V2 x y) -> fillRect r $ Just $ Rectangle (P $ V2 (fromIntegral $ x * 100 + 40) (fromIntegral $ (7 - y) * 100 + 40)) $ V2 20 20)

drawPieces :: Renderer -> Game -> IO ()
drawPieces r g = do
  let wc = V4 255 255 255 255 :: V4 Word8
      bc = V4 0 0 0 255 :: V4 Word8
      wp = M.toList $ pieces $ whitePlayer g
      bp = M.toList $ pieces $ blackPlayer g
  rendererDrawColor r $= wc
  forM_ wp (\(v, p) -> drawPiece r v p)
  rendererDrawColor r $= bc
  forM_ bp (\(v, p) -> drawPiece r v p)

drawPiece :: Renderer -> V2I -> Piece -> IO ()
drawPiece r (V2 x y) p = do
  let v' = V2 (fromIntegral $ x * 100) (fromIntegral $ (7 - y) * 100) :: V2 CInt
      pl = linify p
      pl' = zip (last pl:init pl) pl
      pl'' = map (\(P v1, P v2) -> (P $ v1 + v', P $ v2 + v')) pl'
  forM_ pl'' (\(p1, p2) -> drawLine r p1 p2)

drawPiece' :: Renderer -> V2 CInt -> Piece -> IO ()
drawPiece' r v p = do
  let pl = linify p
      pl' = zip (last pl:init pl) pl
      pl'' = map (\(P v1, P v2) -> (P $ v1 + v, P $ v2 + v)) pl'
  forM_ pl'' (\(p1, p2) -> drawLine r p1 p2)

linify :: Piece -> [Point V2 CInt]
linify Pc.Q = queen
linify Pc.K = king
linify Pc.N = knight
linify Pc.R = rook
linify Pc.P = pawn
linify Pc.B = bishop

king :: [Point V2 CInt]
king = [ P $ V2 20 20
       , P $ V2 20 80
       , P $ V2 40 80
       , P $ V2 40 60
       , P $ V2 60 80
       , P $ V2 80 80
       , P $ V2 50 50
       , P $ V2 80 20
       , P $ V2 60 20
       , P $ V2 40 40
       , P $ V2 40 20
       ]

pawn :: [Point V2 CInt]
pawn = [ P $ V2 20 20
       , P $ V2 20 80
       , P $ V2 40 80
       , P $ V2 40 50
       , P $ V2 80 50
       , P $ V2 80 20
       ]

queen :: [Point V2 CInt]
queen = [ P $ V2 20 20
        , P $ V2 20 80
        , P $ V2 60 80
        , P $ V2 65 70
        , P $ V2 70 80
        , P $ V2 80 70
        , P $ V2 75 60
        , P $ V2 80 50
        , P $ V2 80 20
        ]

knight :: [Point V2 CInt]
knight = [ P $ V2 20 20
         , P $ V2 20 80
         , P $ V2 40 80
         , P $ V2 40 40
         , P $ V2 60 80
         , P $ V2 80 80
         , P $ V2 80 20
         , P $ V2 60 20
         , P $ V2 60 60
         , P $ V2 40 20
         ]


rook :: [Point V2 CInt]
rook = [ P $ V2 20 20
       , P $ V2 20 80
       , P $ V2 40 80
       , P $ V2 40 60
       , P $ V2 60 80
       , P $ V2 80 80
       , P $ V2 40 45
       , P $ V2 80 45
       , P $ V2 80 20
       ]

bishop :: [Point V2 CInt]
bishop = [ P $ V2 20 20
         , P $ V2 20 80
         , P $ V2 80 80
         , P $ V2 80 60
         , P $ V2 40 50
         , P $ V2 80 40
         , P $ V2 80 20
         ]
