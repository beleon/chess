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

import Model.Util
import Model.Player
import Model.Piece (Piece)
import qualified Model.Piece as Pc
import Model.Game
import qualified Model.Game as G

initUI :: Game -> IO ()
initUI g = do
  initializeAll
  window <- createWindow "Chess" $ defaultWindow {windowInitialSize = V2 800 800}
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer 255 g Nothing

appLoop :: Renderer -> Word8 -> Game -> Maybe V2I -> IO ()
appLoop renderer p g c = do
  events <- pollEvents
  let closeWindow event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          WindowClosedEvent (WindowClosedEventData window) -> True
          _ -> False
      wPressedEvent event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeW
          _ -> False
      toInd (P (V2 x y)) = V2 (fromIntegral x `div` 100) (7 - (fromIntegral y `div` 100))
      clickedSquare :: Event -> Maybe V2I
      clickedSquare event =
        case eventPayload event of
          MouseButtonEvent mouseEvent -> if mouseButtonEventButton mouseEvent == ButtonLeft  && mouseButtonEventMotion mouseEvent == Released then (pure $ toInd $ mouseButtonEventPos mouseEvent) else Nothing
          _ -> Nothing
      wPressed = any wPressedEvent events
      qPressed = any closeWindow events
      p' = if wPressed then p - 10 else p
      click = listToMaybe $ catMaybes $ map clickedSquare events :: Maybe V2I
      c' = if click == Nothing then c else if c == Nothing then click else Nothing
      (err, g') = if c /= Nothing && click /= Nothing
                  then (either (flip (,) g . Just) ((,) Nothing) $ G.move g (fromJust c, fromJust click))
                  else (Nothing, g)
  fromMaybe (pure ()) $ fmap putStrLn err
  drawBoard renderer
  drawPieces renderer g'
  rendererDrawColor renderer $= V4 0 p 0 255
  if wPressed then clear renderer else pure ()
  present renderer
  unless qPressed $ appLoop renderer p' g' c'

drawBoard :: Renderer -> IO ()
drawBoard r = do
  let rects = [[Rectangle (P (V2 x y)) $ V2 (x+100) (y+100) | x <- [0,100..700]] | y <- [0,100..700]]
      (_, c) = mapAccumL (\a b -> (tail a, zip a b)) (cycle [white, brown]) rects :: ([V4 Word8], [[(V4 Word8, Rectangle CInt)]])
  clear r
  forM_ (concat c) (\(col, rec) ->
             (rendererDrawColor r $= col)
             >> (fillRect r $ Just $ rec))

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

brown :: V4 Word8
brown = V4 181 136 99 255

white :: V4 Word8
white = V4 240 217 181 255

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
