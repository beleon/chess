{-# LANGUAGE OverloadedStrings #-}
module UI.Graphics where

import SDL
import Linear (V4(..))
import Control.Monad (unless)
import GHC.Word

initUI :: IO ()
initUI = do
  initializeAll
  window <- createWindow "Chess" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer 255

appLoop :: Renderer -> Word8 -> IO ()
appLoop renderer p = do
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
      wPressed = any wPressedEvent events
      qPressed = any closeWindow events
      p' = if wPressed then p - 10 else p
  rendererDrawColor renderer $= V4 0 p 0 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer p')
