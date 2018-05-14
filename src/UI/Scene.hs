module UI.Scene where

import Data.List
import Data.Maybe
import Control.Arrow
import Linear (V4(..))
import GHC.Word
import Control.Monad

import UI.Color

data Layout = LVert | LHor
data Component = HPanel {layout :: Layout, visible :: Bool, children :: [Component]}
               | HBox {width :: Int, height :: Int, bg :: Maybe (V4 Word8)}
               -- JComboBox, JLabel, JList, JPanel, JPopupMenu, JProgressBar, JScrollBar, JSlider, JSpinner, JTabbedPane, JTextComponent

data Layer = Layer {active :: Bool, xOff :: Int, yOff :: Int, root :: Component}
data Scene = Scene {layers :: [Layer]}

chessScene :: Scene
chessScene =
  let sl = 100 -- square side length
      gs = 8 * sl -- size of the complete board
      clSquare = HBox sl sl
      lines = [if even x then bl else wl | x <- [0..7]]
      bl = take 8 $ map clSquare $ map Just $ cycle [dark, light]
      wl = take 8 $ map clSquare $ map Just $ cycle [light, dark]
      gameRow = HPanel LHor True $ map (HPanel LHor True) lines
      gameComponent = HPanel LVert True $ replicate 8 gameRow
      selectorComponent = HBox gs gs Nothing
      menuComponent = HBox gs gs Nothing
  in Scene [ Layer True 0 0 gameComponent
           , Layer False (2 * sl) (3 * sl + sl `div` 2) selectorComponent
           , Layer False 0 0 $ HBox gs gs Nothing
           , Layer False (2 * sl) sl menuComponent
           ]

componentWidth :: Component -> Int
componentWidth (HBox w _ _) = w
componentWidth (HPanel l _ c) =
  case l of
    LVert -> maximum $ map componentWidth c
    LHor  -> sum $ map componentWidth c

componentHeight :: Component -> Int
componentHeight (HBox _ h _) = h
componentHeight (HPanel l _ c) =
  case l of
    LVert -> sum $ map componentHeight c
    LHor  -> maximum $ map componentHeight c

clickedElem :: Int -> Int -> Scene -> Maybe [Int]
clickedElem x y (Scene l) =
  let clickedLayerElem :: Int -> Int -> Layer -> Maybe [Int]
      clickedLayerElem x y (Layer a xo yo r)
        | not a || x < xo || y < yo
          || x > xo + componentWidth r || y > yo + componentHeight r = Nothing
        | otherwise = clickedComponentElem (x - xo) (y - yo) r
      clickedComponentElem :: Int -> Int -> Component -> Maybe [Int]
      clickedComponentElem _ _ (HBox _ _ _) = Just []
      clickedComponentElem x y (HPanel l v c) =
        let clickedChild :: Int -> Int -> [(Component, Int)] -> (Int, Int, Component)
            clickedChild _ _ [] = error "clickedChild: Should not be reached."
            clickedChild n limit ((x,l):xs) = if limit > l
                                              then clickedChild (n+1) (limit-l) xs
                                              else (limit, n, x)
            clickedGrandChild :: (Int -> Component -> Maybe [Int]) -> (Int, Int, Component) -> Maybe [Int]
            clickedGrandChild f (limit, n, c) = fmap (n:) $ f limit c

            cc :: Maybe [Int]
            cc = case l of
                   LVert -> clickedGrandChild (clickedComponentElem x) $ clickedChild x y $ map (id &&& componentHeight) c
                   LHor  -> clickedGrandChild (flip clickedComponentElem y) $ clickedChild x y$ map (id &&& componentWidth) c
        in if not v then Nothing else cc
  in join $ fmap
            (\(i,l) -> fmap (i:) l)
            $ find
              ((/= Nothing) . snd)
              $ reverse $ zip [0..] $ map (clickedLayerElem x y) l
