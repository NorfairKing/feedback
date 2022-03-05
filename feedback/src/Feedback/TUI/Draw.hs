{-# LANGUAGE RecordWildCards #-}

module Feedback.TUI.Draw where

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Core
import Feedback.TUI.State
import Graphics.Vty.Attributes

buildAttrMap :: State -> AttrMap
buildAttrMap = const $ attrMap defAttr []

drawTui :: State -> [Widget ResourceName]
drawTui State {..} =
  [ vBox
      [ str $ show stateCommand,
        hBorder,
        str $ show stateCurrentProcess,
        hBorder,
        vBox $ map (str . show) stateEvents
      ]
  ]
