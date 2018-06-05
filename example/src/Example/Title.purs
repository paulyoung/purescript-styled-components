module Example.Title where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Style.Declaration (FontSizeValue, TextAlignValue, color, fontSize, textAlign)
import Styled.Components as Styled

type State s =
  ( color :: Maybe Color
  , fontSize :: Maybe FontSizeValue
  , textAlign :: Maybe TextAlignValue
  | s
  )

title
  :: forall s p i
   . { | State s }
  -> Array (HH.IProp _ i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
title = Styled.element HH.h1
  [ \state -> Array.catMaybes
      [ color <$> state.color
      , fontSize <$> state.fontSize
      , textAlign <$> state.textAlign
      ]
  ]

title_ :: forall s p i. { | State s } -> Array (HH.HTML p i) -> HH.HTML p i
title_ state = title state []
