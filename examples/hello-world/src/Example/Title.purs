module Example.Title where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Style.Declaration (FontSizeValue, TextAlignValue, color, fontSize, textAlign)
import Styled.Components (element) as Styled
import Styled.Components.Constructors (css)
import Styled.Components.Effect (StyledM)
import Styled.Components.Types (Element, Element_, ID) as Styled

type State =
  { color :: Maybe Color
  , fontSize :: Maybe FontSizeValue
  , textAlign :: Maybe TextAlignValue
  }

title
  :: forall p i
   . Styled.ID
  -> State
  -> StyledM (Styled.Element _ p i)
title = Styled.element HH.h1
  [ css \state -> Array.catMaybes
      [ color <$> state.color
      , fontSize <$> state.fontSize
      , textAlign <$> state.textAlign
      ]
  ]

title_
  :: forall p i
   . Styled.ID
  -> State
  -> StyledM (Styled.Element_ p i)
title_ id state = title id state <@> []
