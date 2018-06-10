module Example.Wrapper where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Style.Declaration (PaddingValue, backgroundColor, padding)
import Styled.Components (element) as Styled
import Styled.Components.Effect (StyledM)
import Styled.Components.Types (Element, Element_, ID) as Styled

type State =
  { backgroundColor :: Maybe Color
  , padding :: Maybe PaddingValue
  }

wrapper
  :: forall p i
   . Styled.ID
  -> State
  -> StyledM (Styled.Element _ p i)
wrapper = Styled.element HH.section
  [ \state -> Array.catMaybes
      [ backgroundColor <$> state.backgroundColor
      , padding <$> state.padding
      ]
  ]

wrapper_
  :: forall p i
   . Styled.ID
  -> State
  -> StyledM (Styled.Element_ p i)
wrapper_ id state = wrapper id state <*> pure []
