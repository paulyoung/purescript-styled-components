module Example.Wrapper where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Style.Property (PaddingValue, backgroundColor, padding)
import Styled.Components as Styled

type State s =
  ( backgroundColor :: Maybe Color
  , padding :: Maybe PaddingValue
  | s
  )

wrapper
  :: forall s p i
   . { | State s }
  -> Array (HH.IProp _ i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
wrapper = Styled.element HH.section
  [ \state -> Array.catMaybes
      [ backgroundColor <$> state.backgroundColor
      , padding <$> state.padding
      ]
  ]

wrapper_ :: forall s p i. { | State s } -> Array (HH.HTML p i) -> HH.HTML p i
wrapper_ state = wrapper state []
