module Styled.Components
  ( element
  ) where

import Prelude

import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Style.Declaration (Declaration)
import Style.Render (inline)

element
  :: forall s r p i
   . (Array (HH.IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i)
  -> Array (s -> Array Declaration)
  -> s
  -> Array (HH.IProp r i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
element el fns state props = el (props <> [ attr ])
  where

  styles :: Array Declaration
  styles = Array.foldl (\styleProps fn -> styleProps <> fn state) [] fns

  attr :: HH.IProp r i
  attr = HP.attr (HC.AttrName "style") $ inline styles
