module Styled.Components.Types where

import Prelude

import Data.Newtype (class Newtype)
import Halogen.HTML as HH

type Element r p i = Array (HH.IProp (class :: String | r) i) -> Element_ p i

type Element_ p i = Array (HH.HTML p i) -> HH.HTML p i

newtype ID = ID String

derive instance newtypeID :: Newtype ID _
derive newtype instance eqID :: Eq ID
derive newtype instance ordID :: Ord ID
