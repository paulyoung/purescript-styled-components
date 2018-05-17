module Example where

import Prelude

import Color.Scheme.X11 (palevioletred, papayawhip)
import Data.Maybe (Maybe(..))
import Example.Title (title_)
import Example.Wrapper (wrapper_)
import Halogen as H
import Halogen.HTML as HH
import Style.Property.Value (center, em)

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

type Slot = Unit

example :: forall m. H.Component HH.HTML Query Input Message m
example =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = unit

  render :: State -> H.ComponentHTML Query
  render _ =
    wrapper_
      { backgroundColor: Just papayawhip
      , padding: Just $ 4.0 # em
      }
      [ title_
          { color: Just palevioletred
          , fontSize: Just $ 1.5 # em
          , textAlign: Just center
          }
          [ HH.text "Hello World, this is my first styled component!"
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (NoOp next) = pure next
