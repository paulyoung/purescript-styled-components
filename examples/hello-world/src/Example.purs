module Example where

import Prelude

import Color.Scheme.X11 (palevioletred, papayawhip)
import Data.Maybe (Maybe(..))
import Example.Title (title_)
import Example.Wrapper (wrapper_)
import Halogen as H
import Halogen.HTML as HH
import Style.Declaration.Value (center, em)
import Styled.Components (css, id) as Styled
import Styled.Components.Effect (StyledM, deleteCSS)
import Styled.Components.Types (ID(..)) as Styled

type State =
  { html :: H.ComponentHTML Query
  , id :: Styled.ID
  }

data Query a
  = Initialize a
  | Finalize a

type Input = Unit

type Message = Void

example :: H.Component HH.HTML Query Input Message StyledM
example =
  H.lifecycleComponent
    { initialState: const initialState
    , render: _.html
    , eval
    , initializer: Just $ H.action Initialize
    , finalizer: Just $ H.action Finalize
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { html: HH.text ""
    , id: Styled.ID ""
    }

  render :: State -> StyledM (H.ComponentHTML Query)
  render state = do
    title' <- title_ state.id
      { color: Just palevioletred
      , fontSize: Just $ 1.5 # em
      , textAlign: Just center
      }

    wrapper' <- wrapper_ state.id
      { backgroundColor: Just papayawhip
      , padding: Just $ 4.0 # em
      }

    styleTag <- Styled.css

    pure $
      HH.div_
        [ styleTag
        , wrapper'
            [ title'
                [ HH.text "Hello World, this is my first styled component!" ]
            ]
        ]

  eval :: Query ~> H.ComponentDSL State Query Message StyledM
  eval = case _ of
    Initialize next -> do
      state <- H.get
      id <- H.lift $ Styled.id
      html <- H.lift $ render $ state { id = id }
      H.modify_ _ { html = html, id = id }
      pure next
    Finalize next -> do
      id <- H.gets _.id
      H.lift $ deleteCSS id
      pure next
