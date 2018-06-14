module Example.Button where

import Prelude

import Color (Color, rgb, white)
import Color.Scheme.HTML (blue)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Style.Declaration (FontSizeValue, TextAlignValue, backgroundColor, borderRadius, color, fontSize, padding, textAlign)
import Style.Declaration.Value (px)
import Styled.Components (element, id, modify_) as Styled
import Styled.Components.Effect (StyledM, deleteCSS)
import Styled.Components.Types (Element, Element_, ID(..)) as Styled

-- FIXME: button styling
buttonEl
  :: forall p i
   . Styled.ID
  -> StyledM (Styled.Element _ p i)
buttonEl id = Styled.element HH.button -- TODO: hover
  [ \_ ->
      [ backgroundColor $ rgb 0 103 238
      , borderRadius $ 4.0 # px
      , color white
      , fontSize $ 14.0 # px
      , padding $ 3.0 # px
      ]
  ]
  id
  {}

type State =
  { html :: H.ComponentHTML Query
  , id :: Styled.ID
  , isOn :: Boolean
  }

data Query a
  = Initialize a
  | Finalize a
  | Toggle a

type Input = Unit

data Message
  = Initialized
  | Finalized
  | Toggled Boolean

button :: H.Component HH.HTML Query Input Message StyledM
button =
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
    , isOn: false
    }

  render :: State -> StyledM (H.ComponentHTML Query)
  render state = do
    let label = if state.isOn then "On" else "Off"
    button' <- buttonEl state.id
    pure $
      button'
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message StyledM
  eval = case _ of
    Initialize next -> do
      id <- H.lift $ Styled.id
      Styled.modify_ render $ _ { id = id }
      H.raise Initialized -- tedious to do manually, transparent HOC will help, should we pass CSS up here instead of appendCSS/StyledM?
      pure next
    Finalize next -> do
      id <- H.gets _.id
      H.lift $ deleteCSS id
      H.raise Finalized -- should we just pass id here instead of deleteCSS?
      pure next
    Toggle next -> do
      state <- H.get
      let isOn = not state.isOn
      Styled.modify_ render $ _ { isOn = isOn }
      H.raise $ Toggled isOn
      pure next
