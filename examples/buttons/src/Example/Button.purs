module Example.Button where

import Prelude

import Color (Color, rgb, white)
import Color.Scheme.HTML (blue)
import Data.Array as Array
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, over2, overF, overF2, under, unwrap, wrap)
import Data.Tuple (Tuple(..), uncurry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM
import Style.Declaration as CSS
import Style.Declaration.Value (bold, px)
import Style.Selector (PseudoClass(..), Selector(..))
import Styled.Components (element, id, modify_, modifyOver_) as Styled
import Styled.Components.Constructors (Constructors, active, css, disabled, focus, hover)
import Styled.Components.Effect (StyledM, deleteCSS)
import Styled.Components.Types (Element, Element_, ID(..)) as Styled

-- FIXME: button styling
buttonEl
  :: forall p i
   . State
  -> StyledM (Styled.Element _ p i)
buttonEl state@(State s) = el s.id state

  where

  -- el
  --   :: Styled.ID
  --   -> StyledM (Styled.Element _ p i)
  el = Styled.element HH.button $
    [ css \_ ->
        [ CSS.backgroundColor $ rgb 0 103 238
        , CSS.borderRadius $ 4.0 # px
        , CSS.color white
        , CSS.fontSize $ 14.0 # px
        , CSS.fontWeight bold
        , CSS.paddingTop $ 8.0 # px
        , CSS.paddingRight $ 16.0 # px
        , CSS.paddingBottom $ 8.0 # px
        , CSS.paddingLeft $ 16.0 # px
        ]
    , active \_ ->
        [
        ]
    , disabled \_ ->
        [
        ]
    , focus \_ ->
        [
        ]
    , hover \_ ->
        [ CSS.backgroundColor $ rgb 0 238 103 -- FIXME
        ]
    ] <> s.css

type StateFields =
  { css :: Array (Constructors State)
  , html :: H.ComponentHTML Query
  , id :: Styled.ID
  , isOn :: Boolean
  }

newtype State = State StateFields

derive instance newtypeState :: Newtype State _

data Query a
  = Initialize a
  | Finalize a
  | Toggle a

type Input =
  { css :: Array (Constructors State)
  }

data Message
  = Initialized
  | Finalized
  | Toggled Boolean

button :: H.Component HH.HTML Query Input Message StyledM
button =
  H.lifecycleComponent
    { initialState
    , render: _.html <<< unwrap
    , eval
    , initializer: Just $ H.action Initialize
    , finalizer: Just $ H.action Finalize
    , receiver: const Nothing
    }

  where

  initialState :: Input -> State
  initialState input =
    State
      { css: input.css
      , html: HH.text ""
      , id: Styled.ID ""
      , isOn: false
      }

  render :: State -> StyledM (H.ComponentHTML Query)
  render state@(State s) = do
    let label = if s.isOn then "On" else "Off"
    button' <- buttonEl state
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
      Styled.modifyOver_ State render _ { id = id }
      H.raise Initialized -- tedious to do manually, transparent HOC will help, should we pass CSS up here instead of appendCSS/StyledM?
      pure next
    Finalize next -> do
      id <- H.gets $ _.id <<< unwrap
      H.lift $ deleteCSS id
      H.raise Finalized -- should we just pass id here instead of deleteCSS?
      pure next
    Toggle next -> do
      (State state) <- H.get
      let isOn = not state.isOn
      Styled.modifyOver_ State render _ { isOn = isOn }
      H.raise $ Toggled isOn
      pure next
