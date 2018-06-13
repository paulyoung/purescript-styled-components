module Styled.Components
  ( css
  , element
  , id
  , modify_
  ) where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Int (hexadecimal, toStringAs)
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Style.Declaration (Declaration)
import Style.Ruleset (Ruleset(..))
import Style.Ruleset as Ruleset
import Style.Selector (Selector(..))
import Styled.Components.Effect (CSS, StyledM(..), appendCSS, cssValues)
import Styled.Components.Types (Element, ID(..))

element
  :: forall s r p i
   . Element r p i
  -> Array (s -> Array Declaration)
  -> ID
  -> s
  -> StyledM (Element r p i)
element el fns ident state = do
  -- FIXME: purescript-murmur3 when compatible with 0.12
  hashed <- unwrap <$> id

  let
    decls :: Array Declaration
    decls = Array.foldl (\styleProps fn -> styleProps <> fn state) [] fns

    className :: String
    className = "_" <> hashed

    prop :: HH.IProp (class :: String | r) i
    prop = HP.class_ $ H.ClassName className

    ruleset :: Ruleset
    ruleset = Ruleset [ClassSelector className] decls

  -- TODO: CSS statements
  appendCSS ident [ruleset]
  pure $ \props -> el $ props <> [prop]

css :: forall p i. StyledM (HH.HTML p i)
css = cssToHTML <$> cssValues

cssToHTML :: forall p i. CSS -> HH.HTML p i
cssToHTML = HH.style_ <<< map (HH.text <<< Ruleset.render)

id :: StyledM ID
id = do
  bytes <- liftEffect $ sequence $ Array.replicate 16 $ randomInt 16 255
  pure $ ID $ Array.intercalate "" $ toStringAs hexadecimal <$> bytes

modify_
  :: forall s f g p o
   . ({ html :: HH.HTML _ (f Unit) | s } -> StyledM (HH.HTML _ (f Unit)))
  -> ({ html :: HH.HTML _ (f Unit) | s } -> { html :: HH.HTML _ (f Unit) | s })
  -> (H.HalogenM { html :: HH.HTML _ (f Unit) | s } f g _ o StyledM) Unit
modify_ render f = do
  state <- H.get
  let newState = f state
  html <- H.lift $ render newState
  H.modify_ $ const $ newState { html = html }
