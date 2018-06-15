module Styled.Components
  ( css
  , element
  , id
  , modify_
  ) where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int (hexadecimal, toStringAs)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Murmur3 (hashString)
import Style.Declaration (Declaration(..))
import Style.Render (inline)
import Style.Ruleset (Ruleset(..))
import Style.Ruleset as Ruleset
import Style.Selector (Selector(..))
import Styled.Components.Constructors (Constructors)
import Styled.Components.Effect (CSS, StyledM, appendCSS, cssValues)
import Styled.Components.Types (Element, ID(..))

element
  :: forall s r p i
   . Element r p i
  -> Array (Constructors s)
  -> ID
  -> s
  -> StyledM (Element r p i)
element el constructors ident state = do
  let
    step { classNames, rulesets } construct =
      let
        decls :: Array Declaration
        decls = construct.declarations state

        hashed :: BigInt
        hashed = hashString (BigInt.fromInt 0) (inline decls)

        className :: String
        className = "_" <> BigInt.toBase 16 hashed

        selector :: Selector
        selector = construct.selector $ ClassSelector className
      in
        { classNames: classNames <> [ H.ClassName className ]
        , rulesets: rulesets <> [ Ruleset [ selector ] decls ]
        }

    new ::
      { classNames :: Array H.ClassName
      , rulesets :: Array Ruleset
      }
    new = Array.foldl step { classNames: [], rulesets: [] } constructors

  -- TODO: CSS statements
  appendCSS ident new.rulesets
  pure $ \props -> el $ props <> [ HP.classes $ Array.nub new.classNames ]

css :: forall p i. StyledM (HH.HTML p i)
css = cssToHTML <$> cssValues

cssToHTML :: forall p i. CSS -> HH.HTML p i
-- cssToHTML = HH.style_ <<< map (HH.text <<< Ruleset.render)
cssToHTML =
  HH.style_
    <<< Array.singleton
    <<< HH.text
    <<< Array.intercalate "\n\n"
    <<< Array.filter (_ /= "")
    <<< map Ruleset.render

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
