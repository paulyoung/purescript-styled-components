module Styled.Components
  ( css
  , element
  , id
  , modify_
  , modifyOver_
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (class Newtype, under, unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM
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
    step st construct = fromMaybe st do
      decls <- NEA.fromArray $ construct.declarations state

      let
        inlined :: String
        inlined = inline decls

        hashed :: BigInt
        hashed = hashString zero inlined

        className :: String
        className = "_" <> BigInt.toBase 16 hashed

        selector :: Selector
        selector = construct.selector $ ClassSelector className

      selectors <- NEA.fromArray [ selector ]

      pure
        { classNames: st.classNames <> [ H.ClassName className ]
        , rulesets: st.rulesets <> [ Ruleset selectors decls ]
        }

    new ::
      { classNames :: Array H.ClassName
      , rulesets :: Array Ruleset
      }
    new = Array.foldl step { classNames: [], rulesets: [] } constructors

  -- TODO: CSS statement
  appendCSS ident new.rulesets
  pure $ \props -> el $ props <> [ HP.classes $ Array.nub new.classNames ]

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

modifyOver_
  :: forall state s f g p o
   . Newtype state { html :: HH.HTML _ (f Unit) | s }
  => ({ html :: HH.HTML _ (f Unit) | s } -> state)
  -> (state -> StyledM (HH.HTML _ (f Unit)))
  -> ({ html :: HH.HTML _ (f Unit) | s } -> { html :: HH.HTML _ (f Unit) | s })
  -> H.HalogenM state f g _ o StyledM Unit
modifyOver_ t render' =
  HM.imapState wrap unwrap
    <<< modify_ (wrap <<< under t render')
