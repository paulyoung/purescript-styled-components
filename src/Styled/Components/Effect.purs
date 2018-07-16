module Styled.Components.Effect where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, modify_, read)
import Style.Ruleset (Ruleset)
import Styled.Components.Types (ID)

type CSS = Array Ruleset

type StyledEnv = { css :: Ref (Map ID CSS) }

newtype StyledM a = StyledM (ReaderT StyledEnv Aff a)

derive instance newtypeStyledM :: Newtype (StyledM a) _

derive newtype instance functorStyledM :: Functor StyledM
derive newtype instance applyStyledM :: Apply StyledM
derive newtype instance applicativeStyledM :: Applicative StyledM
derive newtype instance bindStyledM :: Bind StyledM
derive newtype instance monadStyledM :: Monad StyledM
derive newtype instance monadEffectStyledM :: MonadEffect StyledM

runStyledM :: StyledEnv -> StyledM ~> Aff
runStyledM env (StyledM app) = runReaderT app env

deleteCSS :: ID -> StyledM Unit
deleteCSS id = StyledM $ ReaderT \r ->
  liftEffect $ modify_ (Map.delete id) r.css

appendCSS :: ID -> CSS -> StyledM Unit
appendCSS id css = StyledM $ ReaderT \r ->
  liftEffect $ modify_ (Map.alter alter id) r.css

  where

  alter :: Maybe CSS -> Maybe CSS
  alter = case _ of
    Just prevCss -> Just $ prevCss <> css
    Nothing -> Just css

cssValues :: StyledM CSS
cssValues = StyledM $ ReaderT \r ->
  liftEffect $ map values $ read $ r.css

  where

  values :: Map ID CSS -> CSS
  values = Array.nub <<< Array.concat <<< Array.fromFoldable <<< Map.values
