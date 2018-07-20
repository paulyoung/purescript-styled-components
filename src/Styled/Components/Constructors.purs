module Styled.Components.Constructors where

import Prelude

import Style.Declaration (Declaration)
import Style.Selector (PseudoClass(..), Selector(..))

type Constructors state =
  { selector :: Selector -> Selector
  , declarations :: state -> Array Declaration
  }

css :: forall s. (s -> Array Declaration) -> Constructors s
css = { selector: identity, declarations: _ }


pseudoClass :: forall s. PseudoClass -> (s -> Array Declaration) -> Constructors s
pseudoClass pc =
  { selector: flip PseudoClassSelector pc
  , declarations: _
  }

active :: forall s. (s -> Array Declaration) -> Constructors s
active = pseudoClass Active

disabled :: forall s. (s -> Array Declaration) -> Constructors s
disabled = pseudoClass Disabled

focus :: forall s. (s -> Array Declaration) -> Constructors s
focus = pseudoClass Focus

hover :: forall s. (s -> Array Declaration) -> Constructors s
hover = pseudoClass Hover
