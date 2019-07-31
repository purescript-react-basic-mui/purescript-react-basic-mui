module MUI.Core.ExpansionPanel where

import Effect.Uncurried (EffectFn2)
import MUI.Core (JSS)
import MUI.Core.Paper (PaperProps)
--import MUI.Core.Transition (TransitionProps)
import Prelude (Unit)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (SyntheticEvent)
import Unsafe.Coerce (unsafeCoerce)

type ExpansionPanelPropsRow componentProps = 
  ( children :: (Array JSX)
  , defaultExpanded :: Boolean
  , disabled :: Boolean
  , expanded :: Boolean
  , onChange :: EffectFn2 SyntheticEvent Boolean Unit
 -- , "TransitionComponent" :: ReactComponent { | TransitionPropsRow }
 -- , "TransitionProps" :: TransitionProps
  | componentProps
  )

foreign import data ExpansionPanelProps :: Type

type ExpansionPanelClassKeyRowG a =
  ( root :: a 
  , rounded :: a 
  , expanded :: a 
  , disabled :: a
  )
type ExpansionPanelClassKeyRow = ExpansionPanelClassKeyRowG String
type ExpansionPanelClassKeyRowJSS = ExpansionPanelClassKeyRowG JSS
foreign import data ExpansionPanelClassKey :: Type
foreign import data ExpansionPanelClassKeyJSS :: Type

expansionPanelClassKey :: ∀  props props_
  .  Union props props_ (ExpansionPanelClassKeyRow )
  => Record props
  -> ExpansionPanelClassKey
expansionPanelClassKey = unsafeCoerce

expansionPanelClassKeyJSS :: ∀  props props_
  .  Union props props_ (ExpansionPanelClassKeyRowJSS )
  => Record props
  -> ExpansionPanelClassKeyJSS
expansionPanelClassKeyJSS = unsafeCoerce

expansionPanel :: ∀  props props_
  .  Union props props_ (ExpansionPanelPropsRow (PaperProps Props_div) )
  => Record props
  -> JSX
expansionPanel = element _ExpansionPanel

expansionPanel_component :: ∀ componentProps props props_
  .  Union props props_ (ExpansionPanelPropsRow componentProps)
  => Record props
  -> JSX
expansionPanel_component = element _ExpansionPanel

foreign import _ExpansionPanel :: ∀ a. ReactComponent a