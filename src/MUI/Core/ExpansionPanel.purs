module MUI.MUI.Core.ExpansionPanel where

import Prelude

import Effect.Uncurried (EffectFn2)
import MUI.Core (JSS)
import MUI.Core.Paper (PaperProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (SyntheticEvent)
import Unsafe.Coerce (unsafeCoerce)

type ExpansionPanelPropsOptions componentProps = 
  ( children :: (Array JSX)
  , defaultExpanded :: Boolean
  , disabled :: Boolean
  , expanded :: Boolean
  , onChange :: EffectFn2 SyntheticEvent Boolean Unit
  | componentProps
  )

foreign import data ExpansionPanelProps :: Type



type ExpansionPanelClassKeyGenericOptions a =
  ( root :: a 
  , rounded :: a 
  , expanded :: a 
  , disabled :: a 
  )
type ExpansionPanelClassKeyOptions = ExpansionPanelClassKeyGenericOptions String
type ExpansionPanelClassKeyJSSOptions = ExpansionPanelClassKeyGenericOptions JSS
foreign import data ExpansionPanelClassKey :: Type
foreign import data ExpansionPanelClassKeyJSS :: Type

expansionPanelClassKey :: ∀  given required
  .  Union given required (ExpansionPanelClassKeyOptions )
  => Record given
  -> ExpansionPanelClassKey
expansionPanelClassKey = unsafeCoerce

expansionPanelClassKeyJSS :: ∀  given required
  .  Union given required (ExpansionPanelClassKeyJSSOptions )
  => Record given
  -> ExpansionPanelClassKeyJSS
expansionPanelClassKeyJSS = unsafeCoerce

expansionPanel :: ∀  given required
  .  Union given required (ExpansionPanelPropsOptions (PaperProps Props_div) )
  => Record given
  -> JSX
expansionPanel = element _ExpansionPanel

expansionPanel_component :: ∀ componentProps given required
  .  Union given required (ExpansionPanelPropsOptions componentProps)
  => Record given
  -> JSX
expansionPanel_component = element _ExpansionPanel

foreign import _ExpansionPanel :: ∀ a. ReactComponent a