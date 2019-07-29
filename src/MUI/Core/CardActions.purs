module MUI.Core.CardActions where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CardActionsProps componentProps =
  ( children :: Array JSX
  , classes :: CardActionsClassKey
  , disableSpacing :: Boolean
  | componentProps
  )

foreign import data CardActionsClassKey :: Type
foreign import data CardActionsClassKeyJSS :: Type
foreign import data CardActionsPropsPartial :: Type

type CardActionsClassKeyOptionsJSS  = CardActionsClassKeyOptionsR JSS
type CardActionsClassKeyOptions  =  CardActionsClassKeyOptionsR String 
type CardActionsClassKeyOptionsR a = 
  ( root :: a 
  , spacing :: a
  )

cardActionsClassKey :: ∀ options options_
  . Union options options_ CardActionsClassKeyOptions
  => Record options
  -> CardActionsClassKey
cardActionsClassKey = unsafeCoerce

cardActionsClassKeyJSS :: ∀ options options_
  . Union options options_ CardActionsClassKeyOptionsJSS
  => Record options
  -> CardActionsClassKeyJSS
cardActionsClassKeyJSS = unsafeCoerce

cardActionsPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (CardActionsProps componentProps)
  => Record props 
  -> CardActionsPropsPartial
cardActionsPropsPartial_component = unsafeCoerce

cardActionsPropsPartial :: ∀ props props_
  . Union props props_ (CardActionsProps Props_div)
  => Record props 
  -> CardActionsPropsPartial
cardActionsPropsPartial = unsafeCoerce

cardActions_component :: ∀ componentProps props props_
  . Union props props_ (CardActionsProps componentProps)
  => Record props 
  -> JSX
cardActions_component = element _CardActions

cardActions :: ∀ props props_
  . Union props props_ (CardActionsProps Props_div)
  => Record props 
  -> JSX
cardActions = element _CardActions


foreign import _CardActions :: ∀ a. ReactComponent a