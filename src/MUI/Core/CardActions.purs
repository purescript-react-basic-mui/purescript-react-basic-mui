module MUI.Core.CardActions where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CardActionsPropsOptions componentProps = 
  ( children :: (Array JSX)
  , classes :: CardActionsClassKey
  , disableSpacing :: Boolean
  | componentProps
  )

foreign import data CardActionsProps :: Type

type CardActionsClassKeyGenericOptions a =
  ( root :: a 
  , spacing :: a 
  )
type CardActionsClassKeyOptions = CardActionsClassKeyGenericOptions String
type CardActionsClassKeyJSSOptions = CardActionsClassKeyGenericOptions JSS
foreign import data CardActionsClassKey :: Type
foreign import data CardActionsClassKeyJSS :: Type

cardActionsClassKey :: ∀  given required
  .  Union given required (CardActionsClassKeyOptions )
  => Record given
  -> CardActionsClassKey
cardActionsClassKey = unsafeCoerce

cardActionsClassKeyJSS :: ∀  given required
  .  Union given required (CardActionsClassKeyJSSOptions )
  => Record given
  -> CardActionsClassKeyJSS
cardActionsClassKeyJSS = unsafeCoerce

cardActions :: ∀  given required
  .  Union given required (CardActionsPropsOptions Props_div )
  => Record given
  -> JSX
cardActions = element _CardActions

cardActions_component :: ∀ componentProps given required
  .  Union given required (CardActionsPropsOptions componentProps)
  => Record given
  -> JSX
cardActions_component = element _CardActions

foreign import _CardActions :: ∀ a. ReactComponent a