module MUI.Core.CardContent where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CardContentPropsOptions componentProps = 
  ( classes :: CardContentClassKey
  , component :: ReactComponent { | componentProps }
  | componentProps
  )

foreign import data CardContentProps :: Type

type CardContentClassKeyGenericOptions a =
  ( root :: a 
  )
type CardContentClassKeyOptions = CardContentClassKeyGenericOptions String
type CardContentClassKeyJSSOptions = CardContentClassKeyGenericOptions JSS
foreign import data CardContentClassKey :: Type
foreign import data CardContentClassKeyJSS :: Type

cardContentClassKey :: ∀  given required
  .  Union given required (CardContentClassKeyOptions )
  => Record given
  -> CardContentClassKey
cardContentClassKey = unsafeCoerce

cardContentClassKeyJSS :: ∀  given required
  .  Union given required (CardContentClassKeyJSSOptions )
  => Record given
  -> CardContentClassKeyJSS
cardContentClassKeyJSS = unsafeCoerce

cardContent :: ∀  given required
  .  Union given required (CardContentPropsOptions Props_div )
  => Record given
  -> JSX
cardContent = element _CardContent

cardContent_component :: ∀ componentProps given required
  .  Union given required (CardContentPropsOptions componentProps)
  => Record given
  -> JSX
cardContent_component = element _CardContent

foreign import _CardContent :: ∀ a. ReactComponent a