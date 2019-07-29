module MUI.Core.CardContent where


import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CardContentProps componentProps =
  ( children :: Array JSX
  , classes :: CardContentClassKey 
  , component :: ReactComponent { | componentProps }
  | componentProps
  )

foreign import data CardContentPropsPartial :: Type

foreign import data CardContentClassKey :: Type
foreign import data CardContentClassKeyJSS :: Type

type CardContentClassKeyOptionsJSS = CardContentClassKeyOptionsR JSS
type CardContentClassKeyOptions = CardContentClassKeyOptionsR String
type CardContentClassKeyOptionsR a = ( root :: a )

cardContentClassKey :: ∀ options options_
  . Union options options_ CardContentClassKeyOptions
  => Record options
  -> CardContentClassKey
cardContentClassKey = unsafeCoerce

cardContentClassKeyJSS :: ∀ options options_
  . Union options options_ CardContentClassKeyOptionsJSS
  => Record options
  -> CardContentClassKeyJSS
cardContentClassKeyJSS = unsafeCoerce

cardContentPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (CardContentProps componentProps)
  => Record props 
  -> CardContentPropsPartial 
cardContentPropsPartial_component = unsafeCoerce

cardContentPropsPartial :: ∀ props props_
  . Union props props_ (CardContentProps Props_div)
  => Record props 
  -> CardContentPropsPartial 
cardContentPropsPartial = unsafeCoerce

cardContent_component :: ∀ componentProps props props_
  . Union props props_ (CardContentProps componentProps)
  => Record props 
  -> JSX
cardContent_component = element _CardContent

cardContent :: ∀ props props_
  . Union props props_ (CardContentProps Props_div)
  => Record props 
  -> JSX
cardContent = element _CardContent



foreign import _CardContent :: ∀ a. ReactComponent a