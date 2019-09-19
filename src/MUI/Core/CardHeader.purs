module MUI.Core.CardHeader where

import MUI.Core (JSS)
import MUI.Core.Typography (TypographyProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CardHeaderPropsOptions componentProps = 
  ( action :: JSX
  , avatar :: JSX
  , classes :: CardHeaderClassKey
  , component :: ReactComponent { | componentProps }
  , disableTypography :: Boolean
  , subheader :: JSX
  , subheaderTypographyProps :: TypographyProps
  , title :: JSX
  , titleTypographyProps :: TypographyProps
  | componentProps
  )

foreign import data CardHeaderProps :: Type

type CardHeaderClassKeyGenericOptions a =
  ( root :: a 
  , avatar :: a 
  , action :: a 
  , content :: a 
  , title :: a 
  , subheader :: a 
  )
type CardHeaderClassKeyOptions = CardHeaderClassKeyGenericOptions String
type CardHeaderClassKeyJSSOptions = CardHeaderClassKeyGenericOptions JSS
foreign import data CardHeaderClassKey :: Type
foreign import data CardHeaderClassKeyJSS :: Type

cardHeaderClassKey :: ∀  given required
  .  Union given required (CardHeaderClassKeyOptions )
  => Record given
  -> CardHeaderClassKey
cardHeaderClassKey = unsafeCoerce

cardHeaderClassKeyJSS :: ∀  given required
  .  Union given required (CardHeaderClassKeyJSSOptions )
  => Record given
  -> CardHeaderClassKeyJSS
cardHeaderClassKeyJSS = unsafeCoerce

cardHeader :: ∀  given required
  .  Union given required (CardHeaderPropsOptions Props_div )
  => Record given
  -> JSX
cardHeader = element _CardHeader

cardHeader_component :: ∀ componentProps given required
  .  Union given required (CardHeaderPropsOptions componentProps)
  => Record given
  -> JSX
cardHeader_component = element _CardHeader

foreign import _CardHeader :: ∀ a. ReactComponent a