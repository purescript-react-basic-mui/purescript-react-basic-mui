module MUI.Core.CardHeader where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type TypographyProps = String

type CardHeaderProps componentProps =
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

foreign import data CardHeaderClassKey :: Type
foreign import data CardHeaderClassKeyJSS :: Type

foreign import data CardHeaderPropsPartial :: Type

type CardHeaderClassKeyOptionsJSS = CardHeaderClassKeyOptionsR JSS
type CardHeaderClassKeyOptions = CardHeaderClassKeyOptionsR String
type CardHeaderClassKeyOptionsR a =
  ( root :: a
  , avatar :: a
  , action :: a
  , content :: a
  , title :: a
  , subheader :: a
  )

cardHeaderClassKey :: ∀ options options_
  . Union options options_ CardHeaderClassKeyOptions
  => Record options
  -> CardHeaderClassKey
cardHeaderClassKey = unsafeCoerce

cardHeaderClassKeyJSS :: ∀ options options_
  . Union options options_ CardHeaderClassKeyOptionsJSS
  => Record options
  -> CardHeaderClassKeyJSS
cardHeaderClassKeyJSS = unsafeCoerce

cardHeaderPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (CardHeaderProps componentProps)
  => Record props 
  -> CardHeaderPropsPartial 
cardHeaderPropsPartial_component = unsafeCoerce

cardHeaderPropsPartial :: ∀ props props_
  . Union props props_ (CardHeaderProps Props_div)
  => Record props 
  -> CardHeaderPropsPartial 
cardHeaderPropsPartial = unsafeCoerce

cardHeader_component :: ∀ componentProps props props_
  . Union props props_ (CardHeaderProps componentProps)
  => Record props 
  -> JSX
cardHeader_component = element _CardHeader

cardHeader :: ∀ props props_
  . Union props props_ (CardHeaderProps Props_div)
  => Record props 
  -> JSX
cardHeader = element _CardHeader


foreign import _CardHeader :: ∀ a. ReactComponent a