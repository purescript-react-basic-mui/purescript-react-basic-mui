module MUI.Core.CardHeader where

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

foreign import data CardHeaderPropsPartial :: Type

type CardHeaderClassKeyOptions =
  ( root :: String
  , avatar :: String
  , action :: String
  , content :: String
  , title :: String
  , subheader :: String
  )

cardHeaderClassKey :: ∀ options options_
  . Union options options_ CardHeaderClassKeyOptions
  => Record options
  -> CardHeaderClassKey
cardHeaderClassKey = unsafeCoerce

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