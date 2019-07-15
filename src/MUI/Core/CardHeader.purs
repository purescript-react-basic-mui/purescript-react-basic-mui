module MUI.Core.CardHeader where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type TypographyProps = String

type CardHeaderProps =
  ( action :: JSX
  , avatar :: JSX
  , classes :: CardHeaderClassKey 
  , component :: String
  , disableTypography :: Boolean
  , subheader :: JSX
  , subheaderTypographyProps :: TypographyProps
  , title :: JSX
  , titleTypographyProps :: TypographyProps
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

cardHeaderPropsPartial :: ∀ props props_
  . Union props props_ CardHeaderProps
  => Record props 
  -> CardHeaderPropsPartial 
cardHeaderPropsPartial = unsafeCoerce

cardHeader :: ∀ props props_
  . Union props props_ CardHeaderProps
  => Record props 
  -> JSX
cardHeader = element _CardHeader

foreign import _CardHeader :: ∀ a. ReactComponent a