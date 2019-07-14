module MUI.Core.Icon where


import Data.Maybe (Maybe)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type IconProps =
  ( children :: Maybe (Array JSX)
  , classes :: IconClassKey
  , color :: Maybe String
  , component :: Maybe String
  , fontSize :: Maybe String
  )

type IconClassKeyOptions =
  ( root :: Maybe String
  , colorPrimary :: Maybe String
  , colorSecondary :: Maybe String
  , colorAction :: Maybe String
  , colorError :: Maybe String
  , colorDisabled :: Maybe String
  , fontSizeInherit :: Maybe String
  , fontSizeSmall :: Maybe String
  , fontSizeLarge :: Maybe String
  )

foreign import data IconClassKey :: Type

iconClassKey 
  :: ∀ options options_
  . Union options options_ IconClassKeyOptions
  => Record options
  -> IconClassKey 
iconClassKey = unsafeCoerce

icon
  :: ∀ props props_
  . Union props props_ IconProps
  => Record props 
  -> JSX
icon = element _Icon


foreign import _Icon :: ∀ a. ReactComponent a