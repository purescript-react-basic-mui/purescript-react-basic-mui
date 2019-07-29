module MUI.Core.AppBar where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type AppBarProps componentProps =
  ( children :: Array JSX
  , classes :: AppBarClassKey
  , color :: ColorProp
  , position :: PositionProp
  , component :: ReactComponent { | componentProps }
  , elevation :: Number
  , square :: Boolean
  | componentProps
  )

foreign import data AppBarPropsPartial :: Type

foreign import data ColorProp :: Type
data Color = Inherit | Primary | Secondary | Default 
color :: Color -> ColorProp
color Inherit = unsafeCoerce "inherit"
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"
color Default = unsafeCoerce "default"

foreign import data PositionProp :: Type
data Position = Fixed | Absolute | Sticky | Static | Relative
position :: Position -> PositionProp
position Fixed = unsafeCoerce "fixed"
position Absolute = unsafeCoerce "absolute"
position Sticky = unsafeCoerce "sticky"
position Static = unsafeCoerce "static"
position Relative = unsafeCoerce "relative"


type AppBarClassKeyOptions = AppBarClassKeyOptionsR String
type AppBarClassKeyOptionsJSS = AppBarClassKeyOptionsR JSS
type AppBarClassKeyOptionsR a =
  ( positionFixed :: a
  , positionAbsolute :: a
  , positionSticky :: a
  , positionStatic :: a
  , positionRelative :: a
  , colorDefault :: a
  , colorPrimary :: a
  , colorSecondary :: a
  )

foreign import data AppBarClassKey :: Type
foreign import data AppBarClassKeyJSS :: Type

appBarClassKey :: ∀ options options_
  . Union options options_ AppBarClassKeyOptions
  => Record options
  -> AppBarClassKey
appBarClassKey = unsafeCoerce

appBarClassKeyJSS :: ∀ options options_
  . Union options options_ AppBarClassKeyOptionsJSS
  => Record options
  -> AppBarClassKeyJSS
appBarClassKeyJSS = unsafeCoerce


appBarPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (AppBarProps componentProps)
  => Record props 
  -> AppBarPropsPartial
appBarPropsPartial_component = unsafeCoerce

appBarPropsPartial :: ∀ props props_
  . Union props props_ (AppBarProps Props_div)
  => Record props 
  -> AppBarPropsPartial
appBarPropsPartial = unsafeCoerce

appBar_component :: ∀ componentProps props props_
  . Union props props_ (AppBarProps componentProps)
  => Record props 
  -> JSX
appBar_component = element _AppBar

appBar :: ∀ props props_
  . Union props props_ (AppBarProps Props_div)
  => Record props 
  -> JSX
appBar = element _AppBar



foreign import _AppBar :: ∀ a. ReactComponent a