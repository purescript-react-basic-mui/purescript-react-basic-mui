module MUI.Core.AppBar where

import Prelude

import MUI.Core (JSS)
import MUI.Core.Paper (PaperProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type AppBarPropsOptions componentProps = 
  ( children :: (Array JSX)
  , classes :: AppBarClassKey
  , color :: ColorProp
  , position :: PositionProp
  | componentProps
  )

foreign import data AppBarProps :: Type

foreign import data ColorProp :: Type
foreign import _eqColorProp :: ColorProp -> ColorProp -> Boolean
foreign import _ordColorProp :: ColorProp -> ColorProp -> Int
instance eqColorProp :: Eq ColorProp where eq left right = _eqColorProp left right
instance ordColorProp :: Ord ColorProp where compare left right = compare (_ordColorProp left right) (_ordColorProp right left)

inherit :: ColorProp
inherit = unsafeCoerce "inherit"

primary :: ColorProp
primary = unsafeCoerce "primary"

secondary :: ColorProp
secondary = unsafeCoerce "secondary"

default :: ColorProp
default = unsafeCoerce "default"
foreign import data PositionProp :: Type
foreign import _eqPositionProp :: PositionProp -> PositionProp -> Boolean
foreign import _ordPositionProp :: PositionProp -> PositionProp -> Int
instance eqPositionProp :: Eq PositionProp where eq left right = _eqPositionProp left right
instance ordPositionProp :: Ord PositionProp where compare left right = compare (_ordPositionProp left right) (_ordPositionProp right left)

fixed :: PositionProp
fixed = unsafeCoerce "fixed"

absolute :: PositionProp
absolute = unsafeCoerce "absolute"

sticky :: PositionProp
sticky = unsafeCoerce "sticky"

static :: PositionProp
static = unsafeCoerce "static"

relative :: PositionProp
relative = unsafeCoerce "relative"

type AppBarClassKeyGenericOptions a =
  ( root :: a 
  , positionFixed :: a 
  , positionAbsolute :: a 
  , positionSticky :: a 
  , positionStatic :: a 
  , positionRelative :: a 
  , colorDefault :: a 
  , colorPrimary :: a 
  , colorSecondary :: a 
  )
type AppBarClassKeyOptions = AppBarClassKeyGenericOptions String
type AppBarClassKeyJSSOptions = AppBarClassKeyGenericOptions JSS
foreign import data AppBarClassKey :: Type
foreign import data AppBarClassKeyJSS :: Type

appBarClassKey :: ∀  given required
  .  Union given required (AppBarClassKeyOptions )
  => Record given
  -> AppBarClassKey
appBarClassKey = unsafeCoerce

appBarClassKeyJSS :: ∀  given required
  .  Union given required (AppBarClassKeyJSSOptions )
  => Record given
  -> AppBarClassKeyJSS
appBarClassKeyJSS = unsafeCoerce

appBar :: ∀  given required
  .  Union given required (AppBarPropsOptions (PaperProps Props_div) )
  => Record given
  -> JSX
appBar = element _AppBar

appBar_component :: ∀ componentProps given required
  .  Union given required (AppBarPropsOptions componentProps)
  => Record given
  -> JSX
appBar_component = element _AppBar

foreign import _AppBar :: ∀ a. ReactComponent a