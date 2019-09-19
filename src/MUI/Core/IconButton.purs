module MUI.Core.IconButton where

import Prelude

import MUI.Core (JSS)
import MUI.Core.ButtonBase (ButtonBasePropsOptions)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import Unsafe.Coerce (unsafeCoerce)

type IconButtonPropsOptions componentProps = 
  ( children :: (Array JSX)
  , classes :: IconButtonClassKey
  , color :: ColorProp
  , disabled :: Boolean
  , disableFocusRipple :: Boolean
  , disableRipple :: Boolean
  , edge :: EdgeProp
  , size :: SizeProp
  | componentProps
  )

foreign import data IconButtonProps :: Type

type IconButtonPropsPartial = {}

foreign import data ColorProp :: Type
foreign import _eqColorProp :: ColorProp -> ColorProp -> Boolean
foreign import _ordColorProp :: ColorProp -> ColorProp -> Int
instance eqColorProp :: Eq ColorProp where eq _left _right = _eqColorProp _left _right
instance ordColorProp :: Ord ColorProp where compare _left _right = compare (_ordColorProp _left _right) (_ordColorProp _right _left)

primary :: ColorProp
primary = unsafeCoerce "primary"

secondary :: ColorProp
secondary = unsafeCoerce "secondary"

default :: ColorProp
default = unsafeCoerce "default"

inherit :: ColorProp
inherit = unsafeCoerce "inherit"
foreign import data EdgeProp :: Type
foreign import _eqEdgeProp :: EdgeProp -> EdgeProp -> Boolean
foreign import _ordEdgeProp :: EdgeProp -> EdgeProp -> Int
instance eqEdgeProp :: Eq EdgeProp where eq _left _right = _eqEdgeProp _left _right
instance ordEdgeProp :: Ord EdgeProp where compare _left _right = compare (_ordEdgeProp _left _right) (_ordEdgeProp _right _left)

start :: EdgeProp
start = unsafeCoerce "start"

end :: EdgeProp
end = unsafeCoerce "end"

boolean :: Boolean -> EdgeProp
boolean value = unsafeCoerce value
foreign import data SizeProp :: Type
foreign import _eqSizeProp :: SizeProp -> SizeProp -> Boolean
foreign import _ordSizeProp :: SizeProp -> SizeProp -> Int
instance eqSizeProp :: Eq SizeProp where eq _left _right = _eqSizeProp _left _right
instance ordSizeProp :: Ord SizeProp where compare _left _right = compare (_ordSizeProp _left _right) (_ordSizeProp _right _left)

small :: SizeProp
small = unsafeCoerce "small"

medium :: SizeProp
medium = unsafeCoerce "medium"

type IconButtonClassKeyGenericOptions a =
  ( root :: a 
  , edgeStart :: a 
  , edgeEnd :: a 
  , colorInherit :: a 
  , colorPrimary :: a 
  , colorSecondary :: a 
  , disabled :: a 
  , sizeSmall :: a 
  , label :: a 
  )
type IconButtonClassKeyOptions = IconButtonClassKeyGenericOptions String
type IconButtonClassKeyJSSOptions = IconButtonClassKeyGenericOptions JSS
foreign import data IconButtonClassKey :: Type
foreign import data IconButtonClassKeyJSS :: Type

iconButtonClassKey :: ∀  given required
  .  Union given required (IconButtonClassKeyOptions )
  => Record given
  -> IconButtonClassKey
iconButtonClassKey = unsafeCoerce

iconButtonClassKeyJSS :: ∀  given required
  .  Union given required (IconButtonClassKeyJSSOptions )
  => Record given
  -> IconButtonClassKeyJSS
iconButtonClassKeyJSS = unsafeCoerce

iconButton :: ∀  given required
  .  Union given required (IconButtonPropsOptions (ButtonBasePropsOptions Props_button) )
  => Record given
  -> JSX
iconButton = element _IconButton

iconButton_component :: ∀ componentProps given required
  .  Union given required (IconButtonPropsOptions componentProps)
  => Record given
  -> JSX
iconButton_component = element _IconButton

foreign import _IconButton :: ∀ a. ReactComponent a