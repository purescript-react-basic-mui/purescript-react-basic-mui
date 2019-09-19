module MUI.Core.CircularProgress where

import Prelude

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CircularProgressPropsOptions componentProps = 
  ( classes :: CircularProgressClassKey
  , color :: ColorProp
  , disableShrink :: Boolean
  , size :: Number
  , thickness :: Number
  , value :: Number
  , variant :: VariantProp
  | componentProps
  )

foreign import data CircularProgressProps :: Type

foreign import data ColorProp :: Type
foreign import _eqColorProp :: ColorProp -> ColorProp -> Boolean
foreign import _ordColorProp :: ColorProp -> ColorProp -> Int
instance eqColorProp :: Eq ColorProp where eq _left _right = _eqColorProp _left _right
instance ordColorProp :: Ord ColorProp where compare _left _right = compare (_ordColorProp _left _right) (_ordColorProp _right _left)

primary :: ColorProp
primary = unsafeCoerce "primary"

secondary :: ColorProp
secondary = unsafeCoerce "secondary"

inherit :: ColorProp
inherit = unsafeCoerce "inherit"
foreign import data VariantProp :: Type
foreign import _eqVariantProp :: VariantProp -> VariantProp -> Boolean
foreign import _ordVariantProp :: VariantProp -> VariantProp -> Int
instance eqVariantProp :: Eq VariantProp where eq _left _right = _eqVariantProp _left _right
instance ordVariantProp :: Ord VariantProp where compare _left _right = compare (_ordVariantProp _left _right) (_ordVariantProp _right _left)

determinate :: VariantProp
determinate = unsafeCoerce "determinate"

indeterminate :: VariantProp
indeterminate = unsafeCoerce "indeterminate"

static :: VariantProp
static = unsafeCoerce "static"

type CircularProgressClassKeyGenericOptions a =
  ( root :: a 
  , static :: a 
  , indeterminate :: a 
  , colorPrimary :: a 
  , colorSecondary :: a 
  , svg :: a 
  , circle :: a 
  , circleStatic :: a 
  , circleIndeterminate :: a 
  , circleDisableShrink :: a 
  )
type CircularProgressClassKeyOptions = CircularProgressClassKeyGenericOptions String
type CircularProgressClassKeyJSSOptions = CircularProgressClassKeyGenericOptions JSS
foreign import data CircularProgressClassKey :: Type
foreign import data CircularProgressClassKeyJSS :: Type

circularProgressClassKey :: ∀  given required
  .  Union given required (CircularProgressClassKeyOptions )
  => Record given
  -> CircularProgressClassKey
circularProgressClassKey = unsafeCoerce

circularProgressClassKeyJSS :: ∀  given required
  .  Union given required (CircularProgressClassKeyJSSOptions )
  => Record given
  -> CircularProgressClassKeyJSS
circularProgressClassKeyJSS = unsafeCoerce

circularProgress :: ∀  given required
  .  Union given required (CircularProgressPropsOptions Props_div )
  => Record given
  -> JSX
circularProgress = element _CircularProgress

circularProgress_component :: ∀ componentProps given required
  .  Union given required (CircularProgressPropsOptions componentProps)
  => Record given
  -> JSX
circularProgress_component = element _CircularProgress

foreign import _CircularProgress :: ∀ a. ReactComponent a