module MUI.Core.CircularProgress where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { determinate :: Variant, indeterminate :: Variant, static :: Variant }
variant = { determinate: Unsafe.Coerce.unsafeCoerce "determinate", indeterminate: Unsafe.Coerce.unsafeCoerce "indeterminate", static: Unsafe.Coerce.unsafeCoerce "static" }

foreign import data Size :: Type

size :: { number :: Number -> Size, string :: String -> Size }
size = { number: Unsafe.Coerce.unsafeCoerce, string: Unsafe.Coerce.unsafeCoerce }

foreign import data Color :: Type

color :: { inherit :: Color, primary :: Color, secondary :: Color }
color = { inherit: Unsafe.Coerce.unsafeCoerce "inherit", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type CircularProgressPropsOptions componentProps = ( classes :: CircularProgressClassKey, color :: Color, disableShrink :: Boolean, size :: Size, thickness :: Number, value :: Number, variant :: Variant | componentProps )

foreign import data CircularProgressProps :: Type

foreign import data CircularProgressPropsPartial :: Type

circularProgressPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (CircularProgressPropsOptions React.Basic.DOM.Props_div) => Record options -> CircularProgressPropsPartial
circularProgressPropsPartial = Unsafe.Coerce.unsafeCoerce

type CircularProgressClassKeyGenericOptions a = ( circle :: a, circleDisableShrink :: a, circleIndeterminate :: a, circleStatic :: a, colorPrimary :: a, colorSecondary :: a, indeterminate :: a, root :: a, static :: a, svg :: a )

type CircularProgressClassKeyOptions  = CircularProgressClassKeyGenericOptions String

foreign import data CircularProgressClassKey :: Type

circularProgressClassKey :: ∀ required given. Prim.Row.Union given required CircularProgressClassKeyOptions => Record given -> CircularProgressClassKey
circularProgressClassKey = Unsafe.Coerce.unsafeCoerce

type CircularProgressClassKeyOptionsJSS  = CircularProgressClassKeyGenericOptions MUI.Core.JSS

foreign import data CircularProgressClassKeyJSS :: Type

circularProgressClassKeyJSS :: ∀ required given. Prim.Row.Union given required CircularProgressClassKeyOptionsJSS => Record given -> CircularProgressClassKeyJSS
circularProgressClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _CircularProgress :: ∀ a. React.Basic.ReactComponent a

circularProgress :: ∀ required given. Prim.Row.Union given required (CircularProgressPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
circularProgress = React.Basic.element _CircularProgress

circularProgress_component :: ∀ required given componentProps. Prim.Row.Union given required (CircularProgressPropsOptions componentProps) => Record given -> React.Basic.JSX
circularProgress_component = React.Basic.element _CircularProgress

circularProgressWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (CircularProgressPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ CircularProgressClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
circularProgressWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _CircularProgress)