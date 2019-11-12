module MUI.Core.LinearProgress where

import MUI.Core (JSS) as MUI.Core
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { buffer :: Variant, determinate :: Variant, indeterminate :: Variant, query :: Variant }
variant = { buffer: Unsafe.Coerce.unsafeCoerce "buffer", determinate: Unsafe.Coerce.unsafeCoerce "determinate", indeterminate: Unsafe.Coerce.unsafeCoerce "indeterminate", query: Unsafe.Coerce.unsafeCoerce "query" }

foreign import data Color :: Type

color :: { primary :: Color, secondary :: Color }
color = { primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type LinearProgressPropsOptions componentProps = ( classes :: LinearProgressClassKey, color :: Color, value :: Number, valueBuffer :: Number, variant :: Variant | componentProps )

foreign import data LinearProgressProps :: Type

foreign import data LinearProgressPropsPartial :: Type

type LinearProgressClassKeyGenericOptions a = ( bar :: a, bar1Buffer :: a, bar1Determinate :: a, bar1Indeterminate :: a, bar2Buffer :: a, bar2Indeterminate :: a, barColorPrimary :: a, barColorSecondary :: a, buffer :: a, colorPrimary :: a, colorSecondary :: a, dashed :: a, dashedColorPrimary :: a, dashedColorSecondary :: a, determinate :: a, indeterminate :: a, query :: a, root :: a )

type LinearProgressClassKeyOptions  = LinearProgressClassKeyGenericOptions String

foreign import data LinearProgressClassKey :: Type

linearProgressClassKey :: ∀ required given. Prim.Row.Union given required LinearProgressClassKeyOptions => Record given -> LinearProgressClassKey
linearProgressClassKey = Unsafe.Coerce.unsafeCoerce

type LinearProgressClassKeyOptionsJSS  = LinearProgressClassKeyGenericOptions MUI.Core.JSS

foreign import data LinearProgressClassKeyJSS :: Type

linearProgressClassKeyJSS :: ∀ required given. Prim.Row.Union given required LinearProgressClassKeyOptionsJSS => Record given -> LinearProgressClassKeyJSS
linearProgressClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _LinearProgress :: ∀ a. React.Basic.ReactComponent a

linearProgress :: ∀ required given. Prim.Row.Union given required (LinearProgressPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
linearProgress = React.Basic.element _LinearProgress

linearProgress_component :: ∀ required given componentProps. Prim.Row.Union given required (LinearProgressPropsOptions componentProps) => Record given -> React.Basic.JSX
linearProgress_component = React.Basic.element _LinearProgress