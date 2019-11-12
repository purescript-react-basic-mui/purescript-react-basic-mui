module MUI.Core.NativeSelect where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.Input (InputProps, InputPropsOptions) as MUI.Core.Input
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { filled :: Variant, outlined :: Variant, standard :: Variant }
variant = { filled: Unsafe.Coerce.unsafeCoerce "filled", outlined: Unsafe.Coerce.unsafeCoerce "outlined", standard: Unsafe.Coerce.unsafeCoerce "standard" }

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type NativeSelectPropsOptions componentProps = ( "IconComponent" :: React.Basic.JSX, children :: Array React.Basic.JSX, classes :: NativeSelectClassKey, input :: React.Basic.JSX, inputProps :: MUI.Core.Input.InputProps, onChange :: React.Basic.Events.EventHandler, value :: Foreign.Foreign, variant :: Variant | componentProps )

foreign import data NativeSelectProps :: Type

foreign import data NativeSelectPropsPartial :: Type

nativeSelectPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (NativeSelectPropsOptions (MUI.Core.Input.InputPropsOptions React.Basic.DOM.Props_div)) => Record options -> NativeSelectPropsPartial
nativeSelectPropsPartial = Unsafe.Coerce.unsafeCoerce

type NativeSelectClassKeyGenericOptions a = ( disabled :: a, filled :: a, icon :: a, iconFilled :: a, iconOutlined :: a, outlined :: a, root :: a, select :: a, selectMenu :: a )

type NativeSelectClassKeyOptions  = NativeSelectClassKeyGenericOptions String

foreign import data NativeSelectClassKey :: Type

nativeSelectClassKey :: ∀ required given. Prim.Row.Union given required NativeSelectClassKeyOptions => Record given -> NativeSelectClassKey
nativeSelectClassKey = Unsafe.Coerce.unsafeCoerce

type NativeSelectClassKeyOptionsJSS  = NativeSelectClassKeyGenericOptions MUI.Core.JSS

foreign import data NativeSelectClassKeyJSS :: Type

nativeSelectClassKeyJSS :: ∀ required given. Prim.Row.Union given required NativeSelectClassKeyOptionsJSS => Record given -> NativeSelectClassKeyJSS
nativeSelectClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _NativeSelect :: ∀ a. React.Basic.ReactComponent a

nativeSelect :: ∀ required given. Prim.Row.Union given required (NativeSelectPropsOptions (MUI.Core.Input.InputPropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
nativeSelect = React.Basic.element _NativeSelect

nativeSelect_component :: ∀ required given componentProps. Prim.Row.Union given required (NativeSelectPropsOptions componentProps) => Record given -> React.Basic.JSX
nativeSelect_component = React.Basic.element _NativeSelect