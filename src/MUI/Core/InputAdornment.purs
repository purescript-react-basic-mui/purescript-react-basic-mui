module MUI.Core.InputAdornment where

import MUI.Core (JSS) as MUI.Core
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { filled :: Variant, outlined :: Variant, standard :: Variant }
variant = { filled: Unsafe.Coerce.unsafeCoerce "filled", outlined: Unsafe.Coerce.unsafeCoerce "outlined", standard: Unsafe.Coerce.unsafeCoerce "standard" }

foreign import data Position :: Type

position :: { end :: Position, start :: Position }
position = { end: Unsafe.Coerce.unsafeCoerce "end", start: Unsafe.Coerce.unsafeCoerce "start" }

instance eqPosition :: Eq Position where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type InputAdornmentPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: InputAdornmentClassKey, component :: React.Basic.ReactComponent {  | componentProps }, disablePointerEvents :: Boolean, disableTypography :: Boolean, position :: Position, variant :: Variant | componentProps )

foreign import data InputAdornmentProps :: Type

foreign import data InputAdornmentPropsPartial :: Type

inputAdornmentPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (InputAdornmentPropsOptions React.Basic.DOM.Props_div) => Record options -> InputAdornmentPropsPartial
inputAdornmentPropsPartial = Unsafe.Coerce.unsafeCoerce

type InputAdornmentClassKeyGenericOptions a = ( disablePointerEvents :: a, filled :: a, hiddenLabel :: a, marginDense :: a, positionEnd :: a, positionStart :: a, root :: a )

type InputAdornmentClassKeyOptions  = InputAdornmentClassKeyGenericOptions String

foreign import data InputAdornmentClassKey :: Type

inputAdornmentClassKey :: ∀ required given. Prim.Row.Union given required InputAdornmentClassKeyOptions => Record given -> InputAdornmentClassKey
inputAdornmentClassKey = Unsafe.Coerce.unsafeCoerce

type InputAdornmentClassKeyOptionsJSS  = InputAdornmentClassKeyGenericOptions MUI.Core.JSS

foreign import data InputAdornmentClassKeyJSS :: Type

inputAdornmentClassKeyJSS :: ∀ required given. Prim.Row.Union given required InputAdornmentClassKeyOptionsJSS => Record given -> InputAdornmentClassKeyJSS
inputAdornmentClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _InputAdornment :: ∀ a. React.Basic.ReactComponent a

inputAdornment :: ∀ required given. Prim.Row.Union given required (InputAdornmentPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
inputAdornment = React.Basic.element _InputAdornment

inputAdornment_component :: ∀ required given componentProps. Prim.Row.Union given required (InputAdornmentPropsOptions componentProps) => Record given -> React.Basic.JSX
inputAdornment_component = React.Basic.element _InputAdornment