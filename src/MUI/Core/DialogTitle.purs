module MUI.Core.DialogTitle where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type DialogTitlePropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: DialogTitleClassKey, disableTypography :: Boolean | componentProps )

foreign import data DialogTitleProps :: Type

foreign import data DialogTitlePropsPartial :: Type

dialogTitlePropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (DialogTitlePropsOptions React.Basic.DOM.Props_div) => Record options -> DialogTitlePropsPartial
dialogTitlePropsPartial = Unsafe.Coerce.unsafeCoerce

type DialogTitleClassKeyGenericOptions a = ( root :: a )

type DialogTitleClassKeyOptions  = DialogTitleClassKeyGenericOptions String

foreign import data DialogTitleClassKey :: Type

dialogTitleClassKey :: ∀ required given. Prim.Row.Union given required DialogTitleClassKeyOptions => Record given -> DialogTitleClassKey
dialogTitleClassKey = Unsafe.Coerce.unsafeCoerce

type DialogTitleClassKeyOptionsJSS  = DialogTitleClassKeyGenericOptions MUI.Core.JSS

foreign import data DialogTitleClassKeyJSS :: Type

dialogTitleClassKeyJSS :: ∀ required given. Prim.Row.Union given required DialogTitleClassKeyOptionsJSS => Record given -> DialogTitleClassKeyJSS
dialogTitleClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _DialogTitle :: ∀ a. React.Basic.ReactComponent a

dialogTitle :: ∀ required given. Prim.Row.Union given required (DialogTitlePropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
dialogTitle = React.Basic.element _DialogTitle

dialogTitle_component :: ∀ required given componentProps. Prim.Row.Union given required (DialogTitlePropsOptions componentProps) => Record given -> React.Basic.JSX
dialogTitle_component = React.Basic.element _DialogTitle