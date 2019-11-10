module MUI.Core.DialogContent where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type DialogContentPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: DialogContentClassKey, dividers :: Boolean | componentProps )

foreign import data DialogContentProps :: Type

type DialogContentClassKeyGenericOptions a = ( root :: a )

type DialogContentClassKeyOptions  = DialogContentClassKeyGenericOptions String

foreign import data DialogContentClassKey :: Type

dialogContentClassKey :: ∀ required given. Prim.Row.Union given required DialogContentClassKeyOptions => Record given -> DialogContentClassKey
dialogContentClassKey = Unsafe.Coerce.unsafeCoerce

type DialogContentClassKeyOptionsJSS  = DialogContentClassKeyGenericOptions MUI.Core.JSS

foreign import data DialogContentClassKeyJSS :: Type

dialogContentClassKeyJSS :: ∀ required given. Prim.Row.Union given required DialogContentClassKeyOptionsJSS => Record given -> DialogContentClassKeyJSS
dialogContentClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _DialogContent :: ∀ a. React.Basic.ReactComponent a

dialogContent :: ∀ required given. Prim.Row.Union given required (DialogContentPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
dialogContent = React.Basic.element _DialogContent

dialogContent_component :: ∀ required given componentProps. Prim.Row.Union given required (DialogContentPropsOptions componentProps) => Record given -> React.Basic.JSX
dialogContent_component = React.Basic.element _DialogContent