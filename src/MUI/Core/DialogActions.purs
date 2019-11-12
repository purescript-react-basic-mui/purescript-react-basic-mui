module MUI.Core.DialogActions where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type DialogActionsPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: DialogActionsClassKey, disableSpacing :: Boolean | componentProps )

foreign import data DialogActionsProps :: Type

foreign import data DialogActionsPropsPartial :: Type

type DialogActionsClassKeyGenericOptions a = ( root :: a, spacing :: a )

type DialogActionsClassKeyOptions  = DialogActionsClassKeyGenericOptions String

foreign import data DialogActionsClassKey :: Type

dialogActionsClassKey :: ∀ required given. Prim.Row.Union given required DialogActionsClassKeyOptions => Record given -> DialogActionsClassKey
dialogActionsClassKey = Unsafe.Coerce.unsafeCoerce

type DialogActionsClassKeyOptionsJSS  = DialogActionsClassKeyGenericOptions MUI.Core.JSS

foreign import data DialogActionsClassKeyJSS :: Type

dialogActionsClassKeyJSS :: ∀ required given. Prim.Row.Union given required DialogActionsClassKeyOptionsJSS => Record given -> DialogActionsClassKeyJSS
dialogActionsClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _DialogActions :: ∀ a. React.Basic.ReactComponent a

dialogActions :: ∀ required given. Prim.Row.Union given required (DialogActionsPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
dialogActions = React.Basic.element _DialogActions

dialogActions_component :: ∀ required given componentProps. Prim.Row.Union given required (DialogActionsPropsOptions componentProps) => Record given -> React.Basic.JSX
dialogActions_component = React.Basic.element _DialogActions