module MUI.Core.ButtonBase where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.ButtonBase.TouchRipple (TouchRippleProps) as MUI.Core.ButtonBase.TouchRipple
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_button) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Type_ :: Type

type_ :: { button :: Type_, reset :: Type_, submit :: Type_ }
type_ = { button: Unsafe.Coerce.unsafeCoerce "button", reset: Unsafe.Coerce.unsafeCoerce "reset", submit: Unsafe.Coerce.unsafeCoerce "submit" }

instance eqType :: Eq Type_ where
  eq = Unsafe.Reference.unsafeRefEq

type ButtonBaseActions  = Foreign.Foreign

type ButtonBaseTypeProp  = Foreign.Foreign

type ButtonBasePropsOptions componentProps = ( "TouchRippleProps" :: MUI.Core.ButtonBase.TouchRipple.TouchRippleProps, action :: Foreign.Foreign, buttonRef :: Foreign.Foreign, centerRipple :: Boolean, children :: Array React.Basic.JSX, classes :: ButtonBaseClassKey, color :: String, disableRipple :: Boolean, disabled :: Boolean, focusRipple :: Boolean, focusVisibleClassName :: String, onFocusVisible :: React.Basic.Events.EventHandler, "type" :: Type_ | componentProps )

foreign import data ButtonBaseProps :: Type

foreign import data ButtonBasePropsPartial :: Type

buttonBasePropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ButtonBasePropsOptions (ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record options -> ButtonBasePropsPartial
buttonBasePropsPartial = Unsafe.Coerce.unsafeCoerce

type ButtonBaseClassKeyGenericOptions a = ( disabled :: a, focusVisible :: a, root :: a )

type ButtonBaseClassKeyOptions  = ButtonBaseClassKeyGenericOptions String

foreign import data ButtonBaseClassKey :: Type

buttonBaseClassKey :: ∀ required given. Prim.Row.Union given required ButtonBaseClassKeyOptions => Record given -> ButtonBaseClassKey
buttonBaseClassKey = Unsafe.Coerce.unsafeCoerce

type ButtonBaseClassKeyOptionsJSS  = ButtonBaseClassKeyGenericOptions MUI.Core.JSS

foreign import data ButtonBaseClassKeyJSS :: Type

buttonBaseClassKeyJSS :: ∀ required given. Prim.Row.Union given required ButtonBaseClassKeyOptionsJSS => Record given -> ButtonBaseClassKeyJSS
buttonBaseClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _ButtonBase :: ∀ a. React.Basic.ReactComponent a

buttonBase :: ∀ required given. Prim.Row.Union given required (ButtonBasePropsOptions (ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record given -> React.Basic.JSX
buttonBase = React.Basic.element _ButtonBase

buttonBase_component :: ∀ required given componentProps. Prim.Row.Union given required (ButtonBasePropsOptions componentProps) => Record given -> React.Basic.JSX
buttonBase_component = React.Basic.element _ButtonBase

buttonBaseWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ButtonBasePropsOptions (ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Prim.Row.Union jss jss_ ButtonBaseClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
buttonBaseWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _ButtonBase)