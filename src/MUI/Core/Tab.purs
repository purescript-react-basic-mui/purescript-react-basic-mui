module MUI.Core.Tab where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.ButtonBase (ButtonBasePropsOptions) as MUI.Core.ButtonBase
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_button) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type TabPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: TabClassKey, disableFocusRipple :: Boolean, disableRipple :: Boolean, disabled :: Boolean, icon :: React.Basic.JSX, label :: React.Basic.JSX, value :: Foreign.Foreign, wrapped :: Boolean | componentProps )

foreign import data TabProps :: Type

foreign import data TabPropsPartial :: Type

tabPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (TabPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record options -> TabPropsPartial
tabPropsPartial = Unsafe.Coerce.unsafeCoerce

type TabClassKeyGenericOptions a = ( disabled :: a, fullWidth :: a, labelIcon :: a, root :: a, selected :: a, textColorInherit :: a, textColorPrimary :: a, textColorSecondary :: a, wrapped :: a, wrapper :: a )

type TabClassKeyOptions  = TabClassKeyGenericOptions String

foreign import data TabClassKey :: Type

tabClassKey :: ∀ required given. Prim.Row.Union given required TabClassKeyOptions => Record given -> TabClassKey
tabClassKey = Unsafe.Coerce.unsafeCoerce

type TabClassKeyOptionsJSS  = TabClassKeyGenericOptions MUI.Core.JSS

foreign import data TabClassKeyJSS :: Type

tabClassKeyJSS :: ∀ required given. Prim.Row.Union given required TabClassKeyOptionsJSS => Record given -> TabClassKeyJSS
tabClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Tab :: ∀ a. React.Basic.ReactComponent a

tab :: ∀ required given. Prim.Row.Union given required (TabPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record given -> React.Basic.JSX
tab = React.Basic.element _Tab

tab_component :: ∀ required given componentProps. Prim.Row.Union given required (TabPropsOptions componentProps) => Record given -> React.Basic.JSX
tab_component = React.Basic.element _Tab

tabWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (TabPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Prim.Row.Union jss jss_ TabClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
tabWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Tab)