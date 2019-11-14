module MUI.Core.Select where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.Input (InputProps, InputPropsOptions) as MUI.Core.Input
import MUI.Core.Menu (MenuProps) as MUI.Core.Menu
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
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

type SelectPropsOptions componentProps = ( "IconComponent" :: React.Basic.JSX, "MenuProps" :: MUI.Core.Menu.MenuProps, "SelectDisplayProps" :: Foreign.Foreign, autoWidth :: Boolean, children :: Array React.Basic.JSX, classes :: SelectClassKey, displayEmpty :: Boolean, input :: React.Basic.JSX, inputProps :: MUI.Core.Input.InputProps, labelWidth :: Number, multiple :: Boolean, native :: Boolean, onChange :: React.Basic.Events.EventHandler, onClose :: React.Basic.Events.EventHandler, onOpen :: React.Basic.Events.EventHandler, open :: Boolean, renderValue :: {  }, value :: Foreign.Foreign, variant :: Variant | componentProps )

foreign import data SelectProps :: Type

foreign import data SelectPropsPartial :: Type

selectPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (SelectPropsOptions (MUI.Core.Input.InputPropsOptions React.Basic.DOM.Props_div)) => Record options -> SelectPropsPartial
selectPropsPartial = Unsafe.Coerce.unsafeCoerce

type SelectClassKeyGenericOptions a = ( disabled :: a, filled :: a, icon :: a, iconFilled :: a, iconOpen :: a, iconOutlined :: a, outlined :: a, root :: a, select :: a, selectMenu :: a )

type SelectClassKeyOptions  = SelectClassKeyGenericOptions String

foreign import data SelectClassKey :: Type

selectClassKey :: ∀ required given. Prim.Row.Union given required SelectClassKeyOptions => Record given -> SelectClassKey
selectClassKey = Unsafe.Coerce.unsafeCoerce

type SelectClassKeyOptionsJSS  = SelectClassKeyGenericOptions MUI.Core.JSS

foreign import data SelectClassKeyJSS :: Type

selectClassKeyJSS :: ∀ required given. Prim.Row.Union given required SelectClassKeyOptionsJSS => Record given -> SelectClassKeyJSS
selectClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Select :: ∀ a. React.Basic.ReactComponent a

select :: ∀ required given. Prim.Row.Union given required (SelectPropsOptions (MUI.Core.Input.InputPropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
select = React.Basic.element _Select

select_component :: ∀ required given componentProps. Prim.Row.Union given required (SelectPropsOptions componentProps) => Record given -> React.Basic.JSX
select_component = React.Basic.element _Select

selectWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (SelectPropsOptions (MUI.Core.Input.InputPropsOptions React.Basic.DOM.Props_div)) => Prim.Row.Union jss jss_ SelectClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
selectWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Select)
