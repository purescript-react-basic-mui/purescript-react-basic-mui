module MUI.Core.ListSubheader where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_li) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Color :: Type

color :: { default :: Color, inherit :: Color, primary :: Color }
color = { default: Unsafe.Coerce.unsafeCoerce "default", inherit: Unsafe.Coerce.unsafeCoerce "inherit", primary: Unsafe.Coerce.unsafeCoerce "primary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

type ListSubheaderPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: ListSubheaderClassKey, color :: Color, disableGutters :: Boolean, disableSticky :: Boolean, inset :: Boolean | componentProps )

foreign import data ListSubheaderProps :: Type

foreign import data ListSubheaderPropsPartial :: Type

listSubheaderPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ListSubheaderPropsOptions React.Basic.DOM.Props_li) => Record options -> ListSubheaderPropsPartial
listSubheaderPropsPartial = Unsafe.Coerce.unsafeCoerce

type ListSubheaderClassKeyGenericOptions a = ( colorInherit :: a, colorPrimary :: a, gutters :: a, inset :: a, root :: a, sticky :: a )

type ListSubheaderClassKeyOptions  = ListSubheaderClassKeyGenericOptions String

foreign import data ListSubheaderClassKey :: Type

listSubheaderClassKey :: ∀ required given. Prim.Row.Union given required ListSubheaderClassKeyOptions => Record given -> ListSubheaderClassKey
listSubheaderClassKey = Unsafe.Coerce.unsafeCoerce

type ListSubheaderClassKeyOptionsJSS  = ListSubheaderClassKeyGenericOptions MUI.Core.JSS

foreign import data ListSubheaderClassKeyJSS :: Type

listSubheaderClassKeyJSS :: ∀ required given. Prim.Row.Union given required ListSubheaderClassKeyOptionsJSS => Record given -> ListSubheaderClassKeyJSS
listSubheaderClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _ListSubheader :: ∀ a. React.Basic.ReactComponent a

listSubheader :: ∀ required given. Prim.Row.Union given required (ListSubheaderPropsOptions React.Basic.DOM.Props_li) => Record given -> React.Basic.JSX
listSubheader = React.Basic.element _ListSubheader

listSubheader_component :: ∀ required given componentProps. Prim.Row.Union given required (ListSubheaderPropsOptions componentProps) => Record given -> React.Basic.JSX
listSubheader_component = React.Basic.element _ListSubheader

listSubheaderWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ListSubheaderPropsOptions React.Basic.DOM.Props_li) => Prim.Row.Union jss jss_ ListSubheaderClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
listSubheaderWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _ListSubheader)