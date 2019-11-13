module MUI.Core.TableHead where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_thead) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type TableHeadPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: TableHeadClassKey | componentProps )

foreign import data TableHeadProps :: Type

foreign import data TableHeadPropsPartial :: Type

tableHeadPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (TableHeadPropsOptions React.Basic.DOM.Props_thead) => Record options -> TableHeadPropsPartial
tableHeadPropsPartial = Unsafe.Coerce.unsafeCoerce

type TableHeadClassKeyGenericOptions a = ( root :: a )

type TableHeadClassKeyOptions  = TableHeadClassKeyGenericOptions String

foreign import data TableHeadClassKey :: Type

tableHeadClassKey :: ∀ required given. Prim.Row.Union given required TableHeadClassKeyOptions => Record given -> TableHeadClassKey
tableHeadClassKey = Unsafe.Coerce.unsafeCoerce

type TableHeadClassKeyOptionsJSS  = TableHeadClassKeyGenericOptions MUI.Core.JSS

foreign import data TableHeadClassKeyJSS :: Type

tableHeadClassKeyJSS :: ∀ required given. Prim.Row.Union given required TableHeadClassKeyOptionsJSS => Record given -> TableHeadClassKeyJSS
tableHeadClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _TableHead :: ∀ a. React.Basic.ReactComponent a

tableHead :: ∀ required given. Prim.Row.Union given required (TableHeadPropsOptions React.Basic.DOM.Props_thead) => Record given -> React.Basic.JSX
tableHead = React.Basic.element _TableHead

tableHead_component :: ∀ required given componentProps. Prim.Row.Union given required (TableHeadPropsOptions componentProps) => Record given -> React.Basic.JSX
tableHead_component = React.Basic.element _TableHead

tableHeadWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (TableHeadPropsOptions React.Basic.DOM.Props_thead) => Prim.Row.Union jss jss_ TableHeadClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
tableHeadWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _TableHead)