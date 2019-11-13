module MUI.Core.TableRow where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_tr) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type TableRowPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: TableRowClassKey, hover :: Boolean, selected :: Boolean | componentProps )

foreign import data TableRowProps :: Type

foreign import data TableRowPropsPartial :: Type

tableRowPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (TableRowPropsOptions React.Basic.DOM.Props_tr) => Record options -> TableRowPropsPartial
tableRowPropsPartial = Unsafe.Coerce.unsafeCoerce

type TableRowClassKeyGenericOptions a = ( footer :: a, head :: a, hover :: a, root :: a, selected :: a )

type TableRowClassKeyOptions  = TableRowClassKeyGenericOptions String

foreign import data TableRowClassKey :: Type

tableRowClassKey :: ∀ required given. Prim.Row.Union given required TableRowClassKeyOptions => Record given -> TableRowClassKey
tableRowClassKey = Unsafe.Coerce.unsafeCoerce

type TableRowClassKeyOptionsJSS  = TableRowClassKeyGenericOptions MUI.Core.JSS

foreign import data TableRowClassKeyJSS :: Type

tableRowClassKeyJSS :: ∀ required given. Prim.Row.Union given required TableRowClassKeyOptionsJSS => Record given -> TableRowClassKeyJSS
tableRowClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _TableRow :: ∀ a. React.Basic.ReactComponent a

tableRow :: ∀ required given. Prim.Row.Union given required (TableRowPropsOptions React.Basic.DOM.Props_tr) => Record given -> React.Basic.JSX
tableRow = React.Basic.element _TableRow

tableRow_component :: ∀ required given componentProps. Prim.Row.Union given required (TableRowPropsOptions componentProps) => Record given -> React.Basic.JSX
tableRow_component = React.Basic.element _TableRow

tableRowWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (TableRowPropsOptions React.Basic.DOM.Props_tr) => Prim.Row.Union jss jss_ TableRowClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
tableRowWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _TableRow)