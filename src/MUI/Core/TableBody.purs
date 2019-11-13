module MUI.Core.TableBody where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_tbody) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type TableBodyPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: TableBodyClassKey | componentProps )

foreign import data TableBodyProps :: Type

foreign import data TableBodyPropsPartial :: Type

tableBodyPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (TableBodyPropsOptions React.Basic.DOM.Props_tbody) => Record options -> TableBodyPropsPartial
tableBodyPropsPartial = Unsafe.Coerce.unsafeCoerce

type TableBodyClassKeyGenericOptions a = ( root :: a )

type TableBodyClassKeyOptions  = TableBodyClassKeyGenericOptions String

foreign import data TableBodyClassKey :: Type

tableBodyClassKey :: ∀ required given. Prim.Row.Union given required TableBodyClassKeyOptions => Record given -> TableBodyClassKey
tableBodyClassKey = Unsafe.Coerce.unsafeCoerce

type TableBodyClassKeyOptionsJSS  = TableBodyClassKeyGenericOptions MUI.Core.JSS

foreign import data TableBodyClassKeyJSS :: Type

tableBodyClassKeyJSS :: ∀ required given. Prim.Row.Union given required TableBodyClassKeyOptionsJSS => Record given -> TableBodyClassKeyJSS
tableBodyClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _TableBody :: ∀ a. React.Basic.ReactComponent a

tableBody :: ∀ required given. Prim.Row.Union given required (TableBodyPropsOptions React.Basic.DOM.Props_tbody) => Record given -> React.Basic.JSX
tableBody = React.Basic.element _TableBody

tableBody_component :: ∀ required given componentProps. Prim.Row.Union given required (TableBodyPropsOptions componentProps) => Record given -> React.Basic.JSX
tableBody_component = React.Basic.element _TableBody

tableBodyWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (TableBodyPropsOptions React.Basic.DOM.Props_tbody) => Prim.Row.Union jss jss_ TableBodyClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
tableBodyWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _TableBody)