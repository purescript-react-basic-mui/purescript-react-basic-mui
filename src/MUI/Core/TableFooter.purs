module MUI.Core.TableFooter where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_tfoot) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type TableFooterPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: TableFooterClassKey | componentProps )

foreign import data TableFooterProps :: Type

foreign import data TableFooterPropsPartial :: Type

tableFooterPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (TableFooterPropsOptions React.Basic.DOM.Props_tfoot) => Record options -> TableFooterPropsPartial
tableFooterPropsPartial = Unsafe.Coerce.unsafeCoerce

type TableFooterClassKeyGenericOptions a = ( root :: a )

type TableFooterClassKeyOptions  = TableFooterClassKeyGenericOptions String

foreign import data TableFooterClassKey :: Type

tableFooterClassKey :: ∀ required given. Prim.Row.Union given required TableFooterClassKeyOptions => Record given -> TableFooterClassKey
tableFooterClassKey = Unsafe.Coerce.unsafeCoerce

type TableFooterClassKeyOptionsJSS  = TableFooterClassKeyGenericOptions MUI.Core.JSS

foreign import data TableFooterClassKeyJSS :: Type

tableFooterClassKeyJSS :: ∀ required given. Prim.Row.Union given required TableFooterClassKeyOptionsJSS => Record given -> TableFooterClassKeyJSS
tableFooterClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _TableFooter :: ∀ a. React.Basic.ReactComponent a

tableFooter :: ∀ required given. Prim.Row.Union given required (TableFooterPropsOptions React.Basic.DOM.Props_tfoot) => Record given -> React.Basic.JSX
tableFooter = React.Basic.element _TableFooter

tableFooter_component :: ∀ required given componentProps. Prim.Row.Union given required (TableFooterPropsOptions componentProps) => Record given -> React.Basic.JSX
tableFooter_component = React.Basic.element _TableFooter

tableFooterWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (TableFooterPropsOptions React.Basic.DOM.Props_tfoot) => Prim.Row.Union jss jss_ TableFooterClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
tableFooterWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _TableFooter)