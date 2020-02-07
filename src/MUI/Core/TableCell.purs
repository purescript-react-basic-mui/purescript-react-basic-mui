module MUI.Core.TableCell where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_td) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { body :: Variant, footer :: Variant, head :: Variant }
variant = { body: Unsafe.Coerce.unsafeCoerce "body", footer: Unsafe.Coerce.unsafeCoerce "footer", head: Unsafe.Coerce.unsafeCoerce "head" }

foreign import data Size :: Type

size :: { medium :: Size, small :: Size }
size = { medium: Unsafe.Coerce.unsafeCoerce "medium", small: Unsafe.Coerce.unsafeCoerce "small" }

foreign import data Padding :: Type

padding :: { checkbox :: Padding, default :: Padding, none :: Padding }
padding = { checkbox: Unsafe.Coerce.unsafeCoerce "checkbox", default: Unsafe.Coerce.unsafeCoerce "default", none: Unsafe.Coerce.unsafeCoerce "none" }

foreign import data Align :: Type

align :: { center :: Align, inherit :: Align, justify :: Align, left :: Align, right :: Align }
align = { center: Unsafe.Coerce.unsafeCoerce "center", inherit: Unsafe.Coerce.unsafeCoerce "inherit", justify: Unsafe.Coerce.unsafeCoerce "justify", left: Unsafe.Coerce.unsafeCoerce "left", right: Unsafe.Coerce.unsafeCoerce "right" }

instance eqAlign :: Eq Align where
  eq = Unsafe.Reference.unsafeRefEq

instance eqPadding :: Eq Padding where
  eq = Unsafe.Reference.unsafeRefEq

instance eqSize :: Eq Size where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type TableCellPropsOptions componentProps = ( align :: Align, children :: Array React.Basic.JSX, classes :: TableCellClassKey, padding :: Padding, scope :: String, size :: Size, variant :: Variant | componentProps )

foreign import data TableCellProps :: Type

foreign import data TableCellPropsPartial :: Type

tableCellPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (TableCellPropsOptions React.Basic.DOM.Props_td) => Record options -> TableCellPropsPartial
tableCellPropsPartial = Unsafe.Coerce.unsafeCoerce

type TableCellClassKeyGenericOptions a = ( alignCenter :: a, alignJustify :: a, alignLeft :: a, alignRight :: a, body :: a, footer :: a, head :: a, paddingCheckbox :: a, paddingNone :: a, root :: a, sizeSmall :: a, stickyHeader :: a )

type TableCellClassKeyOptions  = TableCellClassKeyGenericOptions String

foreign import data TableCellClassKey :: Type

tableCellClassKey :: ∀ required given. Prim.Row.Union given required TableCellClassKeyOptions => Record given -> TableCellClassKey
tableCellClassKey = Unsafe.Coerce.unsafeCoerce

type TableCellClassKeyOptionsJSS  = TableCellClassKeyGenericOptions MUI.Core.JSS

foreign import data TableCellClassKeyJSS :: Type

tableCellClassKeyJSS :: ∀ required given. Prim.Row.Union given required TableCellClassKeyOptionsJSS => Record given -> TableCellClassKeyJSS
tableCellClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _TableCell :: ∀ a. React.Basic.ReactComponent a

tableCell :: ∀ required given. Prim.Row.Union given required (TableCellPropsOptions React.Basic.DOM.Props_td) => Record given -> React.Basic.JSX
tableCell = React.Basic.element _TableCell

tableCell_component :: ∀ required given componentProps. Prim.Row.Union given required (TableCellPropsOptions componentProps) => Record given -> React.Basic.JSX
tableCell_component = React.Basic.element _TableCell

tableCellWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (TableCellPropsOptions React.Basic.DOM.Props_td) => Prim.Row.Union jss jss_ TableCellClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
tableCellWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _TableCell)