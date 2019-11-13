module MUI.Core.TableSortLabel where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.ButtonBase (ButtonBasePropsOptions) as MUI.Core.ButtonBase
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_button) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Direction :: Type

direction :: { asc :: Direction, desc :: Direction }
direction = { asc: Unsafe.Coerce.unsafeCoerce "asc", desc: Unsafe.Coerce.unsafeCoerce "desc" }

instance eqDirection :: Eq Direction where
  eq = Unsafe.Reference.unsafeRefEq

type TableSortLabelPropsOptions componentProps = ( "IconComponent" :: Foreign.Foreign, active :: Boolean, children :: Array React.Basic.JSX, classes :: TableSortLabelClassKey, direction :: Direction, hideSortIcon :: Boolean | componentProps )

foreign import data TableSortLabelProps :: Type

foreign import data TableSortLabelPropsPartial :: Type

tableSortLabelPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (TableSortLabelPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record options -> TableSortLabelPropsPartial
tableSortLabelPropsPartial = Unsafe.Coerce.unsafeCoerce

type TableSortLabelClassKeyGenericOptions a = ( active :: a, icon :: a, iconDirectionAsc :: a, iconDirectionDesc :: a, root :: a )

type TableSortLabelClassKeyOptions  = TableSortLabelClassKeyGenericOptions String

foreign import data TableSortLabelClassKey :: Type

tableSortLabelClassKey :: ∀ required given. Prim.Row.Union given required TableSortLabelClassKeyOptions => Record given -> TableSortLabelClassKey
tableSortLabelClassKey = Unsafe.Coerce.unsafeCoerce

type TableSortLabelClassKeyOptionsJSS  = TableSortLabelClassKeyGenericOptions MUI.Core.JSS

foreign import data TableSortLabelClassKeyJSS :: Type

tableSortLabelClassKeyJSS :: ∀ required given. Prim.Row.Union given required TableSortLabelClassKeyOptionsJSS => Record given -> TableSortLabelClassKeyJSS
tableSortLabelClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _TableSortLabel :: ∀ a. React.Basic.ReactComponent a

tableSortLabel :: ∀ required given. Prim.Row.Union given required (TableSortLabelPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record given -> React.Basic.JSX
tableSortLabel = React.Basic.element _TableSortLabel

tableSortLabel_component :: ∀ required given componentProps. Prim.Row.Union given required (TableSortLabelPropsOptions componentProps) => Record given -> React.Basic.JSX
tableSortLabel_component = React.Basic.element _TableSortLabel

tableSortLabelWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (TableSortLabelPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Prim.Row.Union jss jss_ TableSortLabelClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
tableSortLabelWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _TableSortLabel)