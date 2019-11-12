module MUI.Core.GridList where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_ul) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

foreign import data CellHeight :: Type

cellHeight :: { auto :: CellHeight, number :: Number -> CellHeight }
cellHeight = { auto: Unsafe.Coerce.unsafeCoerce "auto", number: Unsafe.Coerce.unsafeCoerce }

type GridListPropsOptions componentProps = ( cellHeight :: CellHeight, children :: Array React.Basic.JSX, classes :: GridListClassKey, cols :: Number, component :: React.Basic.ReactComponent {  | componentProps }, spacing :: Number | componentProps )

foreign import data GridListProps :: Type

foreign import data GridListPropsPartial :: Type

gridListPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (GridListPropsOptions React.Basic.DOM.Props_ul) => Record options -> GridListPropsPartial
gridListPropsPartial = Unsafe.Coerce.unsafeCoerce

type GridListClassKeyGenericOptions a = ( root :: a )

type GridListClassKeyOptions  = GridListClassKeyGenericOptions String

foreign import data GridListClassKey :: Type

gridListClassKey :: ∀ required given. Prim.Row.Union given required GridListClassKeyOptions => Record given -> GridListClassKey
gridListClassKey = Unsafe.Coerce.unsafeCoerce

type GridListClassKeyOptionsJSS  = GridListClassKeyGenericOptions MUI.Core.JSS

foreign import data GridListClassKeyJSS :: Type

gridListClassKeyJSS :: ∀ required given. Prim.Row.Union given required GridListClassKeyOptionsJSS => Record given -> GridListClassKeyJSS
gridListClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _GridList :: ∀ a. React.Basic.ReactComponent a

gridList :: ∀ required given. Prim.Row.Union given required (GridListPropsOptions React.Basic.DOM.Props_ul) => Record given -> React.Basic.JSX
gridList = React.Basic.element _GridList

gridList_component :: ∀ required given componentProps. Prim.Row.Union given required (GridListPropsOptions componentProps) => Record given -> React.Basic.JSX
gridList_component = React.Basic.element _GridList

gridListWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (GridListPropsOptions React.Basic.DOM.Props_ul) => Prim.Row.Union jss jss_ GridListClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
gridListWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _GridList)