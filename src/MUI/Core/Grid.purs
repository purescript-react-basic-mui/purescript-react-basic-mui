module MUI.Core.Grid where

import Literals.Boolean (False, True) as Literals.Boolean
import Literals.Number (Literal) as Literals.Number
import Literals.String (Literal) as Literals.String
import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import OneOf (OneOf) as OneOf
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type GridPropsOptions componentProps =
  (alignContent :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (Literals.String.Literal "stretch") (Literals.String.Literal "space-around")) (Literals.String.Literal "space-between")) (Literals.String.Literal "flex-end")) (Literals.String.Literal "flex-start")) (Literals.String.Literal "center"),
  alignItems :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (Literals.String.Literal "stretch") (Literals.String.Literal "baseline")) (Literals.String.Literal "flex-end")) (Literals.String.Literal "flex-start")) (Literals.String.Literal "center"), children :: Array React.Basic.JSX, classes :: GridClassKey, container :: Boolean, direction :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (Literals.String.Literal "row") (Literals.String.Literal "column-reverse")) (Literals.String.Literal "column")) (Literals.String.Literal "row-reverse"), item :: Boolean, justify :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (Literals.String.Literal "center") (Literals.String.Literal "space-evenly")) (Literals.String.Literal "space-around")) (Literals.String.Literal "space-between")) (Literals.String.Literal "flex-end")) (Literals.String.Literal "flex-start"), lg :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf Literals.Boolean.False (Literals.Number.Literal "12.0")) (Literals.Number.Literal "11.0")) (Literals.Number.Literal "10.0")) (Literals.Number.Literal "9.0")) (Literals.Number.Literal "8.0")) (Literals.Number.Literal "7.0")) (Literals.Number.Literal "6.0")) (Literals.Number.Literal "5.0")) (Literals.Number.Literal "4.0")) (Literals.Number.Literal "3.0")) (Literals.Number.Literal "2.0")) (Literals.Number.Literal "1.0")) (Literals.String.Literal "auto")) Literals.Boolean.True, md :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf Literals.Boolean.False (Literals.Number.Literal "12.0")) (Literals.Number.Literal "11.0")) (Literals.Number.Literal "10.0")) (Literals.Number.Literal "9.0")) (Literals.Number.Literal "8.0")) (Literals.Number.Literal "7.0")) (Literals.Number.Literal "6.0")) (Literals.Number.Literal "5.0")) (Literals.Number.Literal "4.0")) (Literals.Number.Literal "3.0")) (Literals.Number.Literal "2.0")) (Literals.Number.Literal "1.0")) (Literals.String.Literal "auto")) Literals.Boolean.True, sm :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf Literals.Boolean.False (Literals.Number.Literal "12.0")) (Literals.Number.Literal "11.0")) (Literals.Number.Literal "10.0")) (Literals.Number.Literal "9.0")) (Literals.Number.Literal "8.0")) (Literals.Number.Literal "7.0")) (Literals.Number.Literal "6.0")) (Literals.Number.Literal "5.0")) (Literals.Number.Literal "4.0")) (Literals.Number.Literal "3.0")) (Literals.Number.Literal "2.0")) (Literals.Number.Literal "1.0")) (Literals.String.Literal "auto")) Literals.Boolean.True, spacing :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (Literals.Number.Literal "0.0") (Literals.Number.Literal "10.0")) (Literals.Number.Literal "9.0")) (Literals.Number.Literal "8.0")) (Literals.Number.Literal "7.0")) (Literals.Number.Literal "6.0")) (Literals.Number.Literal "5.0")) (Literals.Number.Literal "4.0")) (Literals.Number.Literal "3.0")) (Literals.Number.Literal "2.0")) (Literals.Number.Literal "1.0"), wrap :: OneOf.OneOf (OneOf.OneOf (Literals.String.Literal "nowrap") (Literals.String.Literal "wrap-reverse")) (Literals.String.Literal "wrap"), xl :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf Literals.Boolean.False (Literals.Number.Literal "12.0")) (Literals.Number.Literal "11.0")) (Literals.Number.Literal "10.0")) (Literals.Number.Literal "9.0")) (Literals.Number.Literal "8.0")) (Literals.Number.Literal "7.0")) (Literals.Number.Literal "6.0")) (Literals.Number.Literal "5.0")) (Literals.Number.Literal "4.0")) (Literals.Number.Literal "3.0")) (Literals.Number.Literal "2.0")) (Literals.Number.Literal "1.0")) (Literals.String.Literal "auto")) Literals.Boolean.True, xs :: OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf (OneOf.OneOf Literals.Boolean.False (Literals.Number.Literal "12.0")) (Literals.Number.Literal "11.0")) (Literals.Number.Literal "10.0")) (Literals.Number.Literal "9.0")) (Literals.Number.Literal "8.0")) (Literals.Number.Literal "7.0")) (Literals.Number.Literal "6.0")) (Literals.Number.Literal "5.0")) (Literals.Number.Literal "4.0")) (Literals.Number.Literal "3.0")) (Literals.Number.Literal "2.0")) (Literals.Number.Literal "1.0")) (Literals.String.Literal "auto")) Literals.Boolean.True, zeroMinWidth :: Boolean | componentProps )

foreign import data GridProps :: Type

foreign import data GridPropsPartial :: Type

gridPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (GridPropsOptions React.Basic.DOM.Props_div) => Record options -> GridPropsPartial
gridPropsPartial = Unsafe.Coerce.unsafeCoerce

type GridClassKeyGenericOptions a = ( "align-content-xs-center" :: a, "align-content-xs-flex-end" :: a, "align-content-xs-flex-start" :: a, "align-content-xs-space-around" :: a, "align-content-xs-space-between" :: a, "align-items-xs-baseline" :: a, "align-items-xs-center" :: a, "align-items-xs-flex-end" :: a, "align-items-xs-flex-start" :: a, container :: a, "direction-xs-column" :: a, "direction-xs-column-reverse" :: a, "direction-xs-row-reverse" :: a, "grid-xs-1" :: a, "grid-xs-10" :: a, "grid-xs-11" :: a, "grid-xs-12" :: a, "grid-xs-2" :: a, "grid-xs-3" :: a, "grid-xs-4" :: a, "grid-xs-5" :: a, "grid-xs-6" :: a, "grid-xs-7" :: a, "grid-xs-8" :: a, "grid-xs-9" :: a, "grid-xs-auto" :: a, "grid-xs-true" :: a, item :: a, "justify-xs-center" :: a, "justify-xs-flex-end" :: a, "justify-xs-space-around" :: a, "justify-xs-space-between" :: a, root :: a, "spacing-xs-1" :: a, "spacing-xs-10" :: a, "spacing-xs-2" :: a, "spacing-xs-3" :: a, "spacing-xs-4" :: a, "spacing-xs-5" :: a, "spacing-xs-6" :: a, "spacing-xs-7" :: a, "spacing-xs-8" :: a, "spacing-xs-9" :: a, "wrap-xs-nowrap" :: a, "wrap-xs-wrap-reverse" :: a )

type GridClassKeyOptions  = GridClassKeyGenericOptions String

foreign import data GridClassKey :: Type

gridClassKey :: ∀ required given. Prim.Row.Union given required GridClassKeyOptions => Record given -> GridClassKey
gridClassKey = Unsafe.Coerce.unsafeCoerce

type GridClassKeyOptionsJSS  = GridClassKeyGenericOptions MUI.Core.JSS

foreign import data GridClassKeyJSS :: Type

gridClassKeyJSS :: ∀ required given. Prim.Row.Union given required GridClassKeyOptionsJSS => Record given -> GridClassKeyJSS
gridClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Grid :: ∀ a. React.Basic.ReactComponent a

grid :: ∀ required given. Prim.Row.Union given required (GridPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
grid = React.Basic.element _Grid

grid_component :: ∀ required given componentProps. Prim.Row.Union given required (GridPropsOptions componentProps) => Record given -> React.Basic.JSX
grid_component = React.Basic.element _Grid

gridWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (GridPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ GridClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
gridWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Grid)
