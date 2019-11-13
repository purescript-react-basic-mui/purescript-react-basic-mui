module MUI.Core.Grid where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Xs :: Type

xs :: { auto :: Xs, eight :: Xs, eleven :: Xs, "false" :: Xs, five :: Xs, four :: Xs, nine :: Xs, one :: Xs, seven :: Xs, six :: Xs, ten :: Xs, three :: Xs, "true" :: Xs, twelve :: Xs, two :: Xs }
xs = { auto: Unsafe.Coerce.unsafeCoerce "auto", eight: Unsafe.Coerce.unsafeCoerce 8.0, eleven: Unsafe.Coerce.unsafeCoerce 11.0, "false": Unsafe.Coerce.unsafeCoerce false, five: Unsafe.Coerce.unsafeCoerce 5.0, four: Unsafe.Coerce.unsafeCoerce 4.0, nine: Unsafe.Coerce.unsafeCoerce 9.0, one: Unsafe.Coerce.unsafeCoerce 1.0, seven: Unsafe.Coerce.unsafeCoerce 7.0, six: Unsafe.Coerce.unsafeCoerce 6.0, ten: Unsafe.Coerce.unsafeCoerce 10.0, three: Unsafe.Coerce.unsafeCoerce 3.0, "true": Unsafe.Coerce.unsafeCoerce true, twelve: Unsafe.Coerce.unsafeCoerce 12.0, two: Unsafe.Coerce.unsafeCoerce 2.0 }

foreign import data Xl :: Type

xl :: { auto :: Xl, eight :: Xl, eleven :: Xl, "false" :: Xl, five :: Xl, four :: Xl, nine :: Xl, one :: Xl, seven :: Xl, six :: Xl, ten :: Xl, three :: Xl, "true" :: Xl, twelve :: Xl, two :: Xl }
xl = { auto: Unsafe.Coerce.unsafeCoerce "auto", eight: Unsafe.Coerce.unsafeCoerce 8.0, eleven: Unsafe.Coerce.unsafeCoerce 11.0, "false": Unsafe.Coerce.unsafeCoerce false, five: Unsafe.Coerce.unsafeCoerce 5.0, four: Unsafe.Coerce.unsafeCoerce 4.0, nine: Unsafe.Coerce.unsafeCoerce 9.0, one: Unsafe.Coerce.unsafeCoerce 1.0, seven: Unsafe.Coerce.unsafeCoerce 7.0, six: Unsafe.Coerce.unsafeCoerce 6.0, ten: Unsafe.Coerce.unsafeCoerce 10.0, three: Unsafe.Coerce.unsafeCoerce 3.0, "true": Unsafe.Coerce.unsafeCoerce true, twelve: Unsafe.Coerce.unsafeCoerce 12.0, two: Unsafe.Coerce.unsafeCoerce 2.0 }

foreign import data Wrap :: Type

wrap :: { "WrapReverse" :: Wrap, nowrap :: Wrap, wrap :: Wrap }
wrap = { "WrapReverse": Unsafe.Coerce.unsafeCoerce "wrap-reverse", nowrap: Unsafe.Coerce.unsafeCoerce "nowrap", wrap: Unsafe.Coerce.unsafeCoerce "wrap" }

foreign import data Spacing :: Type

spacing :: { "_0.0" :: Spacing, eight :: Spacing, five :: Spacing, four :: Spacing, nine :: Spacing, one :: Spacing, seven :: Spacing, six :: Spacing, ten :: Spacing, three :: Spacing, two :: Spacing }
spacing = { "_0.0": Unsafe.Coerce.unsafeCoerce 0.0, eight: Unsafe.Coerce.unsafeCoerce 8.0, five: Unsafe.Coerce.unsafeCoerce 5.0, four: Unsafe.Coerce.unsafeCoerce 4.0, nine: Unsafe.Coerce.unsafeCoerce 9.0, one: Unsafe.Coerce.unsafeCoerce 1.0, seven: Unsafe.Coerce.unsafeCoerce 7.0, six: Unsafe.Coerce.unsafeCoerce 6.0, ten: Unsafe.Coerce.unsafeCoerce 10.0, three: Unsafe.Coerce.unsafeCoerce 3.0, two: Unsafe.Coerce.unsafeCoerce 2.0 }

foreign import data Sm :: Type

sm :: { auto :: Sm, eight :: Sm, eleven :: Sm, "false" :: Sm, five :: Sm, four :: Sm, nine :: Sm, one :: Sm, seven :: Sm, six :: Sm, ten :: Sm, three :: Sm, "true" :: Sm, twelve :: Sm, two :: Sm }
sm = { auto: Unsafe.Coerce.unsafeCoerce "auto", eight: Unsafe.Coerce.unsafeCoerce 8.0, eleven: Unsafe.Coerce.unsafeCoerce 11.0, "false": Unsafe.Coerce.unsafeCoerce false, five: Unsafe.Coerce.unsafeCoerce 5.0, four: Unsafe.Coerce.unsafeCoerce 4.0, nine: Unsafe.Coerce.unsafeCoerce 9.0, one: Unsafe.Coerce.unsafeCoerce 1.0, seven: Unsafe.Coerce.unsafeCoerce 7.0, six: Unsafe.Coerce.unsafeCoerce 6.0, ten: Unsafe.Coerce.unsafeCoerce 10.0, three: Unsafe.Coerce.unsafeCoerce 3.0, "true": Unsafe.Coerce.unsafeCoerce true, twelve: Unsafe.Coerce.unsafeCoerce 12.0, two: Unsafe.Coerce.unsafeCoerce 2.0 }

foreign import data Md :: Type

md :: { auto :: Md, eight :: Md, eleven :: Md, "false" :: Md, five :: Md, four :: Md, nine :: Md, one :: Md, seven :: Md, six :: Md, ten :: Md, three :: Md, "true" :: Md, twelve :: Md, two :: Md }
md = { auto: Unsafe.Coerce.unsafeCoerce "auto", eight: Unsafe.Coerce.unsafeCoerce 8.0, eleven: Unsafe.Coerce.unsafeCoerce 11.0, "false": Unsafe.Coerce.unsafeCoerce false, five: Unsafe.Coerce.unsafeCoerce 5.0, four: Unsafe.Coerce.unsafeCoerce 4.0, nine: Unsafe.Coerce.unsafeCoerce 9.0, one: Unsafe.Coerce.unsafeCoerce 1.0, seven: Unsafe.Coerce.unsafeCoerce 7.0, six: Unsafe.Coerce.unsafeCoerce 6.0, ten: Unsafe.Coerce.unsafeCoerce 10.0, three: Unsafe.Coerce.unsafeCoerce 3.0, "true": Unsafe.Coerce.unsafeCoerce true, twelve: Unsafe.Coerce.unsafeCoerce 12.0, two: Unsafe.Coerce.unsafeCoerce 2.0 }

foreign import data Lg :: Type

lg :: { auto :: Lg, eight :: Lg, eleven :: Lg, "false" :: Lg, five :: Lg, four :: Lg, nine :: Lg, one :: Lg, seven :: Lg, six :: Lg, ten :: Lg, three :: Lg, "true" :: Lg, twelve :: Lg, two :: Lg }
lg = { auto: Unsafe.Coerce.unsafeCoerce "auto", eight: Unsafe.Coerce.unsafeCoerce 8.0, eleven: Unsafe.Coerce.unsafeCoerce 11.0, "false": Unsafe.Coerce.unsafeCoerce false, five: Unsafe.Coerce.unsafeCoerce 5.0, four: Unsafe.Coerce.unsafeCoerce 4.0, nine: Unsafe.Coerce.unsafeCoerce 9.0, one: Unsafe.Coerce.unsafeCoerce 1.0, seven: Unsafe.Coerce.unsafeCoerce 7.0, six: Unsafe.Coerce.unsafeCoerce 6.0, ten: Unsafe.Coerce.unsafeCoerce 10.0, three: Unsafe.Coerce.unsafeCoerce 3.0, "true": Unsafe.Coerce.unsafeCoerce true, twelve: Unsafe.Coerce.unsafeCoerce 12.0, two: Unsafe.Coerce.unsafeCoerce 2.0 }

foreign import data Justify :: Type

justify :: { "FlexEnd" :: Justify, "FlexStart" :: Justify, "SpaceAround" :: Justify, "SpaceBetween" :: Justify, "SpaceEvenly" :: Justify, center :: Justify }
justify = { "FlexEnd": Unsafe.Coerce.unsafeCoerce "flex-end", "FlexStart": Unsafe.Coerce.unsafeCoerce "flex-start", "SpaceAround": Unsafe.Coerce.unsafeCoerce "space-around", "SpaceBetween": Unsafe.Coerce.unsafeCoerce "space-between", "SpaceEvenly": Unsafe.Coerce.unsafeCoerce "space-evenly", center: Unsafe.Coerce.unsafeCoerce "center" }

foreign import data Direction :: Type

direction :: { "ColumnReverse" :: Direction, "RowReverse" :: Direction, column :: Direction, row :: Direction }
direction = { "ColumnReverse": Unsafe.Coerce.unsafeCoerce "column-reverse", "RowReverse": Unsafe.Coerce.unsafeCoerce "row-reverse", column: Unsafe.Coerce.unsafeCoerce "column", row: Unsafe.Coerce.unsafeCoerce "row" }

foreign import data AlignItems :: Type

alignItems :: { "FlexEnd" :: AlignItems, "FlexStart" :: AlignItems, baseline :: AlignItems, center :: AlignItems, stretch :: AlignItems }
alignItems = { "FlexEnd": Unsafe.Coerce.unsafeCoerce "flex-end", "FlexStart": Unsafe.Coerce.unsafeCoerce "flex-start", baseline: Unsafe.Coerce.unsafeCoerce "baseline", center: Unsafe.Coerce.unsafeCoerce "center", stretch: Unsafe.Coerce.unsafeCoerce "stretch" }

foreign import data AlignContent :: Type

alignContent :: { "FlexEnd" :: AlignContent, "FlexStart" :: AlignContent, "SpaceAround" :: AlignContent, "SpaceBetween" :: AlignContent, center :: AlignContent, stretch :: AlignContent }
alignContent = { "FlexEnd": Unsafe.Coerce.unsafeCoerce "flex-end", "FlexStart": Unsafe.Coerce.unsafeCoerce "flex-start", "SpaceAround": Unsafe.Coerce.unsafeCoerce "space-around", "SpaceBetween": Unsafe.Coerce.unsafeCoerce "space-between", center: Unsafe.Coerce.unsafeCoerce "center", stretch: Unsafe.Coerce.unsafeCoerce "stretch" }

instance eqAlignContent :: Eq AlignContent where
  eq = Unsafe.Reference.unsafeRefEq

instance eqAlignItems :: Eq AlignItems where
  eq = Unsafe.Reference.unsafeRefEq

instance eqDirection :: Eq Direction where
  eq = Unsafe.Reference.unsafeRefEq

instance eqJustify :: Eq Justify where
  eq = Unsafe.Reference.unsafeRefEq

instance eqLg :: Eq Lg where
  eq = Unsafe.Reference.unsafeRefEq

instance eqMd :: Eq Md where
  eq = Unsafe.Reference.unsafeRefEq

instance eqSm :: Eq Sm where
  eq = Unsafe.Reference.unsafeRefEq

instance eqSpacing :: Eq Spacing where
  eq = Unsafe.Reference.unsafeRefEq

instance eqWrap :: Eq Wrap where
  eq = Unsafe.Reference.unsafeRefEq

instance eqXl :: Eq Xl where
  eq = Unsafe.Reference.unsafeRefEq

instance eqXs :: Eq Xs where
  eq = Unsafe.Reference.unsafeRefEq

type GridPropsOptions componentProps = ( alignContent :: AlignContent, alignItems :: AlignItems, children :: Array React.Basic.JSX, classes :: GridClassKey, container :: Boolean, direction :: Direction, item :: Boolean, justify :: Justify, lg :: Lg, md :: Md, sm :: Sm, spacing :: Spacing, wrap :: Wrap, xl :: Xl, xs :: Xs, zeroMinWidth :: Boolean | componentProps )

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