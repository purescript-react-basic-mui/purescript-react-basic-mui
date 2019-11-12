module MUI.Core.ExpansionPanelSummary where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.IconButton (IconButtonPropsPartial) as MUI.Core.IconButton
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type ExpansionPanelSummaryPropsOptions componentProps = ( "IconButtonProps" :: MUI.Core.IconButton.IconButtonPropsPartial, children :: Array React.Basic.JSX, classes :: ExpansionPanelSummaryClassKey, expandIcon :: React.Basic.JSX | componentProps )

foreign import data ExpansionPanelSummaryProps :: Type

foreign import data ExpansionPanelSummaryPropsPartial :: Type

expansionPanelSummaryPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ExpansionPanelSummaryPropsOptions React.Basic.DOM.Props_div) => Record options -> ExpansionPanelSummaryPropsPartial
expansionPanelSummaryPropsPartial = Unsafe.Coerce.unsafeCoerce

type ExpansionPanelSummaryClassKeyGenericOptions a = ( content :: a, disabled :: a, expandIcon :: a, expanded :: a, focused :: a, root :: a )

type ExpansionPanelSummaryClassKeyOptions  = ExpansionPanelSummaryClassKeyGenericOptions String

foreign import data ExpansionPanelSummaryClassKey :: Type

expansionPanelSummaryClassKey :: ∀ required given. Prim.Row.Union given required ExpansionPanelSummaryClassKeyOptions => Record given -> ExpansionPanelSummaryClassKey
expansionPanelSummaryClassKey = Unsafe.Coerce.unsafeCoerce

type ExpansionPanelSummaryClassKeyOptionsJSS  = ExpansionPanelSummaryClassKeyGenericOptions MUI.Core.JSS

foreign import data ExpansionPanelSummaryClassKeyJSS :: Type

expansionPanelSummaryClassKeyJSS :: ∀ required given. Prim.Row.Union given required ExpansionPanelSummaryClassKeyOptionsJSS => Record given -> ExpansionPanelSummaryClassKeyJSS
expansionPanelSummaryClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _ExpansionPanelSummary :: ∀ a. React.Basic.ReactComponent a

expansionPanelSummary :: ∀ required given. Prim.Row.Union given required (ExpansionPanelSummaryPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
expansionPanelSummary = React.Basic.element _ExpansionPanelSummary

expansionPanelSummary_component :: ∀ required given componentProps. Prim.Row.Union given required (ExpansionPanelSummaryPropsOptions componentProps) => Record given -> React.Basic.JSX
expansionPanelSummary_component = React.Basic.element _ExpansionPanelSummary

expansionPanelSummaryWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ExpansionPanelSummaryPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ ExpansionPanelSummaryClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
expansionPanelSummaryWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _ExpansionPanelSummary)