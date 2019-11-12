module MUI.Core.ExpansionPanelDetails where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type ExpansionPanelDetailsPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: ExpansionPanelDetailsClassKey | componentProps )

foreign import data ExpansionPanelDetailsProps :: Type

foreign import data ExpansionPanelDetailsPropsPartial :: Type

expansionPanelDetailsPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ExpansionPanelDetailsPropsOptions React.Basic.DOM.Props_div) => Record options -> ExpansionPanelDetailsPropsPartial
expansionPanelDetailsPropsPartial = Unsafe.Coerce.unsafeCoerce

type ExpansionPanelDetailsClassKeyGenericOptions a = ( root :: a )

type ExpansionPanelDetailsClassKeyOptions  = ExpansionPanelDetailsClassKeyGenericOptions String

foreign import data ExpansionPanelDetailsClassKey :: Type

expansionPanelDetailsClassKey :: ∀ required given. Prim.Row.Union given required ExpansionPanelDetailsClassKeyOptions => Record given -> ExpansionPanelDetailsClassKey
expansionPanelDetailsClassKey = Unsafe.Coerce.unsafeCoerce

type ExpansionPanelDetailsClassKeyOptionsJSS  = ExpansionPanelDetailsClassKeyGenericOptions MUI.Core.JSS

foreign import data ExpansionPanelDetailsClassKeyJSS :: Type

expansionPanelDetailsClassKeyJSS :: ∀ required given. Prim.Row.Union given required ExpansionPanelDetailsClassKeyOptionsJSS => Record given -> ExpansionPanelDetailsClassKeyJSS
expansionPanelDetailsClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _ExpansionPanelDetails :: ∀ a. React.Basic.ReactComponent a

expansionPanelDetails :: ∀ required given. Prim.Row.Union given required (ExpansionPanelDetailsPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
expansionPanelDetails = React.Basic.element _ExpansionPanelDetails

expansionPanelDetails_component :: ∀ required given componentProps. Prim.Row.Union given required (ExpansionPanelDetailsPropsOptions componentProps) => Record given -> React.Basic.JSX
expansionPanelDetails_component = React.Basic.element _ExpansionPanelDetails

expansionPanelDetailsWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ExpansionPanelDetailsPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ ExpansionPanelDetailsClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
expansionPanelDetailsWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _ExpansionPanelDetails)