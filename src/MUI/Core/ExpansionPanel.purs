module MUI.Core.ExpansionPanel where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Paper (PaperPropsOptions) as MUI.Core.Paper
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type ExpansionPanelPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: ExpansionPanelClassKey, defaultExpanded :: Boolean, disabled :: Boolean, expanded :: Boolean, onChange :: React.Basic.Events.EventHandler | componentProps )

foreign import data ExpansionPanelProps :: Type

foreign import data ExpansionPanelPropsPartial :: Type

expansionPanelPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ExpansionPanelPropsOptions (MUI.Core.Paper.PaperPropsOptions React.Basic.DOM.Props_div)) => Record options -> ExpansionPanelPropsPartial
expansionPanelPropsPartial = Unsafe.Coerce.unsafeCoerce

type ExpansionPanelClassKeyGenericOptions a = ( disabled :: a, expanded :: a, root :: a, rounded :: a )

type ExpansionPanelClassKeyOptions  = ExpansionPanelClassKeyGenericOptions String

foreign import data ExpansionPanelClassKey :: Type

expansionPanelClassKey :: ∀ required given. Prim.Row.Union given required ExpansionPanelClassKeyOptions => Record given -> ExpansionPanelClassKey
expansionPanelClassKey = Unsafe.Coerce.unsafeCoerce

type ExpansionPanelClassKeyOptionsJSS  = ExpansionPanelClassKeyGenericOptions MUI.Core.JSS

foreign import data ExpansionPanelClassKeyJSS :: Type

expansionPanelClassKeyJSS :: ∀ required given. Prim.Row.Union given required ExpansionPanelClassKeyOptionsJSS => Record given -> ExpansionPanelClassKeyJSS
expansionPanelClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _ExpansionPanel :: ∀ a. React.Basic.ReactComponent a

expansionPanel :: ∀ required given. Prim.Row.Union given required (ExpansionPanelPropsOptions (MUI.Core.Paper.PaperPropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
expansionPanel = React.Basic.element _ExpansionPanel

expansionPanel_component :: ∀ required given componentProps. Prim.Row.Union given required (ExpansionPanelPropsOptions componentProps) => Record given -> React.Basic.JSX
expansionPanel_component = React.Basic.element _ExpansionPanel

expansionPanelWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ExpansionPanelPropsOptions (MUI.Core.Paper.PaperPropsOptions React.Basic.DOM.Props_div)) => Prim.Row.Union jss jss_ ExpansionPanelClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
expansionPanelWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _ExpansionPanel)