module React.Basic.MUI.Core.ExpansionPanelSummary where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Core.IconButton (IconButtonProps)
import React.Basic.Events (EventHandler)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

expansionPanelSummary :: EventHandler
expansionPanelSummary = _ExpansionPanelSummary
foreign import _ExpansionPanelSummary :: EventHandler

type ExpansionPanelSummaryClassKey = Foreign

type ExpansionPanelSummaryProps = SimplifiedPropsOf ExpansionPanelSummary