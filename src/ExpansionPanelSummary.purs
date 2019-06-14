module React.Basic.MUI.ExpansionPanelSummary where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.IconButton (IconButtonProps)
import React.Basic.Events (EventHandler)

expansionPanelSummary :: EventHandler
expansionPanelSummary = _ExpansionPanelSummary
foreign import _ExpansionPanelSummary :: EventHandler

type ExpansionPanelSummaryClassKey = Foreign

type ExpansionPanelSummaryProps = SimplifiedPropsOf ExpansionPanelSummary