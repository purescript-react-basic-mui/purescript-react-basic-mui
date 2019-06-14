module React.Basic.MUI.Styles.MakeStyles where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


makeStyles :: Foreign -> Foreign
makeStyles = _makeStyles
foreign import _makeStyles :: Foreign -> Foreign