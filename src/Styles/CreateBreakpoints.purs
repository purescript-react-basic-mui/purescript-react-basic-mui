module React.Basic.MUI.Styles.CreateBreakpoints where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


keys :: Foreign
keys = _keys
foreign import _keys :: Foreign

type Breakpoints  =
  { keys :: Foreign
  , values :: Foreign
  , up :: Foreign
  , down :: Foreign
  , between :: Foreign
  , only :: Foreign
  , width :: Foreign
  }

type Breakpoints_required =
  ( keys :: Foreign
  , values :: Foreign
  , up :: Foreign
  , down :: Foreign
  , between :: Foreign
  , only :: Foreign
  , width :: Foreign
  )

type Breakpoints_optional =
  ( 
  )

createBreakpoints :: Foreign -> Foreign
createBreakpoints = _createBreakpoints
foreign import _createBreakpoints :: Foreign -> Foreign