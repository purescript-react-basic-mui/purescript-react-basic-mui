module React.Basic.MUI.GridListTileBar where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type GridListTileBarProps  =
  { actionIcon :: JSX
  , actionPosition :: Foreign
  , subtitle :: JSX
  , title :: JSX
  , titlePosition :: Foreign
  , classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  }

type GridListTileBarProps_required =
  ( 
  )

type GridListTileBarProps_optional =
  ( actionIcon :: JSX
  , actionPosition :: Foreign
  , subtitle :: JSX
  , title :: JSX
  , titlePosition :: Foreign
  , classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  )

gridListTileBar :: JSX
gridListTileBar = _GridListTileBar
foreign import _GridListTileBar :: JSX