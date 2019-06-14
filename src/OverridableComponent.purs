module React.Basic.MUI.OverridableComponent where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type OverridableComponent  m =
  { 
  }

type OverridableComponent_required m =
  ( 
  )

type OverridableComponent_optional m =
  ( 
  )

type CommonProps  m =
  { className :: String
  , style :: CSS
  , classes :: Foreign
  , innerRef :: Foreign
  }

type CommonProps_required m =
  ( 
  )

type CommonProps_optional m =
  ( className :: String
  , style :: CSS
  , classes :: Foreign
  , innerRef :: Foreign
  )

type OverridableTypeMap  =
  { props :: Foreign
  , defaultComponent :: JSX
  , classKey :: String
  }

type OverridableTypeMap_required =
  ( props :: Foreign
  , defaultComponent :: JSX
  , classKey :: String
  )

type OverridableTypeMap_optional =
  ( 
  )