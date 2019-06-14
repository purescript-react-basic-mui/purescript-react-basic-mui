module React.Basic.MUI.ButtonBase where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ButtonBaseTypeMap  =
  { props :: Foreign
  , defaultComponent :: String
  , classKey :: Foreign
  }

type ButtonBaseTypeMap_required =
  ( props :: Foreign
  , defaultComponent :: String
  , classKey :: Foreign
  )

type ButtonBaseTypeMap_optional =
  ( 
  )

type ExtendButtonBaseTypeMap  m =
  { props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  }

type ExtendButtonBaseTypeMap_required m =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  )

type ExtendButtonBaseTypeMap_optional m =
  ( 
  )

buttonBase :: Foreign
buttonBase = _ButtonBase
foreign import _ButtonBase :: Foreign

type ButtonBaseActions  =
  { focusVisible :: Foreign
  }

type ButtonBaseActions_required =
  ( focusVisible :: Foreign
  )

type ButtonBaseActions_optional =
  ( 
  )