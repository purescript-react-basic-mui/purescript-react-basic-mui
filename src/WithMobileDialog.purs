module React.Basic.MUI.WithMobileDialog where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type WithMobileDialogOptions  =
  { breakpoint :: Foreign
  }

type WithMobileDialogOptions_required =
  ( breakpoint :: Foreign
  )

type WithMobileDialogOptions_optional =
  ( 
  )

type WithMobileDialog  =
  { fullScreen :: Boolean
  , width :: Foreign
  }

type WithMobileDialog_required =
  ( fullScreen :: Boolean
  , width :: Foreign
  )

type WithMobileDialog_optional =
  ( 
  )

type InjectedProps  =
  { fullScreen :: Boolean
  , width :: Foreign
  }

type InjectedProps_required =
  ( fullScreen :: Boolean
  , width :: Foreign
  )

type InjectedProps_optional =
  ( 
  )

withMobileDialog :: Foreign -> Foreign
withMobileDialog = _withMobileDialog
foreign import _withMobileDialog :: Foreign -> Foreign