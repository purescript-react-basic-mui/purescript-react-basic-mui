module React.Basic.MUI.Styles.WithTheme where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type WithTheme  =
  { theme :: Foreign
  }

type WithTheme_required =
  ( theme :: Foreign
  )

type WithTheme_optional =
  ( 
  )

type ThemedComponentProps  =
  { innerRef :: Foreign
  , theme :: Foreign
  }

type ThemedComponentProps_required =
  ( 
  )

type ThemedComponentProps_optional =
  ( innerRef :: Foreign
  , theme :: Foreign
  )

withTheme :: Foreign
withTheme = _withTheme
foreign import _withTheme :: Foreign