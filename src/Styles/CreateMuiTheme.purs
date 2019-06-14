module React.Basic.MUI.Styles.CreateMuiTheme where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ThemeOptions  =
  { shape :: Foreign
  , breakpoints :: Foreign
  , direction :: Foreign
  , mixins :: Foreign
  , overrides :: Foreign
  , palette :: Foreign
  , props :: Foreign
  , shadows :: Foreign
  , spacing :: Foreign
  , transitions :: Foreign
  , typography :: Foreign
  , zIndex :: Foreign
  }

type ThemeOptions_required =
  ( 
  )

type ThemeOptions_optional =
  ( shape :: Foreign
  , breakpoints :: Foreign
  , direction :: Foreign
  , mixins :: Foreign
  , overrides :: Foreign
  , palette :: Foreign
  , props :: Foreign
  , shadows :: Foreign
  , spacing :: Foreign
  , transitions :: Foreign
  , typography :: Foreign
  , zIndex :: Foreign
  )

type Theme  =
  { shape :: Foreign
  , breakpoints :: Foreign
  , direction :: Foreign
  , mixins :: Foreign
  , overrides :: Foreign
  , palette :: Foreign
  , props :: Foreign
  , shadows :: Foreign
  , spacing :: Foreign
  , transitions :: Foreign
  , typography :: Foreign
  , zIndex :: Foreign
  }

type Theme_required =
  ( shape :: Foreign
  , breakpoints :: Foreign
  , direction :: Foreign
  , mixins :: Foreign
  , palette :: Foreign
  , shadows :: Foreign
  , spacing :: Foreign
  , transitions :: Foreign
  , typography :: Foreign
  , zIndex :: Foreign
  )

type Theme_optional =
  ( overrides :: Foreign
  , props :: Foreign
  )

createMuiTheme :: Foreign -> Foreign
createMuiTheme = _createMuiTheme
foreign import _createMuiTheme :: Foreign -> Foreign