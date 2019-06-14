module React.Basic.MUI. where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type WithThemeCreatorOption  theme =
  { defaultTheme :: Foreign
  }

type WithThemeCreatorOption_required theme =
  ( 
  )

type WithThemeCreatorOption_optional theme =
  ( defaultTheme :: Foreign
  )

withThemeCreator :: Foreign -> Foreign
withThemeCreator = _withThemeCreator
foreign import _withThemeCreator :: Foreign -> Foreign

type WithTheme  theme =
  { theme :: Foreign
  , innerRef :: Foreign
  }

type WithTheme_required theme =
  ( theme :: Foreign
  )

type WithTheme_optional theme =
  ( innerRef :: Foreign
  )

withTheme :: Foreign -> JSX
withTheme = _withTheme
foreign import _withTheme :: Foreign -> JSX