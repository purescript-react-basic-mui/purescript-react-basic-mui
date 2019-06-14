module React.Basic.MUI.Styles.CreateMixins where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type Mixins  =
  { gutters :: Foreign
  , toolbar :: CSS
  }

type Mixins_required =
  ( gutters :: Foreign
  , toolbar :: CSS
  )

type Mixins_optional =
  ( 
  )

type MixinsOptions  =
  { gutters :: Foreign
  , toolbar :: CSS
  }

type MixinsOptions_required =
  ( 
  )

type MixinsOptions_optional =
  ( gutters :: Foreign
  , toolbar :: CSS
  )

createMixins :: Foreign -> Foreign
createMixins = _createMixins
foreign import _createMixins :: Foreign -> Foreign