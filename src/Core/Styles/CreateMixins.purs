module React.Basic.MUI.Core.Styles.CreateMixins where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic.MUI.Core.Styles.CreateBreakpoints (Breakpoints)
import React.Basic.MUI.Core.Styles.CreateSpacing (Spacing)

type Mixins_required  optional =
  ( gutters :: Foreign
  , toolbar :: CSS
  | optional )

type Mixins_optional =
  ( 
  )

foreign import data Mixins :: Type 



type MixinsOptions_optional =
  ( gutters :: Foreign
  , toolbar :: CSS
  )

foreign import data MixinsOptions :: Type 



createMixins :: Mixins 
createMixins = _createMixins
foreign import _createMixins :: Mixins 