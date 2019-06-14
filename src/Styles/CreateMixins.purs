module React.Basic.MUI.Styles.CreateMixins where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic.MUI.Styles.CreateBreakpoints (Breakpoints)
import React.Basic.MUI.Styles.CreateSpacing (Spacing)

type Mixins_required optional =
  ( gutters :: Foreign
  , toolbar :: CSS
  | optional )

type Mixins_optional =
  ( 
  )

foreign import data Mixins :: Type 

mixins
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Mixins_optional)
  => Record (Mixins_required attrs)
  -> Mixins
mixins = unsafeCoerce

type MixinsOptions_optional =
  ( gutters :: Foreign
  , toolbar :: CSS
  )

foreign import data MixinsOptions :: Type 

mixinsOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (MixinsOptions_optional)
  => Record (attrs)
  -> MixinsOptions
mixinsOptions = unsafeCoerce

createMixins :: Mixins 
createMixins = _createMixins
foreign import _createMixins :: Mixins 