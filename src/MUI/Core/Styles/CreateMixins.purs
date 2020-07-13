module MUI.Core.Styles.CreateMixins where

import Prelude
import Foreign (Foreign, unsafeToForeign)
import MUI.Core (JSS)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type MixinsPartial
  = ( gutters :: JSS -> JSS
    , toolbar :: JSS
    )

foreign import data MixinsOptions :: Type

type Mixins
  = Record MixinsPartial

mixinsOptions ::
  ∀ options options_.
  Union options options_ MixinsPartial =>
  Record options ->
  MixinsOptions
mixinsOptions = unsafeCoerce

createMixins ::
  ∀ options options_.
  Union options options_ MixinsPartial =>
  Record options ->
  Mixins
createMixins = _createMixins <<< unsafeToForeign

foreign import _createMixins :: Foreign -> Mixins
