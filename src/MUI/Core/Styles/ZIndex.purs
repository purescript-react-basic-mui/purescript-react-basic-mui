module MUI.Core.Styles.ZIndex where

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type ZIndexPartial
  = ( mobileStepper :: Number
    , appBar :: Number
    , drawer :: Number
    , modal :: Number
    , snackbar :: Number
    , tooltip :: Number
    )

type ZIndex
  = Record ZIndexPartial

foreign import data ZIndexOptions :: Type

zIndexOptions ::
  ∀ options options_.
  Union options options_ ZIndexPartial =>
  Record options ->
  ZIndexOptions
zIndexOptions = unsafeCoerce

foreign import zIndex :: ZIndex
