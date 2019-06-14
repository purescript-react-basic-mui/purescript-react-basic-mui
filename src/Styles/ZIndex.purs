module React.Basic.MUI.Styles.ZIndex where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type ZIndex_required optional =
  ( mobileStepper :: Number
  , appBar :: Number
  , drawer :: Number
  , modal :: Number
  , snackbar :: Number
  , tooltip :: Number
  | optional )

type ZIndex_optional =
  ( 
  )

foreign import data ZIndex :: Type 

zIndex
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ZIndex_optional)
  => Record (ZIndex_required attrs)
  -> ZIndex
zIndex = unsafeCoerce

type ZIndexOptions = Foreign

zIndex :: ZIndex 
zIndex = _zIndex
foreign import _zIndex :: ZIndex 