module React.Basic.MUI.Core.Styles.ZIndex where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type ZIndex_required  optional =
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



type ZIndexOptions = Foreign

zIndex :: ZIndex 
zIndex = _zIndex
foreign import _zIndex :: ZIndex 