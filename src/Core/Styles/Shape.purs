module React.Basic.MUI.Core.Styles.Shape where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type Shape_required  optional =
  ( borderRadius :: Number
  | optional )

type Shape_optional =
  ( 
  )

foreign import data Shape :: Type 



type ShapeOptions = Foreign

shape :: Shape 
shape = _shape
foreign import _shape :: Shape 