module React.Basic.MUI.Styles.Shape where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type Shape_required optional =
  ( borderRadius :: Number
  | optional )

type Shape_optional =
  ( 
  )

foreign import data Shape :: Type 

shape
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Shape_optional)
  => Record (Shape_required attrs)
  -> Shape
shape = unsafeCoerce

type ShapeOptions = Foreign

shape :: Shape 
shape = _shape
foreign import _shape :: Shape 