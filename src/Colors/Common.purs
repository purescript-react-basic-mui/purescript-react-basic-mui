module React.Basic.MUI.Colors.Common where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type CommonColors_required optional =
  ( black :: String
  , white :: String
  | optional )

type CommonColors_optional =
  ( 
  )

foreign import data CommonColors :: Type 

commonColors
  :: ∀ attrs attrs_
   . Union attrs attrs_ (CommonColors_optional)
  => Record (CommonColors_required attrs)
  -> CommonColors
commonColors = unsafeCoerce

common :: Foreign
common = _common
foreign import _common :: Foreign