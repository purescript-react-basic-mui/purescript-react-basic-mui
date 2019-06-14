module React.Basic.MUI.Styles.CreateSpacing where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type SpacingArgument = Foreign

type Spacing_optional =
  ( 
  )

foreign import data Spacing :: Type 

spacing
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Spacing_optional)
  => Record (attrs)
  -> Spacing
spacing = unsafeCoerce

type SpacingOptions = Foreign

createSpacing :: Spacing 
createSpacing = _createSpacing
foreign import _createSpacing :: Spacing 