module React.Basic.MUI.Core.WithWidth where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Styles.CreateBreakpoints (Breakpoint)

type WithWidthOptions_optional =
  ( withTheme :: Boolean
  , noSSR :: Boolean
  , initialWidth :: Breakpoint 
  , resizeInterval :: Number
  )

foreign import data WithWidthOptions :: Type 



type WithWidth_required  optional =
  ( width :: Breakpoint 
  | optional )

type WithWidth_optional =
  ( 
  )

foreign import data WithWidth :: Type 



type WithWidthProps_optional =
  ( innerRef :: Foreign
  , width :: Breakpoint 
  )

foreign import data WithWidthProps :: Type 

withWidthProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithWidthProps_optional )
  => Record (attrs)
  -> WithWidthProps
withWidthProps = unsafeCoerce

isWidthDown :: Boolean
isWidthDown = _isWidthDown
foreign import _isWidthDown :: Boolean

isWidthUp :: Boolean
isWidthUp = _isWidthUp
foreign import _isWidthUp :: Boolean

withWidth :: Foreign
withWidth = _withWidth
foreign import _withWidth :: Foreign