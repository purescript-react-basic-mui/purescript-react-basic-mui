module React.Basic.MUI.WithWidth where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.CreateBreakpoints (Breakpoint)

type WithWidthOptions_optional =
  ( withTheme :: Boolean
  , noSSR :: Boolean
  , initialWidth :: Breakpoint 
  , resizeInterval :: Number
  )

foreign import data WithWidthOptions :: Type 

withWidthOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithWidthOptions_optional)
  => Record (attrs)
  -> WithWidthOptions
withWidthOptions = unsafeCoerce

type WithWidth_required optional =
  ( width :: Breakpoint 
  | optional )

type WithWidth_optional =
  ( 
  )

foreign import data WithWidth :: Type 

withWidth
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithWidth_optional)
  => Record (WithWidth_required attrs)
  -> WithWidth
withWidth = unsafeCoerce

type WithWidthProps_optional =
  ( innerRef :: Foreign
  , width :: Breakpoint 
  )

foreign import data WithWidthProps :: Type 

withWidthProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithWidthProps_optional)
  => Record (attrs)
  -> WithWidthProps
withWidthProps = unsafeCoerce

isWidthDown :: Boolean
isWidthDown = _isWidthDown
foreign import _isWidthDown :: Boolean

isWidthUp :: Boolean
isWidthUp = _isWidthUp
foreign import _isWidthUp :: Boolean

withWidth :: PropInjector WithWidth  WithWidthProps 
withWidth = _withWidth
foreign import _withWidth :: PropInjector WithWidth  WithWidthProps 