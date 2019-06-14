module React.Basic.MUI.NativeSelect.NativeSelectInput where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)

type NativeSelectInputProps_optional =
  ( disabled :: Boolean
  , "IconComponent" :: JSX
  , inputRef :: Foreign
  , name :: String
  , onChange :: Foreign
  , value :: Foreign
  , variant :: Foreign
  )

foreign import data NativeSelectInputProps :: Type 

nativeSelectInputProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (NativeSelectInputProps_optional)
  => Record (attrs)
  -> NativeSelectInputProps
nativeSelectInputProps = unsafeCoerce

nativeSelectInput
  :: ∀ attrs attrs_
   . Union attrs attrs_ (NativeSelectInputProps_optional)
  => Record (attrs)
  -> JSX
nativeSelectInput = element _NativeSelectInput
foreign import _NativeSelectInput :: forall a. ReactComponent a 