module React.Basic.MUI.Core.Styles.WithTheme where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Styles.CreateMuiTheme (Theme)

type WithTheme_required  optional =
  ( theme :: Theme 
  | optional )

type WithTheme_optional =
  ( 
  )

foreign import data WithTheme :: Type 



type ThemedComponentProps_optional =
  ( innerRef :: Foreign
  , theme :: Theme 
  )

foreign import data ThemedComponentProps :: Type 

themedComponentProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ThemedComponentProps_optional )
  => Record (attrs)
  -> ThemedComponentProps
themedComponentProps = unsafeCoerce

withTheme :: Foreign
withTheme = _withTheme
foreign import _withTheme :: Foreign