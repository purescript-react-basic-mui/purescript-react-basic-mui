module React.Basic.MUI.Styles.WithTheme where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.CreateMuiTheme (Theme)
import React.Basic (element, ReactComponent, ReactComponent)

type WithTheme_required optional =
  ( theme :: Theme 
  | optional )

type WithTheme_optional =
  ( 
  )

foreign import data WithTheme :: Type 

withTheme
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithTheme_optional)
  => Record (WithTheme_required attrs)
  -> WithTheme
withTheme = unsafeCoerce

type ThemedComponentProps_optional =
  ( innerRef :: Foreign
  , theme :: Theme 
  )

foreign import data ThemedComponentProps :: Type 

themedComponentProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ThemedComponentProps_optional)
  => Record (attrs)
  -> ThemedComponentProps
themedComponentProps = unsafeCoerce

withTheme :: Foreign
withTheme = _withTheme
foreign import _withTheme :: Foreign