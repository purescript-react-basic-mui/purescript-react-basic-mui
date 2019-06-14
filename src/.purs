module React.Basic.MUI. where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.CreateMuiTheme (Theme)

type WithThemeCreatorOption_optional theme =
  ( defaultTheme :: Theme 
  )

foreign import data WithThemeCreatorOption :: Type 

withThemeCreatorOption
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithThemeCreatorOption_optional)
  => Record (attrs)
  -> WithThemeCreatorOption
withThemeCreatorOption = unsafeCoerce

withThemeCreator :: PropInjector WithTheme Theme  Foreign
withThemeCreator = _withThemeCreator
foreign import _withThemeCreator :: PropInjector WithTheme Theme  Foreign

type WithTheme_required optional theme =
  ( theme :: Theme 
  | optional )

type WithTheme_optional theme =
  ( innerRef :: Foreign
  )

foreign import data WithTheme :: Type 

withTheme
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithTheme_optional)
  => Record (WithTheme_required attrs)
  -> WithTheme
withTheme = unsafeCoerce

withTheme
  :: ∀ attrs attrs_
   . Union attrs attrs_ (withThemeProps_optional)
  => Record (attrs)
  -> JSX
withTheme = element _withTheme
foreign import _withTheme :: forall a. ReactComponent a 