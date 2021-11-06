module MUI.React.Aria where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Haspopup ∷ Type

haspopup ::
  { dialog :: Haspopup
  , "false" :: Haspopup
  , grid :: Haspopup
  , listbox :: Haspopup
  , menu :: Haspopup
  , tree :: Haspopup
  , "true" :: Haspopup
  }
haspopup =
  { dialog: unsafeCoerce "dialog"
  , "false": unsafeCoerce "false"
  , grid: unsafeCoerce "grid"
  , listbox: unsafeCoerce "listbox"
  , menu: unsafeCoerce "menu"
  , tree: unsafeCoerce "tree"
  , "true": unsafeCoerce "true"
  }
