module MUI.Core where

import Unsafe.Coerce (unsafeCoerce)

type Color =
  { "50"  :: String
  , "100" :: String
  , "200" :: String
  , "300" :: String
  , "400" :: String
  , "500" :: String
  , "600" :: String
  , "700" :: String
  , "800" :: String
  , "900" :: String
  , "A100" :: String
  , "A200" :: String
  , "A400" :: String
  , "A700" :: String
}

jss :: ∀ r. { | r } -> JSS
jss = unsafeCoerce

foreign import data JSS :: Type
