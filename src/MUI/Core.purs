module MUI.Core where

import Prelude

import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
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

instance semigroupJSS :: Semigroup JSS where
  append jss1 jss2 = 
    let
      (jss1Obj :: Object Foreign) = unsafeCoerce jss1
      (jss2Obj :: Object Foreign) = unsafeCoerce jss2
    in unsafeCoerce $ Object.union jss1Obj jss2Obj

instance monoidJSS :: Monoid JSS where mempty = jss {}
  
