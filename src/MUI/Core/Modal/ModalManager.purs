module MUI.Core.Modal.ModalManager where

import Prelude
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Simple.JSON (class WriteForeign)

foreign import data ModalManager :: Type

foreign import constructor :: Foreign -> Foreign -> Effect ModalManager

foreign import add :: ModalManager -> Foreign -> Foreign -> Effect Number

foreign import remove :: ModalManager -> Foreign -> Effect Unit

foreign import isTopModal :: ModalManager -> Foreign -> Effect Unit

instance writeForeignModalManager :: WriteForeign ModalManager where
  writeImpl = unsafeToForeign
