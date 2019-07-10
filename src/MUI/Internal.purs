module MUI.Core.Internal where

import Data.Maybe (Maybe)
import Foreign (unsafeToForeign)
import React.Basic (JSX)
import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

newtype InternalJSX = InternalJSX JSX

toInternalChildren :: ∀ r. { children :: Maybe (Array JSX) | r } -> { children :: Maybe (Array InternalJSX) | r }
toInternalChildren record = unsafeCoerce record 

toInternalMaybe :: Maybe JSX -> Maybe InternalJSX
toInternalMaybe = unsafeCoerce

instance writeForeignInternalJSX :: WriteForeign InternalJSX where writeImpl = unsafeToForeign