module MUI.Core.Internal where

import Data.Maybe (Maybe) 
import Foreign (unsafeToForeign)
import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

newtype InternalJSX = InternalJSX JSX
newtype InternalEventHandler = InternalEventHandler EventHandler

toInternalChildren :: ∀ r. { children :: Maybe (Array JSX) | r } -> { children :: Maybe (Array InternalJSX) | r }
toInternalChildren record = unsafeCoerce record 

toInternalMaybe :: Maybe JSX -> Maybe InternalJSX
toInternalMaybe = unsafeCoerce

toInternalMaybeEH :: Maybe EventHandler -> Maybe InternalEventHandler
toInternalMaybeEH = unsafeCoerce

onClose :: ∀ r. { onClose :: Maybe EventHandler | r } -> { onClose :: Maybe InternalEventHandler | r }
onClose = unsafeCoerce 

onEnter :: ∀ r. { onEnter :: Maybe EventHandler | r } -> { onEnter :: Maybe InternalEventHandler | r }
onEnter = unsafeCoerce 

onEntering :: ∀ r. { onEntering :: Maybe EventHandler | r } -> { onEntering :: Maybe InternalEventHandler | r }
onEntering = unsafeCoerce 

onEntered :: ∀ r. { onEntered :: Maybe EventHandler | r } -> { onEntered :: Maybe InternalEventHandler | r }
onEntered = unsafeCoerce 

onExit :: ∀ r. { onExit :: Maybe EventHandler | r } -> { onExit :: Maybe InternalEventHandler | r }
onExit = unsafeCoerce 

onExiting :: ∀ r. { onExiting :: Maybe EventHandler | r } -> { onExiting :: Maybe InternalEventHandler | r }
onExiting = unsafeCoerce 

onExited :: ∀ r. { onExited :: Maybe EventHandler | r } -> { onExited :: Maybe InternalEventHandler | r }
onExited = unsafeCoerce 

addEndListener :: ∀ r. { addEndListener :: Maybe EventHandler | r } -> { addEndListener :: Maybe InternalEventHandler | r }
addEndListener = unsafeCoerce

instance writeForeignInternalJSX :: WriteForeign InternalJSX where writeImpl = unsafeToForeign
instance writeForeignInternalEventHandler :: WriteForeign InternalEventHandler where writeImpl = unsafeToForeign