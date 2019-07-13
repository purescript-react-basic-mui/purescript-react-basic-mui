module MUI.Core.Internal where

import Data.Maybe (Maybe)
import Foreign (Foreign, unsafeToForeign)
import MUI.Core.Styles.CreateMuiTheme (Theme)
import React.Basic (Component, JSX)
import React.Basic.Events (EventHandler)
import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

newtype InternalJSX = InternalJSX JSX
newtype InternalEventHandler = InternalEventHandler EventHandler

newtype ComponentFn a = ContainerFn (a -> Component a)

newtype InternalComponent a = InternalComponent (Component a)

toInternalChildren :: ∀ r. { children :: Maybe (Array JSX) | r } -> { children :: Maybe (Array InternalJSX) | r }
toInternalChildren record = unsafeCoerce record 

toInternalMaybe :: Maybe JSX -> Maybe InternalJSX
toInternalMaybe = unsafeCoerce

toInternalMaybeEH :: Maybe EventHandler -> Maybe InternalEventHandler
toInternalMaybeEH = unsafeCoerce

onClose :: ∀ r. { onClose :: Maybe EventHandler | r } -> { onClose :: Maybe InternalEventHandler | r }
onClose = unsafeCoerce 

onClick :: ∀ r. { onClick :: Maybe EventHandler | r } -> { onClick :: Maybe InternalEventHandler | r }
onClick = unsafeCoerce 

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

onBackdropClick :: ∀ r. { onBackdropClick :: Maybe EventHandler | r } -> { onBackdropClick :: Maybe InternalEventHandler | r }
onBackdropClick = unsafeCoerce 

onEscapeKeyDown :: ∀ r. { onEscapeKeyDown :: Maybe EventHandler | r } -> { onEscapeKeyDown :: Maybe InternalEventHandler | r }
onEscapeKeyDown = unsafeCoerce 

onRendered :: ∀ r. { onRendered :: Maybe EventHandler | r } -> { onRendered :: Maybe InternalEventHandler | r }
onRendered = unsafeCoerce 

addEndListener :: ∀ r. { addEndListener :: Maybe EventHandler | r } -> { addEndListener :: Maybe InternalEventHandler | r }
addEndListener = unsafeCoerce

container :: ∀ a r. { container :: Maybe (a -> Component a) | r } -> { container :: (Maybe (ComponentFn a)) | r}
container = unsafeCoerce

theme :: ∀ r. { theme :: Maybe Theme | r } -> { theme :: (Maybe Foreign) | r}
theme = unsafeCoerce

backdropComponent :: ∀ a r. { "BackdropComponent" :: Maybe (Component a) | r } -> { "BackdropComponent" :: (Maybe (InternalComponent a)) | r}
backdropComponent = unsafeCoerce


instance writeForeignInternalJSX :: WriteForeign InternalJSX where writeImpl = unsafeToForeign
instance writeForeignInternalEventHandler :: WriteForeign InternalEventHandler where writeImpl = unsafeToForeign
instance writeForeignComponentFn :: WriteForeign (ComponentFn a) where writeImpl = unsafeToForeign
instance writeForeignComponent :: WriteForeign (InternalComponent a) where writeImpl = unsafeToForeign