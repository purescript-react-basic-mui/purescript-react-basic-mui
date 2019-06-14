module React.Basic.MUI.Popover where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Modal (ModalProps)
import React.Basic.MUI.Paper (PaperProps)
import React.Basic.MUI.Transitions.Transition (TransitionProps)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)
import React.Basic.MUI.Portal (PortalProps)
import React.Basic.MUI.Backdrop (BackdropProps)

type PopoverOrigin_required optional =
  ( horizontal :: Foreign
  , vertical :: Foreign
  | optional )

type PopoverOrigin_optional =
  ( 
  )

foreign import data PopoverOrigin :: Type 

popoverOrigin
  :: ∀ attrs attrs_
   . Union attrs attrs_ (PopoverOrigin_optional)
  => Record (PopoverOrigin_required attrs)
  -> PopoverOrigin
popoverOrigin = unsafeCoerce

type PopoverPosition_required optional =
  ( top :: Number
  , left :: Number
  | optional )

type PopoverPosition_optional =
  ( 
  )

foreign import data PopoverPosition :: Type 

popoverPosition
  :: ∀ attrs attrs_
   . Union attrs attrs_ (PopoverPosition_optional)
  => Record (PopoverPosition_required attrs)
  -> PopoverPosition
popoverPosition = unsafeCoerce

type PopoverReference = Foreign

type PopoverProps_required optional =
  ( open :: Boolean
  | optional )

type PopoverProps_optional =
  ( action :: Foreign
  , anchorEl :: Foreign
  , anchorOrigin :: PopoverOrigin 
  , anchorPosition :: PopoverPosition 
  , anchorReference :: PopoverReference 
  , children :: JSX
  , elevation :: Number
  , getContentAnchorEl :: Foreign
  , marginThreshold :: Number
  , modal :: Boolean
  , "ModalClasses" :: Foreign
  , "PaperProps" :: Foreign
  , role :: String
  , transformOrigin :: PopoverOrigin 
  , "TransitionComponent" :: JSX
  , transitionDuration :: Foreign
  , "TransitionProps" :: TransitionProps 
  , hidden :: Boolean
  , manifest :: String
  , style :: CSS
  , title :: String
  , dir :: String
  , slot :: String
  , color :: String
  , ref :: Foreign
  , defaultChecked :: Boolean
  , defaultValue :: Foreign
  , suppressContentEditableWarning :: Boolean
  , suppressHydrationWarning :: Boolean
  , accessKey :: String
  , className :: String
  , contentEditable :: Boolean
  , contextMenu :: String
  , draggable :: Boolean
  , id :: String
  , lang :: String
  , placeholder :: String
  , spellCheck :: Boolean
  , tabIndex :: Number
  , inputMode :: String
  , is :: String
  , radioGroup :: String
  , about :: String
  , datatype :: String
  , inlist :: Foreign
  , prefix :: String
  , property :: String
  , resource :: String
  , typeof :: String
  , vocab :: String
  , autoCapitalize :: String
  , autoCorrect :: String
  , autoSave :: String
  , itemProp :: String
  , itemScope :: Boolean
  , itemType :: String
  , itemID :: String
  , itemRef :: String
  , results :: Number
  , security :: String
  , unselectable :: Foreign
  , "aria-activedescendant" :: String
  , "aria-atomic" :: Foreign
  , "aria-autocomplete" :: Foreign
  , "aria-busy" :: Foreign
  , "aria-checked" :: Foreign
  , "aria-colcount" :: Number
  , "aria-colindex" :: Number
  , "aria-colspan" :: Number
  , "aria-controls" :: String
  , "aria-current" :: Foreign
  , "aria-describedby" :: String
  , "aria-details" :: String
  , "aria-disabled" :: Foreign
  , "aria-dropeffect" :: Foreign
  , "aria-errormessage" :: String
  , "aria-expanded" :: Foreign
  , "aria-flowto" :: String
  , "aria-grabbed" :: Foreign
  , "aria-haspopup" :: Foreign
  , "aria-hidden" :: Foreign
  , "aria-invalid" :: Foreign
  , "aria-keyshortcuts" :: String
  , "aria-label" :: String
  , "aria-labelledby" :: String
  , "aria-level" :: Number
  , "aria-live" :: Foreign
  , "aria-modal" :: Foreign
  , "aria-multiline" :: Foreign
  , "aria-multiselectable" :: Foreign
  , "aria-orientation" :: Foreign
  , "aria-owns" :: String
  , "aria-placeholder" :: String
  , "aria-posinset" :: Number
  , "aria-pressed" :: Foreign
  , "aria-readonly" :: Foreign
  , "aria-relevant" :: Foreign
  , "aria-required" :: Foreign
  , "aria-roledescription" :: String
  , "aria-rowcount" :: Number
  , "aria-rowindex" :: Number
  , "aria-rowspan" :: Number
  , "aria-selected" :: Foreign
  , "aria-setsize" :: Number
  , "aria-sort" :: Foreign
  , "aria-valuemax" :: Number
  , "aria-valuemin" :: Number
  , "aria-valuenow" :: Number
  , "aria-valuetext" :: String
  , dangerouslySetInnerHTML :: { __html :: String }
  , onCopy :: EventHandler
  , onCopyCapture :: EventHandler
  , onCut :: EventHandler
  , onCutCapture :: EventHandler
  , onPaste :: EventHandler
  , onPasteCapture :: EventHandler
  , onCompositionEnd :: EventHandler
  , onCompositionEndCapture :: EventHandler
  , onCompositionStart :: EventHandler
  , onCompositionStartCapture :: EventHandler
  , onCompositionUpdate :: EventHandler
  , onCompositionUpdateCapture :: EventHandler
  , onFocus :: EventHandler
  , onFocusCapture :: EventHandler
  , onBlur :: EventHandler
  , onBlurCapture :: EventHandler
  , onChange :: EventHandler
  , onChangeCapture :: EventHandler
  , onBeforeInput :: EventHandler
  , onBeforeInputCapture :: EventHandler
  , onInput :: EventHandler
  , onInputCapture :: EventHandler
  , onReset :: EventHandler
  , onResetCapture :: EventHandler
  , onSubmit :: EventHandler
  , onSubmitCapture :: EventHandler
  , onInvalid :: EventHandler
  , onInvalidCapture :: EventHandler
  , onLoad :: EventHandler
  , onLoadCapture :: EventHandler
  , onError :: EventHandler
  , onErrorCapture :: EventHandler
  , onKeyDown :: EventHandler
  , onKeyDownCapture :: EventHandler
  , onKeyPress :: EventHandler
  , onKeyPressCapture :: EventHandler
  , onKeyUp :: EventHandler
  , onKeyUpCapture :: EventHandler
  , onAbort :: EventHandler
  , onAbortCapture :: EventHandler
  , onCanPlay :: EventHandler
  , onCanPlayCapture :: EventHandler
  , onCanPlayThrough :: EventHandler
  , onCanPlayThroughCapture :: EventHandler
  , onDurationChange :: EventHandler
  , onDurationChangeCapture :: EventHandler
  , onEmptied :: EventHandler
  , onEmptiedCapture :: EventHandler
  , onEncrypted :: EventHandler
  , onEncryptedCapture :: EventHandler
  , onEnded :: EventHandler
  , onEndedCapture :: EventHandler
  , onLoadedData :: EventHandler
  , onLoadedDataCapture :: EventHandler
  , onLoadedMetadata :: EventHandler
  , onLoadedMetadataCapture :: EventHandler
  , onLoadStart :: EventHandler
  , onLoadStartCapture :: EventHandler
  , onPause :: EventHandler
  , onPauseCapture :: EventHandler
  , onPlay :: EventHandler
  , onPlayCapture :: EventHandler
  , onPlaying :: EventHandler
  , onPlayingCapture :: EventHandler
  , onProgress :: EventHandler
  , onProgressCapture :: EventHandler
  , onRateChange :: EventHandler
  , onRateChangeCapture :: EventHandler
  , onSeeked :: EventHandler
  , onSeekedCapture :: EventHandler
  , onSeeking :: EventHandler
  , onSeekingCapture :: EventHandler
  , onStalled :: EventHandler
  , onStalledCapture :: EventHandler
  , onSuspend :: EventHandler
  , onSuspendCapture :: EventHandler
  , onTimeUpdate :: EventHandler
  , onTimeUpdateCapture :: EventHandler
  , onVolumeChange :: EventHandler
  , onVolumeChangeCapture :: EventHandler
  , onWaiting :: EventHandler
  , onWaitingCapture :: EventHandler
  , onAuxClick :: EventHandler
  , onAuxClickCapture :: EventHandler
  , onClick :: EventHandler
  , onClickCapture :: EventHandler
  , onContextMenu :: EventHandler
  , onContextMenuCapture :: EventHandler
  , onDoubleClick :: EventHandler
  , onDoubleClickCapture :: EventHandler
  , onDrag :: EventHandler
  , onDragCapture :: EventHandler
  , onDragEnd :: EventHandler
  , onDragEndCapture :: EventHandler
  , onDragEnter :: EventHandler
  , onDragEnterCapture :: EventHandler
  , onDragExit :: EventHandler
  , onDragExitCapture :: EventHandler
  , onDragLeave :: EventHandler
  , onDragLeaveCapture :: EventHandler
  , onDragOver :: EventHandler
  , onDragOverCapture :: EventHandler
  , onDragStart :: EventHandler
  , onDragStartCapture :: EventHandler
  , onDrop :: EventHandler
  , onDropCapture :: EventHandler
  , onMouseDown :: EventHandler
  , onMouseDownCapture :: EventHandler
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , onMouseMove :: EventHandler
  , onMouseMoveCapture :: EventHandler
  , onMouseOut :: EventHandler
  , onMouseOutCapture :: EventHandler
  , onMouseOver :: EventHandler
  , onMouseOverCapture :: EventHandler
  , onMouseUp :: EventHandler
  , onMouseUpCapture :: EventHandler
  , onSelect :: EventHandler
  , onSelectCapture :: EventHandler
  , onTouchCancel :: EventHandler
  , onTouchCancelCapture :: EventHandler
  , onTouchEnd :: EventHandler
  , onTouchEndCapture :: EventHandler
  , onTouchMove :: EventHandler
  , onTouchMoveCapture :: EventHandler
  , onTouchStart :: EventHandler
  , onTouchStartCapture :: EventHandler
  , onPointerDown :: EventHandler
  , onPointerDownCapture :: EventHandler
  , onPointerMove :: EventHandler
  , onPointerMoveCapture :: EventHandler
  , onPointerUp :: EventHandler
  , onPointerUpCapture :: EventHandler
  , onPointerCancel :: EventHandler
  , onPointerCancelCapture :: EventHandler
  , onPointerEnter :: EventHandler
  , onPointerEnterCapture :: EventHandler
  , onPointerLeave :: EventHandler
  , onPointerLeaveCapture :: EventHandler
  , onPointerOver :: EventHandler
  , onPointerOverCapture :: EventHandler
  , onPointerOut :: EventHandler
  , onPointerOutCapture :: EventHandler
  , onGotPointerCapture :: EventHandler
  , onGotPointerCaptureCapture :: EventHandler
  , onLostPointerCapture :: EventHandler
  , onLostPointerCaptureCapture :: EventHandler
  , onScroll :: EventHandler
  , onScrollCapture :: EventHandler
  , onWheel :: EventHandler
  , onWheelCapture :: EventHandler
  , onAnimationStart :: EventHandler
  , onAnimationStartCapture :: EventHandler
  , onAnimationEnd :: EventHandler
  , onAnimationEndCapture :: EventHandler
  , onAnimationIteration :: EventHandler
  , onAnimationIterationCapture :: EventHandler
  , onTransitionEnd :: EventHandler
  , onTransitionEndCapture :: EventHandler
  , innerRef :: Foreign
  , onEnter :: EventHandler
  , onEntering :: EventHandler
  , onEntered :: EventHandler
  , onExit :: EventHandler
  , onExiting :: EventHandler
  , onExited :: EventHandler
  , container :: Foreign
  , "BackdropComponent" :: JSX
  , "BackdropProps" :: Foreign
  , closeAfterTransition :: Boolean
  , disableAutoFocus :: Boolean
  , disableBackdropClick :: Boolean
  , disableEnforceFocus :: Boolean
  , disableEscapeKeyDown :: Boolean
  , disablePortal :: Foreign
  , disableRestoreFocus :: Boolean
  , hideBackdrop :: Boolean
  , keepMounted :: Boolean
  , manager :: ModalManager 
  , onBackdropClick :: EventHandler
  , onClose :: Foreign
  , onEscapeKeyDown :: EventHandler
  , onRendered :: Foreign
  , classes :: Foreign
  )

foreign import data PopoverProps :: Type 

popoverProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (PopoverProps_optional)
  => Record (PopoverProps_required attrs)
  -> PopoverProps
popoverProps = unsafeCoerce

type PopoverClassKey = String

type PopoverActions_required optional =
  ( updatePosition :: Foreign
  | optional )

type PopoverActions_optional =
  ( 
  )

foreign import data PopoverActions :: Type 

popoverActions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (PopoverActions_optional)
  => Record (PopoverActions_required attrs)
  -> PopoverActions
popoverActions = unsafeCoerce

popover
  :: ∀ attrs attrs_
   . Union attrs attrs_ (PopoverProps_optional)
  => Record (PopoverProps_required attrs)
  -> JSX
popover = element _Popover
foreign import _Popover :: forall a. ReactComponent a 