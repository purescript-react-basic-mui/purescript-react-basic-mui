module React.Basic.MUI.MobileStepper where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.LinearProgress (LinearProgressProps)
import React.Basic.DOM.Internal (CSS)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.Events (EventHandler)

type MobileStepperProps_required optional =
  ( backButton :: Foreign
  , nextButton :: Foreign
  , steps :: Number
  | optional )

type MobileStepperProps_optional =
  ( activeStep :: Number
  , "LinearProgressProps" :: Foreign
  , position :: Foreign
  , variant :: Foreign
  , square :: Boolean
  , hidden :: Boolean
  , style :: CSS
  , title :: String
  , dir :: String
  , slot :: String
  , color :: String
  , ref :: Foreign
  , children :: JSX
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
  , role :: String
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
  , component :: JSX
  , elevation :: Number
  , innerRef :: Foreign
  , classes :: Foreign
  )

foreign import data MobileStepperProps :: Type 

mobileStepperProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (MobileStepperProps_optional)
  => Record (MobileStepperProps_required attrs)
  -> MobileStepperProps
mobileStepperProps = unsafeCoerce

type MobileStepperClassKey = Foreign

mobileStepper
  :: ∀ attrs attrs_
   . Union attrs attrs_ (MobileStepperProps_optional)
  => Record (MobileStepperProps_required attrs)
  -> JSX
mobileStepper = element _MobileStepper
foreign import _MobileStepper :: forall a. ReactComponent a 