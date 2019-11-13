module MUI.Core.Slider where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_span) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data ValueLabelDisplay :: Type

valueLabelDisplay :: { auto :: ValueLabelDisplay, off :: ValueLabelDisplay, on :: ValueLabelDisplay }
valueLabelDisplay = { auto: Unsafe.Coerce.unsafeCoerce "auto", off: Unsafe.Coerce.unsafeCoerce "off", on: Unsafe.Coerce.unsafeCoerce "on" }

foreign import data Track :: Type

track :: { "false" :: Track, inverted :: Track, normal :: Track }
track = { "false": Unsafe.Coerce.unsafeCoerce false, inverted: Unsafe.Coerce.unsafeCoerce "inverted", normal: Unsafe.Coerce.unsafeCoerce "normal" }

foreign import data Orientation :: Type

orientation :: { horizontal :: Orientation, vertical :: Orientation }
orientation = { horizontal: Unsafe.Coerce.unsafeCoerce "horizontal", vertical: Unsafe.Coerce.unsafeCoerce "vertical" }

foreign import data Color :: Type

color :: { primary :: Color, secondary :: Color }
color = { primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqOrientation :: Eq Orientation where
  eq = Unsafe.Reference.unsafeRefEq

instance eqTrack :: Eq Track where
  eq = Unsafe.Reference.unsafeRefEq

instance eqValueLabelDisplay :: Eq ValueLabelDisplay where
  eq = Unsafe.Reference.unsafeRefEq

type SliderPropsOptions componentProps = ( "aria-label" :: String, "aria-labelledby" :: String, "aria-valuetext" :: String, children :: Array React.Basic.JSX, classes :: SliderClassKey, color :: Color, defaultValue :: Foreign.Foreign, disabled :: Boolean, getAriaLabel :: Foreign.Foreign, getAriaValueText :: Foreign.Foreign, marks :: Foreign.Foreign, max :: Number, min :: Number, name :: String, onChange :: React.Basic.Events.EventHandler, onChangeCommitted :: React.Basic.Events.EventHandler, orientation :: Orientation, step :: Number, track :: Track, value :: Foreign.Foreign, valueLabelDisplay :: ValueLabelDisplay, valueLabelFormat :: Foreign.Foreign | componentProps )

foreign import data SliderProps :: Type

foreign import data SliderPropsPartial :: Type

sliderPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (SliderPropsOptions React.Basic.DOM.Props_span) => Record options -> SliderPropsPartial
sliderPropsPartial = Unsafe.Coerce.unsafeCoerce

type SliderClassKeyGenericOptions a = ( colorPrimary :: a, colorSecondary :: a, disabled :: a, mark :: a, markActive :: a, markLabel :: a, markLabelActive :: a, marked :: a, rail :: a, root :: a, rtl :: a, thumb :: a, thumbColorPrimary :: a, thumbColorSecondary :: a, track :: a, trackFalse :: a, trackInverted :: a, valueLabel :: a, vertical :: a )

type SliderClassKeyOptions  = SliderClassKeyGenericOptions String

foreign import data SliderClassKey :: Type

sliderClassKey :: ∀ required given. Prim.Row.Union given required SliderClassKeyOptions => Record given -> SliderClassKey
sliderClassKey = Unsafe.Coerce.unsafeCoerce

type SliderClassKeyOptionsJSS  = SliderClassKeyGenericOptions MUI.Core.JSS

foreign import data SliderClassKeyJSS :: Type

sliderClassKeyJSS :: ∀ required given. Prim.Row.Union given required SliderClassKeyOptionsJSS => Record given -> SliderClassKeyJSS
sliderClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Slider :: ∀ a. React.Basic.ReactComponent a

slider :: ∀ required given. Prim.Row.Union given required (SliderPropsOptions React.Basic.DOM.Props_span) => Record given -> React.Basic.JSX
slider = React.Basic.element _Slider

slider_component :: ∀ required given componentProps. Prim.Row.Union given required (SliderPropsOptions componentProps) => Record given -> React.Basic.JSX
slider_component = React.Basic.element _Slider

sliderWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (SliderPropsOptions React.Basic.DOM.Props_span) => Prim.Row.Union jss jss_ SliderClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
sliderWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Slider)