module MUI.Core.Tabs where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Prim.Row (class Union)
import React.Basic.DOM (Props_div)
import React.Basic.Events (SyntheticEvent)
import React.Basic.Hooks (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type TabsProps value componentProps =
  ( action :: TabActions
  , centered :: Boolean
  , children :: Array JSX
  , classes :: TabsClassKey
  , component :: ReactComponent { | componentProps }
  , indicatorColor :: String
  , onChange  :: EffectFn2 SyntheticEvent value Unit
  , orientation :: OrientationProp
  , "ScrollButtonComponent" :: JSX
  , scrollButtons :: ScrollButtonProp
  , "TabIndicatorProps" :: { | Props_div }
  , textColor :: String
  , value :: value
  , variant :: VariantProp
  | componentProps
  )

type TabActions = { updateIndicator :: Effect Unit }

foreign import data VariantProp :: Type
data Variant = Standard | Scrollable | FullWidth
variant :: Variant -> VariantProp
variant Standard = unsafeCoerce "standard"
variant Scrollable = unsafeCoerce "scrollable"
variant FullWidth = unsafeCoerce "fullWidth"
  
foreign import data OrientationProp :: Type
data Orientation = Horizontal | Vertical
orientation :: Orientation -> OrientationProp
orientation Horizontal = unsafeCoerce "horizontal"
orientation Vertical = unsafeCoerce "vertical"

foreign import data ScrollButtonProp :: Type
data ScrollButton = Auto | Desktop | On | Off
scrollButton :: ScrollButton -> ScrollButtonProp
scrollButton Auto = unsafeCoerce "auto"
scrollButton Desktop = unsafeCoerce "desktop"
scrollButton On = unsafeCoerce "on"
scrollButton Off = unsafeCoerce "off"

foreign import data TabsPropsPartial :: Type

type TabsClassKeyOptions =
  ( root :: String
  , vertical :: String
  , flexContainer :: String
  , flexContainerVertical :: String
  , centered :: String
  , scroller :: String
  , fixed :: String
  , scrollable :: String
  , scrollButtons :: String
  , scrollButtonsDesktop :: String
  , indicator :: String
  )

foreign import data TabsClassKey :: Type

tabsClassKey :: ∀ options options_
  . Union options options_ TabsClassKeyOptions
  => Record options
  -> TabsClassKey
tabsClassKey = unsafeCoerce

tabsPartial_component :: ∀ value componentProps props props_
  . Union props props_ (TabsProps value componentProps)
  => Record props 
  -> TabsPropsPartial
tabsPartial_component = unsafeCoerce

tabsPartial :: ∀ value props props_
  . Union props props_ (TabsProps value Props_div)
  => Record props 
  ->TabsPropsPartial
tabsPartial = unsafeCoerce


tabs_component :: ∀ value componentProps props props_
  . Union props props_ (TabsProps value componentProps)
  => Record props 
  -> JSX
tabs_component = element _Tabs

tabs :: ∀ value props props_
  . Union props props_ (TabsProps value Props_div)
  => Record props 
  -> JSX
tabs = element _Tabs


foreign import _Tabs :: ∀ a. ReactComponent a