module MUI.Core.TableSortLabel where


import Foreign (Foreign)
import MUI.Core.ButtonBase (ButtonBaseActions, TouchRippleProps)
import MUI.Core.SvgIcon (SvgIconProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Ref)
import Unsafe.Coerce (unsafeCoerce)

type TableSortLabelProps =
  ( active :: Boolean
  , children :: Array JSX
  , classes :: TableSortLabelClassKey
  , direction :: String
  , hideSortIcon :: Boolean
  , "IconComponent" :: ReactComponent { | SvgIconProps }
  , action :: Ref ButtonBaseActions
  , buttonRef :: Ref Foreign
  , centerRipple :: Boolean
  , component :: String
  , disabled :: Boolean
  , disableRipple :: Boolean
  , disableTouchRipple :: Boolean
  , focusRipple :: Boolean
  , focusVisibleClassName :: String
  , onFocusVisible :: EventHandler
  , "TouchRippleProps" :: TouchRippleProps
  , type :: String
  )

foreign import data TableSortLabelClassKey :: Type

type TableSortLabelClassKeyOptions =
  ( root :: String
  , active :: String
  , icon :: String
  , iconDirectionDesc :: String
  , iconDirectionAsc :: String
  )

classes 
  :: ∀ options options_
  . Union options options_ TableSortLabelClassKeyOptions
  => Record options
  -> TableSortLabelClassKey
classes = unsafeCoerce

tableSortLabel
  :: ∀ props props_
  . Union props props_ TableSortLabelProps
  => Record props 
  -> JSX
tableSortLabel = element _TableSortLabel

foreign import  _TableSortLabel :: ∀ a. ReactComponent a