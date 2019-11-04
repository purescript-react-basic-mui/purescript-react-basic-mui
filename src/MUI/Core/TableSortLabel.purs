module MUI.Core.TableSortLabel where

import Foreign (Foreign)
import MUI.Core (JSS)
import MUI.Core.ButtonBase (ButtonBaseActions)
import MUI.Core.ButtonBase as ButtonBase
import MUI.Core.SvgIcon (SvgIconProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_span)
import React.Basic.DOM.Internal (SharedSVGProps)
import React.Basic.DOM.SVG (Props_svg)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Ref)
import Unsafe.Coerce (unsafeCoerce)

type TableSortLabelProps componentProps =
  ( active :: Boolean
  , children :: Array JSX
  , classes :: TableSortLabelClassKey
  , direction :: DirectionProp
  , hideSortIcon :: Boolean
  , "IconComponent" :: ReactComponent { | (SvgIconProps (SharedSVGProps Props_svg)) }
  , action :: Ref ButtonBaseActions
  , buttonRef :: Ref Foreign
  , centerRipple :: Boolean
  , component :: ReactComponent { | componentProps }
  , disabled :: Boolean
  , disableRipple :: Boolean
  , disableTouchRipple :: Boolean
  , focusRipple :: Boolean
  , focusVisibleClassName :: String
  , onFocusVisible :: EventHandler
  , "TouchRippleProps" :: Foreign
  , type :: ButtonBase.ButtonBaseTypeProp
  | componentProps
  )

foreign import data DirectionProp :: Type
data Direction = Asc | Desc
direction :: Direction -> DirectionProp
direction Asc = unsafeCoerce "asc"
direction Desc = unsafeCoerce "desc"

foreign import data TableSortLabelClassKey :: Type
foreign import data TableSortLabelClassKeyJSS :: Type
foreign import data TableSortLabelPropsPartial :: Type

type TableSortLabelClassKeyOptionsJSS = TableSortLabelClassKeyOptionsR JSS
type TableSortLabelClassKeyOptions = TableSortLabelClassKeyOptionsR String
type TableSortLabelClassKeyOptionsR a =
  ( root :: a
  , active :: a
  , icon :: a
  , iconDirectionDesc :: a
  , iconDirectionAsc :: a
  )

tableSortLabelClassKey :: ∀ options options_
  .  Union options options_ TableSortLabelClassKeyOptions
  => Record options
  -> TableSortLabelClassKey
tableSortLabelClassKey = unsafeCoerce

tableSortLabelClassKeyJSS :: ∀ options options_
  .  Union options options_ TableSortLabelClassKeyOptionsJSS
  => Record options
  -> TableSortLabelClassKeyJSS
tableSortLabelClassKeyJSS = unsafeCoerce

tableSortLabelPropsPartial_component :: ∀ componentProps props props_
  .  Union props props_ (TableSortLabelProps componentProps)
  => Record props 
  -> TableSortLabelPropsPartial 
tableSortLabelPropsPartial_component = unsafeCoerce

tableSortLabelPropsPartial :: ∀ props props_
  .  Union props props_ (TableSortLabelProps Props_span)
  => Record props 
  -> TableSortLabelPropsPartial 
tableSortLabelPropsPartial = unsafeCoerce

tableSortLabel_component :: ∀ componentProps props props_
  .  Union props props_ (TableSortLabelProps componentProps)
  => Record props 
  -> JSX
tableSortLabel_component = element _TableSortLabel

tableSortLabel :: ∀ props props_
  .  Union props props_ (TableSortLabelProps Props_span)
  => Record props 
  -> JSX
tableSortLabel = element _TableSortLabel

foreign import  _TableSortLabel :: ∀ a. ReactComponent a
