module MUI.Core.TableSortLabel where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Foreign (Foreign, unsafeToForeign)
import MUI.Core.ButtonBase (ButtonBaseActions, TouchRippleProps)
import MUI.Core.Internal (action, buttonRef, onFocusVisible, toInternalChildren)
import MUI.Core.SvgIcon (SvgIconProps)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Ref)
import Record as Record
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TableSortLabelProps =
  ( active :: Maybe Boolean
  , children :: Maybe (Array JSX)
  , classes :: TableSortLabelClassKey
  , direction :: Maybe String
  , hideSortIcon :: Maybe Boolean
  , "IconComponent" :: Maybe (ReactComponent { | SvgIconProps })
  , action :: Maybe (Ref ButtonBaseActions)
  , buttonRef :: Maybe (Ref Foreign)
  , centerRipple :: Maybe Boolean
  , component :: Maybe String
  , disabled :: Maybe Boolean
  , disableRipple :: Maybe Boolean
  , disableTouchRipple :: Maybe Boolean
  , focusRipple :: Maybe Boolean
  , focusVisibleClassName :: Maybe String
  , onFocusVisible :: Maybe EventHandler
  , "TouchRippleProps" :: Maybe TouchRippleProps
  , type :: Maybe String
  )

tableSortLabelProps :: { | TableSortLabelProps }
tableSortLabelProps =
  { active : Just false
  , direction : Just "desc"
  , hideSortIcon  : Just false
  , "IconComponent" : Nothing
  , action : Nothing
  , buttonRef : Nothing
  , centerRipple : Just false
  , children : Nothing
  , classes
  , component : Just "button"
  , disabled : Nothing
  , disableRipple : Just false
  , disableTouchRipple : Just false
  , focusRipple : Just false
  , focusVisibleClassName : Nothing
  , onFocusVisible : Nothing
  , "TouchRippleProps" : Nothing
  , type : Just "button"
  }



type TableSortLabelClassKey =
  { root :: Maybe String
  , active :: Maybe String
  , icon :: Maybe String
  , iconDirectionDesc :: Maybe String
  , iconDirectionAsc :: Maybe String
  }

classes :: TableSortLabelClassKey
classes =
  { root : Nothing
  , active : Nothing
  , icon : Nothing
  , iconDirectionDesc : Nothing
  , iconDirectionAsc : Nothing
  }

tableSortLabel :: { | TableSortLabelProps } -> JSX
tableSortLabel props = do
  let foreignProps = (action <<< buttonRef <<< onFocusVisible <<< toInternalChildren) props
      newProps = Record.set (SProxy :: SProxy "IconComponent") (unsafeToForeign <$> props."IconComponent") foreignProps
  element _TableSortLabel (unsafeCoerce $ write newProps)


foreign import  _TableSortLabel :: ∀ a. ReactComponent a