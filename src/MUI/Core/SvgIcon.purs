module MUI.Core.SvgIcon where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type SvgIconProps =
  ( children :: Maybe (Array JSX)
  , classes :: SvgIconClassKey
  , color :: Maybe String
  , component :: Maybe String
  , fontSize :: Maybe String
  , htmlColor :: Maybe String
  , shapeRendering :: Maybe String
  , titleAccess :: Maybe String
  , viewBox :: Maybe String
  )

svgIconProps :: { | SvgIconProps }
svgIconProps = 
  { children : Nothing
  , classes
  , color : Nothing
  , component : Just "svg"
  , fontSize : Just "default"
  , htmlColor : Nothing
  , shapeRendering : Nothing
  , titleAccess : Nothing
  , viewBox : Just "0 0 24 24"
  }

type SvgIconClassKey =
  { root :: Maybe String
  , colorPrimary :: Maybe String
  , colorSecondary :: Maybe String
  , colorAction :: Maybe String
  , colorError :: Maybe String
  , colorDisabled :: Maybe String
  , fontSizeInherit :: Maybe String
  , fontSizeSmall :: Maybe String
  , fontSizeLarge :: Maybe String
  }

classes :: SvgIconClassKey
classes =
  { root : Nothing
  , colorPrimary : Nothing
  , colorSecondary : Nothing
  , colorAction : Nothing
  , colorError : Nothing
  , colorDisabled : Nothing
  , fontSizeInherit : Nothing
  , fontSizeSmall : Nothing
  , fontSizeLarge : Nothing
  }

svgIcon :: { | SvgIconProps } -> JSX
svgIcon props = do
  let foreignProps = write $ toInternalChildren props
  element _SvgIcon (unsafeCoerce foreignProps)

foreign import _SvgIcon :: ∀ a. ReactComponent a