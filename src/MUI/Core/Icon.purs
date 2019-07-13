module MUI.Core.Icon where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type IconProps =
  ( children :: Maybe (Array JSX)
  , classes :: IconClassKey
  , color :: Maybe String
  , component :: Maybe String
  , fontSize :: Maybe String
  )

iconProps :: { | IconProps }
iconProps = 
  { children : Nothing
  , classes
  , color : Just "inherit"
  , component : Just "span"
  , fontSize : Just "default"
  }

type IconClassKey =
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

classes :: IconClassKey
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

icon :: { | IconProps } -> JSX
icon props = do
  let foreignProps = write $ toInternalChildren props
  element _Icon (unsafeCoerce foreignProps)


foreign import _Icon :: ∀ a. ReactComponent a