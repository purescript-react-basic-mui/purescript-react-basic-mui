module MUI.Core.Link where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import MUI.Core.Typography (TypographyClassKey)
import MUI.Core.Typography as Typography
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type LinkProps =
  ( children :: Maybe (Array JSX)
  , classes :: LinkClassKey
  , color :: Maybe String
  , component :: Maybe String
  , "TypographyClasses" :: TypographyClassKey
  , underline :: Maybe String
  , variant :: Maybe String
  )

linkProps :: { | LinkProps }
linkProps = 
  { children : Nothing
  , classes
  , color : Just "primary"
  , component : Just "a"
  , "TypographyClasses" : Typography.classes
  , underline : Just "hover"
  , variant : Just "inherit"
  }

type LinkClassKey =
  { root :: Maybe String
  , underlineNone :: Maybe String
  , underlineHover :: Maybe String
  , underlineAlways :: Maybe String
  , button :: Maybe String
  , focusVisible :: Maybe String
  }

classes :: LinkClassKey
classes = 
  { root : Nothing
  , underlineNone : Nothing
  , underlineHover : Nothing
  , underlineAlways : Nothing
  , button : Nothing
  , focusVisible : Nothing
  }

link :: { | LinkProps } -> JSX
link props = do
  let foreignProps = write $ toInternalChildren props
  element _Link (unsafeCoerce foreignProps)


foreign import _Link :: ∀ a. ReactComponent a