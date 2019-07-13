module MUI.Core.Divider where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core (JSS)
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type DividerProps =
  ( absolute :: Maybe Boolean
  , children :: Maybe (Array JSX)
  , classes :: DividerClassKey 
  , className :: Maybe String
  , component :: Maybe String
  , light :: Maybe Boolean
  , variant :: Maybe String
  )


type DividerClassKey =
  { root :: Maybe JSS
  , absolute :: Maybe JSS
  , inset :: Maybe JSS
  , light :: Maybe JSS
  , middle :: Maybe JSS
  }

classes :: DividerClassKey
classes =
  { root : Nothing
  , absolute : Nothing
  , inset : Nothing
  , light : Nothing
  , middle : Nothing
  }



dividerProps :: { | DividerProps }
dividerProps = 
  { absolute : Just false
  , children : Nothing
  , classes
  , className : Nothing
  , component : Just "hr"
  , light : Just false
  , variant : Just "fullWidth"
  }

divider :: { | DividerProps } -> JSX
divider props = element _Divider (unsafeCoerce $ write $ toInternalChildren props)


foreign import _Divider :: ∀ a. ReactComponent a