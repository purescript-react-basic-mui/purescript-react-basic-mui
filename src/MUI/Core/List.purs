module MUI.Core.List where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core (JSS)
import MUI.Core.Internal (subheader, toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type ListProps =
  ( children :: Maybe (Array JSX)
  , classes :: ListClassKey
  , component :: Maybe String
  , dense :: Maybe Boolean
  , disablePadding :: Maybe Boolean
  , subheader :: Maybe JSX
  )

type ListClassKey =
  { root :: Maybe JSS
  , padding :: Maybe JSS
  , dense :: Maybe JSS
  , subheader :: Maybe JSS
  }

classes :: ListClassKey
classes = 
  { root : Nothing
  , padding : Nothing
  , dense : Nothing
  , subheader : Nothing
  }

listProps :: { | ListProps }
listProps = 
  { children : Nothing
  , classes
  , component : Just "ul"
  , dense : Just false
  , disablePadding : Just false
  , subheader : Nothing
  }

list :: { | ListProps } -> JSX
list props = do
  let foreignProps = write
        $ (subheader <<< toInternalChildren) 
        $ props
  element _List (unsafeCoerce foreignProps)


foreign import _List :: ∀ a. ReactComponent a