module MUI.Core.Table where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TableProps =
  ( children :: Maybe (Array JSX)
  , classes :: TableClassKey
  , component :: Maybe String
  , padding :: Maybe String
  , size :: Maybe String
  )

type TableClassKey = { root :: Maybe String }

tableProps :: { | TableProps }
tableProps = 
  { children : Nothing
  , classes
  , component : Just "table"
  , padding : Just "default"
  , size : Just "medium"
  }

classes :: TableClassKey
classes = { root : Nothing }

table :: { | TableProps } -> JSX
table props = do
  let foreignProps = write $ toInternalChildren props
  element _Table (unsafeCoerce foreignProps)


foreign import  _Table :: ∀ a. ReactComponent a