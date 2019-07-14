module MUI.Core.TableRow where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TableRowProps =
  ( children :: Maybe (Array JSX)
  , classes :: TableRowClassKey
  , component :: Maybe String
  , hover :: Maybe Boolean
  , selected :: Maybe Boolean
  )

type TableRowClassKey = 
  { root :: Maybe String
  , selected :: Maybe String
  , hover :: Maybe String
  , head :: Maybe String
  , footer :: Maybe String
  }

tableRowProps :: { | TableRowProps }
tableRowProps = 
  { children : Nothing
  , classes
  , component : Just "tr"
  , hover : Just false
  , selected : Just false
  }

classes :: TableRowClassKey
classes =
  { root : Nothing
  , selected : Nothing
  , hover : Nothing
  , head : Nothing
  , footer : Nothing
  }


tableRow :: { | TableRowProps } -> JSX
tableRow props = do
  let foreignProps = write $ toInternalChildren props
  element _TableRow (unsafeCoerce foreignProps)


foreign import  _TableRow :: ∀ a. ReactComponent a
