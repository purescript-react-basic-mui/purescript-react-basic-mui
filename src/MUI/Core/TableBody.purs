module MUI.Core.TableBody where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TableBodyProps =
  ( children :: Maybe (Array JSX)
  , classes :: TableBodyClassKey
  , component :: Maybe String
  )

type TableBodyClassKey = { root :: Maybe String }

tableBodyProps :: { | TableBodyProps }
tableBodyProps = 
  { children : Nothing
  , classes
  , component : Just "tbody"
  }

classes :: TableBodyClassKey
classes = { root : Nothing }

tableBody :: { | TableBodyProps } -> JSX
tableBody props = do
  let foreignProps = write $ toInternalChildren props
  element _TableBody (unsafeCoerce foreignProps)


foreign import  _TableBody :: ∀ a. ReactComponent a
