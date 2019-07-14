module MUI.Core.TableHead where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TableHeadProps =
  ( children :: Maybe (Array JSX)
  , classes :: TableHeadClassKey
  , component :: Maybe String
  )

type TableHeadClassKey = { root :: Maybe String }

tableHeadProps :: { | TableHeadProps }
tableHeadProps = 
  { children : Nothing
  , classes
  , component : Just "thead"
  }

classes :: TableHeadClassKey
classes = { root : Nothing }

tableHead :: { | TableHeadProps } -> JSX
tableHead props = do
  let foreignProps = write $ toInternalChildren props
  element _TableHead (unsafeCoerce foreignProps)


foreign import  _TableHead :: ∀ a. ReactComponent a
