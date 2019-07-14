module MUI.Core.TableFooter where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TableFooterProps =
  ( children :: Maybe (Array JSX)
  , classes :: TableFooterClassKey
  , component :: Maybe String
  )

type TableFooterClassKey = { root :: Maybe String }

tableFooterProps :: { | TableFooterProps }
tableFooterProps = 
  { children : Nothing
  , classes
  , component : Just "tfoot"
  }

classes :: TableFooterClassKey
classes = { root : Nothing }

tableFooter :: { | TableFooterProps } -> JSX
tableFooter props = do
  let foreignProps = write $ toInternalChildren props
  element _TableFooter (unsafeCoerce foreignProps)


foreign import  _TableFooter :: ∀ a. ReactComponent a
