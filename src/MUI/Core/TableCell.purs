module MUI.Core.TableCell where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TableCellProps =
  ( align :: Maybe String
  , children :: Maybe (Array JSX)
  , classes :: TableCellClassKey
  , component :: Maybe String
  , padding :: Maybe String
  , scope :: Maybe String
  , size :: Maybe String
  , sortDirection :: Maybe String
  , variant :: Maybe String
  )

tableCellProps :: { | TableCellProps }
tableCellProps =
  { align : Just "inherit"
  , children : Nothing
  , classes
  , component : Nothing
  , padding : Nothing
  , scope : Nothing
  , size : Nothing
  , sortDirection : Nothing
  , variant : Nothing
  }

type TableCellClassKey =
  { root :: Maybe String
  , head :: Maybe String
  , body :: Maybe String
  , footer :: Maybe String
  , sizeSmall :: Maybe String
  , paddingCheckbox :: Maybe String
  , paddingNone :: Maybe String
  , alignLeft :: Maybe String
  , alignCenter :: Maybe String
  , alignRight :: Maybe String
  , alignJustify :: Maybe String
  }

classes :: TableCellClassKey
classes =
  { root : Nothing
  , head : Nothing
  , body : Nothing
  , footer : Nothing
  , sizeSmall : Nothing
  , paddingCheckbox : Nothing
  , paddingNone : Nothing
  , alignLeft : Nothing
  , alignCenter : Nothing
  , alignRight : Nothing
  , alignJustify : Nothing
  }

tableCell :: { | TableCellProps } -> JSX
tableCell props = do
  let foreignProps = write $ toInternalChildren props
  element _TableCell (unsafeCoerce foreignProps)


foreign import  _TableCell :: ∀ a. ReactComponent a
