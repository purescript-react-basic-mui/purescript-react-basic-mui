module MUI.Core.Grid where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core (JSS)
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type GridProps =
  ( alignContent :: Maybe String
  , alignItems :: Maybe String
  , children :: Maybe (Array JSX)
  , className :: Maybe String
  , classes :: GridClassKey
  , component :: Maybe String
  , container :: Maybe Boolean
  , direction :: Maybe String
  , item :: Maybe Boolean
  , justify :: Maybe String
  , lg :: Maybe String
  , md :: Maybe String
  , sm :: Maybe String
  , spacing :: Maybe Int
  , wrap :: Maybe String
  , xl :: Maybe String
  , xs :: Maybe String
  , zeroMinWidth :: Maybe Boolean
  )

gridProps :: { | GridProps }
gridProps =
  { alignContent : Just "stretch"
  , alignItems : Just "stretch"
  , children : Nothing
  , className : Nothing
  , classes
  , component : Just "div"
  , container : Just false
  , direction : Just "row"
  , item : Just false
  , justify : Just "flex-string"
  , lg : Just "false"
  , md : Just "false"
  , sm : Just "false"
  , spacing : Just 0
  , wrap : Just "wrap"
  , xl : Just "false"
  , xs : Just "false"
  , zeroMinWidth : Just false
  }

type GridClassKey =
  { container :: Maybe JSS
  , item :: Maybe JSS
  , "direction-xs-column" :: Maybe JSS
  , "direction-xs-column-reverse" :: Maybe JSS
  , "direction-xs-row-reverse" :: Maybe JSS
  , "wrap-xs-nowrap" :: Maybe JSS
  , "wrap-xs-wrap-reverse" :: Maybe JSS
  , "align-items-xs-center" :: Maybe JSS
  , "align-items-xs-flex-start" :: Maybe JSS
  , "align-items-xs-flex-end" :: Maybe JSS
  , "align-items-xs-baseline" :: Maybe JSS
  , "align-content-xs-center" :: Maybe JSS
  , "align-content-xs-flex-start" :: Maybe JSS
  , "align-content-xs-flex-end" :: Maybe JSS
  , "align-content-xs-space-between" :: Maybe JSS
  , "align-content-xs-space-around" :: Maybe JSS
  , "justify-xs-center" :: Maybe JSS
  , "justify-xs-flex-end" :: Maybe JSS
  , "justify-xs-space-between" :: Maybe JSS
  , "justify-xs-space-around" :: Maybe JSS
  , "spacing-xs-1" :: Maybe JSS
  , "spacing-xs-2" :: Maybe JSS
  , "spacing-xs-3" :: Maybe JSS
  , "spacing-xs-4" :: Maybe JSS
  , "spacing-xs-5" :: Maybe JSS
  , "spacing-xs-6" :: Maybe JSS
  , "spacing-xs-7" :: Maybe JSS
  , "spacing-xs-8" :: Maybe JSS
  , "spacing-xs-9" :: Maybe JSS
  , "spacing-xs-10" :: Maybe JSS
  , "grid-xs-auto" :: Maybe JSS
  , "grid-xs-true" :: Maybe JSS
  , "grid-xs-1" :: Maybe JSS
  , "grid-xs-2" :: Maybe JSS
  , "grid-xs-3" :: Maybe JSS
  , "grid-xs-4" :: Maybe JSS
  , "grid-xs-5" :: Maybe JSS
  , "grid-xs-6" :: Maybe JSS
  , "grid-xs-7" :: Maybe JSS
  , "grid-xs-8" :: Maybe JSS
  , "grid-xs-9" :: Maybe JSS
  , "grid-xs-10" :: Maybe JSS
  , "grid-xs-11" :: Maybe JSS
  , "grid-xs-12" :: Maybe JSS
  }

classes :: GridClassKey
classes = 
  { container : Nothing
  , item : Nothing
  , "direction-xs-column" : Nothing
  , "direction-xs-column-reverse" : Nothing
  , "direction-xs-row-reverse" : Nothing
  , "wrap-xs-nowrap" : Nothing
  , "wrap-xs-wrap-reverse" : Nothing
  , "align-items-xs-center" : Nothing
  , "align-items-xs-flex-start" : Nothing
  , "align-items-xs-flex-end" : Nothing
  , "align-items-xs-baseline" : Nothing
  , "align-content-xs-center" : Nothing
  , "align-content-xs-flex-start" : Nothing
  , "align-content-xs-flex-end" : Nothing
  , "align-content-xs-space-between" : Nothing
  , "align-content-xs-space-around" : Nothing
  , "justify-xs-center" : Nothing
  , "justify-xs-flex-end" : Nothing
  , "justify-xs-space-between" : Nothing
  , "justify-xs-space-around" : Nothing
  , "spacing-xs-1" : Nothing
  , "spacing-xs-2" : Nothing
  , "spacing-xs-3" : Nothing
  , "spacing-xs-4" : Nothing
  , "spacing-xs-5" : Nothing
  , "spacing-xs-6" : Nothing
  , "spacing-xs-7" : Nothing
  , "spacing-xs-8" : Nothing
  , "spacing-xs-9" : Nothing
  , "spacing-xs-10" : Nothing
  , "grid-xs-auto" : Nothing
  , "grid-xs-true" : Nothing
  , "grid-xs-1" : Nothing
  , "grid-xs-2" : Nothing
  , "grid-xs-3" : Nothing
  , "grid-xs-4" : Nothing
  , "grid-xs-5" : Nothing
  , "grid-xs-6" : Nothing
  , "grid-xs-7" : Nothing
  , "grid-xs-8" : Nothing
  , "grid-xs-9" : Nothing
  , "grid-xs-10" : Nothing
  , "grid-xs-11" : Nothing
  , "grid-xs-12" : Nothing
  }

grid :: { | GridProps } -> JSX
grid props = element _Grid (unsafeCoerce $ write $ toInternalChildren props)

foreign import _Grid :: ∀ a. ReactComponent a
