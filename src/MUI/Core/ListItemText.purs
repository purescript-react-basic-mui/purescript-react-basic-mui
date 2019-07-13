module MUI.Core.ListItemText where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import MUI.Core (JSS)
import MUI.Core.Internal (primary, secondary, toInternalChildren)
import MUI.Core.Typography (TypographyProps, propsToForeign)
import React.Basic (JSX, ReactComponent, element)
import Record as Record
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type ListItemTextProps =
  ( children :: Maybe (Array JSX)
  , classes :: ListItemTextClassKey
  , disableTypography :: Maybe Boolean
  , inset :: Maybe Boolean
  , primary :: Maybe JSX
  , primaryTypographyProps :: Maybe ({ | TypographyProps })
  , secondary :: Maybe JSX
  , secondaryTypographyProps :: Maybe ({ | TypographyProps })
  )

type ListItemTextClassKey =
  { root :: Maybe JSS
  , multiline :: Maybe JSS
  , dense :: Maybe JSS
  , inset :: Maybe JSS
  , primary :: Maybe JSS
  , secondary :: Maybe JSS
  }


classes :: ListItemTextClassKey
classes =
  { root : Nothing
  , multiline : Nothing
  , dense : Nothing
  , inset : Nothing
  , primary : Nothing
  , secondary : Nothing
  }


listItemTextProps :: { | ListItemTextProps }
listItemTextProps = 
  { classes
  , children : Nothing
  , disableTypography : Just false
  , inset : Just false
  , primary : Nothing
  , primaryTypographyProps : Nothing
  , secondary : Nothing
  , secondaryTypographyProps : Nothing
  }

listItemIcon :: { | ListItemTextProps } -> JSX
listItemIcon props = do
  let foreignPrimaryTypographyProps = propsToForeign <$> props.primaryTypographyProps
      foreignSecondaryTypographyProps = propsToForeign <$> props.secondaryTypographyProps
      foreignProps = (secondary <<< primary <<< toInternalChildren) props
      newProps = Record.set (SProxy :: SProxy "primaryTypographyProps") foreignPrimaryTypographyProps
                   $ Record.set (SProxy :: SProxy "secondaryTypographyProps") foreignSecondaryTypographyProps foreignProps
  element _ListItemText (unsafeCoerce $ write newProps)



foreign import _ListItemText :: ∀ a. ReactComponent a