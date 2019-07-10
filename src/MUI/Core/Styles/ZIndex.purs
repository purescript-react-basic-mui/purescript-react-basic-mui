module MUI.Core.Styles.ZIndex where

import Data.Maybe (Maybe(..))

type ZIndex =
  { mobileStepper :: Number
  , appBar :: Number
  , drawer :: Number
  , modal :: Number
  , snackbar :: Number
  , tooltip :: Number
  }

type ZIndexOptions =
  { mobileStepper :: Maybe Number
  , appBar :: Maybe Number
  , drawer :: Maybe Number
  , modal :: Maybe Number
  , snackbar :: Maybe Number
  , tooltip :: Maybe Number
  }

zIndexOptions :: ZIndexOptions
zIndexOptions = 
  { mobileStepper : Nothing
  , appBar : Nothing
  , drawer : Nothing
  , modal : Nothing
  , snackbar : Nothing
  , tooltip : Nothing
  }

foreign import zIndex :: ZIndex