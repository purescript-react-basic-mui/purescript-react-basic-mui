module MUI.Core.Styles.CreateMixins where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core (JSS, JSSToJSS)
import Simple.JSON (write)

type Mixins =
  { gutters :: JSS -> JSS
  , toolbar :: JSS 
  }

type MixinsOptions =
  { gutters :: Maybe JSSToJSS
  , toolbar :: Maybe JSS
  }

mixinsOptions :: MixinsOptions
mixinsOptions = 
  { gutters : Nothing
  , toolbar : Nothing
  }

createMixins :: MixinsOptions -> Mixins
createMixins = write >>> _createMixins

foreign import _createMixins :: Foreign -> Mixins