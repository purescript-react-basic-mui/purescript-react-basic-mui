-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/styles/createPalette.d.ts
module MaterialUI.Basic.Styles.CreatePalette where 
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic.Events (EventHandler)
import React.Basic (ReactComponent)

import MaterialUI.Basic.Colors.Common (CommonColors)


foreign import data PaletteGrey :: Type

foreign import _createPalette :: forall a. ReactComponent a

type Palette  = {
    action :: TypeAction
  , augmentColor :: (Object Foreign)
  , background :: TypeBackground
  , common :: CommonColors
  , contrastThreshold :: Number
  , divider :: String
  , error :: PaletteColor
  , getContrastText :: EventHandler
  , grey :: PaletteGrey
  , primary :: PaletteColor
  , secondary :: PaletteColor
  , text :: TypeText
  , tonalOffset :: Number
  , type :: String
}

  
type PaletteColor  = {
    contrastText :: String
  , dark :: String
  , light :: String
  , main :: String
}

  
type TypeAction  = {
    active :: String
  , disabled :: String
  , disabledBackground :: String
  , hover :: String
  , hoverOpacity :: Number
  , selected :: String
}

  
type TypeBackground  = {
    default :: String
  , paper :: String
}

  
type TypeText  = {
    disabled :: String
  , hint :: String
  , primary :: String
  , secondary :: String
}

  
