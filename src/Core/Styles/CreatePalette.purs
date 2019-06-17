module React.Basic.MUI.Core.Styles.CreatePalette where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Index (PaletteType)

type ColorPartial = Foreign

type TypeText_required  optional =
  ( primary :: String
  , secondary :: String
  , disabled :: String
  , hint :: String
  | optional )

type TypeText_optional =
  ( 
  )

foreign import data TypeText :: Type 



type TypeAction_required  optional =
  ( active :: String
  , hover :: String
  , hoverOpacity :: Number
  , selected :: String
  , disabled :: String
  , disabledBackground :: String
  | optional )

type TypeAction_optional =
  ( 
  )

foreign import data TypeAction :: Type 



type TypeBackground_required  optional =
  ( default :: String
  , paper :: String
  | optional )

type TypeBackground_optional =
  ( 
  )

foreign import data TypeBackground :: Type 



type TypeDivider = String

type PaletteColorOptions = Foreign

type SimplePaletteColorOptions_required  optional =
  ( main :: String
  | optional )

type SimplePaletteColorOptions_optional =
  ( light :: String
  , dark :: String
  , contrastText :: String
  )

foreign import data SimplePaletteColorOptions :: Type 



type PaletteColor_required  optional =
  ( light :: String
  , main :: String
  , dark :: String
  , contrastText :: String
  | optional )

type PaletteColor_optional =
  ( 
  )

foreign import data PaletteColor :: Type 



type TypeObject_required  optional =
  ( text :: TypeText 
  , action :: TypeAction 
  , divider :: TypeDivider 
  , background :: TypeBackground 
  | optional )

type TypeObject_optional =
  ( 
  )

foreign import data TypeObject :: Type 



light :: TypeObject 
light = _light
foreign import _light :: TypeObject 

dark :: TypeObject 
dark = _dark
foreign import _dark :: TypeObject 

type Palette_required  optional =
  ( common :: Foreign
  , type :: PaletteType 
  , contrastThreshold :: Number
  , tonalOffset :: Number
  , primary :: Foreign
  , secondary :: Foreign
  , error :: Foreign
  , grey :: Foreign
  , text :: TypeText 
  , divider :: TypeDivider 
  , action :: TypeAction 
  , background :: TypeBackground 
  , getContrastText :: Foreign
  , augmentColor :: { noname0  :: Foreign -> Foreign -> Foreign -> Foreign -> Foreign, noname1  :: Foreign -> Foreign }
  | optional )

type Palette_optional =
  ( 
  )

foreign import data Palette :: Type 



type PartialTypeObject = Foreign

type PaletteOptions_optional =
  ( primary :: Foreign
  , secondary :: Foreign
  , error :: Foreign
  , type :: PaletteType 
  , tonalOffset :: Number
  , contrastThreshold :: Number
  , common :: Foreign
  , grey :: Foreign
  , text :: Foreign
  , divider :: String
  , action :: Foreign
  , background :: Foreign
  , getContrastText :: Foreign
  )

foreign import data PaletteOptions :: Type 



createPalette :: Palette 
createPalette = _createPalette
foreign import _createPalette :: Palette 