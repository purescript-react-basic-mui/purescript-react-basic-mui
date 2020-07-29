module MUI.Core.Styles.CreatePalette where

import Prelude
import Foreign (Foreign, unsafeToForeign)
import MUI.Core (Color) as Core
import MUI.Core.Colors.Common (CommonColors)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type PaletteColorPartial
  = ( light :: String
    , main :: String
    , dark :: String
    , contrastText :: String
    )

type PaletteColor
  = Record PaletteColorPartial

foreign import data PaletteColorOptions :: Type

type TypeBackgroundPartial
  = ( default :: String
    , paper :: String
    )

foreign import data TypeBackgroundOptions :: Type

type TypeBackground
  = Record TypeBackgroundPartial

type TypeTextPartial
  = ( primary :: String
    , secondary :: String
    , disabled :: String
    , hint :: String
    )

foreign import data TypeTextOptions :: Type

type TypeText
  = Record TypeTextPartial

type TypeActionPartial
  = ( active :: String
    , hover :: String
    , hoverOpacity :: Number
    , selected :: String
    , disabled :: String
    , disabledBackground :: String
    )

foreign import data TypeActionOptions :: Type

type TypeAction
  = Record TypeActionPartial

type PalettePartial
  = ( primary :: PaletteColorOptions
    , secondary :: PaletteColorOptions
    , error :: PaletteColorOptions
    , type :: String
    , tonalOffset :: Number
    , contrastThreshold :: Number
    , common :: CommonColors
    , grey :: Core.Color
    , text :: TypeTextOptions
    , divider :: String
    , action :: TypeActionOptions
    , background :: TypeBackgroundOptions
    , getContrastText :: String -> String
    )

foreign import data PaletteOptions :: Type

type Palette
  = { common :: CommonColors
    , type :: String
    , contrastThreshold :: Number
    , tonalOffset :: Number
    , primary :: PaletteColor
    , secondary :: PaletteColor
    , error :: PaletteColor
    , grey :: Core.Color
    , text :: TypeText
    , divider :: String
    , action :: TypeAction
    , background :: TypeBackground
    , getContrastText :: String -> String
    , augmentColor :: PaletteColorOptions -> PaletteColor
    }

paletteOptions ::
  ∀ options options_.
  Union options options_ PalettePartial =>
  Record options ->
  PaletteOptions
paletteOptions = unsafeCoerce

createPalette ::
  ∀ options options_.
  Union options options_ PalettePartial =>
  Record options ->
  Palette
createPalette = _createPalette <<< unsafeToForeign

paletteColorOptions ::
  ∀ options options_.
  Union options options_ PaletteColorPartial =>
  Record options ->
  PaletteColorOptions
paletteColorOptions = unsafeCoerce

typeBackgroundOptions ::
  ∀ options options_.
  Union options options_ TypeBackgroundPartial =>
  Record options ->
  TypeBackgroundOptions
typeBackgroundOptions = unsafeCoerce

typeActionOptions ::
  ∀ options options_.
  Union options options_ TypeTextPartial =>
  Record options ->
  TypeTextOptions
typeActionOptions = unsafeCoerce

typeTextOptions ::
  ∀ options options_.
  Union options options_ TypeTextPartial =>
  Record options ->
  TypeTextOptions
typeTextOptions = unsafeCoerce

foreign import _createPalette :: Foreign -> Palette
