module React.Basic.MUI.Styles.CreatePalette where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type ColorPartial = Foreign

type TypeText_required optional =
  ( primary :: String
  , secondary :: String
  , disabled :: String
  , hint :: String
  | optional )

type TypeText_optional =
  ( 
  )

foreign import data TypeText :: Type 

typeText
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TypeText_optional)
  => Record (TypeText_required attrs)
  -> TypeText
typeText = unsafeCoerce

type TypeAction_required optional =
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

typeAction
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TypeAction_optional)
  => Record (TypeAction_required attrs)
  -> TypeAction
typeAction = unsafeCoerce

type TypeBackground_required optional =
  ( default :: String
  , paper :: String
  | optional )

type TypeBackground_optional =
  ( 
  )

foreign import data TypeBackground :: Type 

typeBackground
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TypeBackground_optional)
  => Record (TypeBackground_required attrs)
  -> TypeBackground
typeBackground = unsafeCoerce

type TypeDivider = String

type PaletteColorOptions = Foreign

type SimplePaletteColorOptions_required optional =
  ( main :: String
  | optional )

type SimplePaletteColorOptions_optional =
  ( light :: String
  , dark :: String
  , contrastText :: String
  )

foreign import data SimplePaletteColorOptions :: Type 

simplePaletteColorOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (SimplePaletteColorOptions_optional)
  => Record (SimplePaletteColorOptions_required attrs)
  -> SimplePaletteColorOptions
simplePaletteColorOptions = unsafeCoerce

type PaletteColor_required optional =
  ( light :: String
  , main :: String
  , dark :: String
  , contrastText :: String
  | optional )

type PaletteColor_optional =
  ( 
  )

foreign import data PaletteColor :: Type 

paletteColor
  :: ∀ attrs attrs_
   . Union attrs attrs_ (PaletteColor_optional)
  => Record (PaletteColor_required attrs)
  -> PaletteColor
paletteColor = unsafeCoerce

type TypeObject_required optional =
  ( text :: TypeText 
  , action :: TypeAction 
  , divider :: TypeDivider 
  , background :: TypeBackground 
  | optional )

type TypeObject_optional =
  ( 
  )

foreign import data TypeObject :: Type 

typeObject
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TypeObject_optional)
  => Record (TypeObject_required attrs)
  -> TypeObject
typeObject = unsafeCoerce

light :: TypeObject 
light = _light
foreign import _light :: TypeObject 

dark :: TypeObject 
dark = _dark
foreign import _dark :: TypeObject 

type Palette_required optional =
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
  , augmentColor :: { (CallSignature { isOptional: true, name: Nothing, parameters: [(PropertySignature { isOptional: false, name: Nothing, type: (TypeReference { aliasName: (Just (Identifier "Partial")), aliasTypeArguments: (Just [(TypeReference { aliasName: Nothing, aliasTypeArguments: Nothing, fullyQualifiedName: Nothing, name: (Identifier "Color"), typeArguments: [] })]), fullyQualifiedName: (Just "__type"), name: (Identifier "ColorPartial"), typeArguments: [] }) }),(PropertySignature { isOptional: true, name: Nothing, type: (UnionType [NumberType,StringType]) }),(PropertySignature { isOptional: true, name: Nothing, type: (UnionType [NumberType,StringType]) }),(PropertySignature { isOptional: true, name: Nothing, type: (UnionType [NumberType,StringType]) })], returnType: (TypeReference { aliasName: Nothing, aliasTypeArguments: Nothing, fullyQualifiedName: (Just "@material-ui/core/styles/createPalette\".PaletteColor"), name: (Identifier "PaletteColor"), typeArguments: [] }), typeParameters: [] }), (CallSignature { isOptional: true, name: Nothing, parameters: [(PropertySignature { isOptional: false, name: Nothing, type: (TypeReference { aliasName: (Just (Identifier "PaletteColorOptions")), aliasTypeArguments: (Just []), fullyQualifiedName: Nothing, name: (Identifier "PaletteColorOptions"), typeArguments: [] }) })], returnType: (TypeReference { aliasName: Nothing, aliasTypeArguments: Nothing, fullyQualifiedName: (Just "@material-ui/core/styles/createPalette\".PaletteColor"), name: (Identifier "PaletteColor"), typeArguments: [] }), typeParameters: [] }) }
  | optional )

type Palette_optional =
  ( 
  )

foreign import data Palette :: Type 

palette
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Palette_optional)
  => Record (Palette_required attrs)
  -> Palette
palette = unsafeCoerce

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

paletteOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (PaletteOptions_optional)
  => Record (attrs)
  -> PaletteOptions
paletteOptions = unsafeCoerce

createPalette :: Palette 
createPalette = _createPalette
foreign import _createPalette :: Palette 