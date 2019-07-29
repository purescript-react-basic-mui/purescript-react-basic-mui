module MUI.Core.TextField where

import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic.DOM (Props_div)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX, ReactComponent, Ref, element)
import Unsafe.Coerce (unsafeCoerce)

type TextFieldProps componentProps =
  ( autoComplete :: String
  , autoFocus :: Boolean
  , classes :: TextFieldClassKey
  , component :: ReactComponent { | componentProps }
  , defaultValue :: String
  , disabled :: Boolean
  , error :: Boolean
  , "FormHelperTextProps" :: Object Foreign
  , fullWidth :: Boolean
  , helperText :: JSX
  , id :: String
  , "InputLabelProps" :: Object Foreign
  , "InputProps" :: Object Foreign
  , inputProps :: Object Foreign
  , inputRef :: Effect (Ref Foreign)
  , label :: JSX
  , margin :: MarginProp
  , multiline :: Boolean
  , name :: String
  , onBlur :: EventHandler
  , onChange :: EventHandler
  , onFocus :: EventHandler
  , placeholder :: String
  , required :: Boolean
  , rows :: Number
  , rowsMax :: Number
  , select :: Boolean
  , "SelectProps" :: Object Foreign
  , type :: String
  , value :: String
  , variant :: VariantProp
  | componentProps
  )

foreign import data MarginProp :: Type
data Margin = None | Dense | Normal
margin :: Margin -> MarginProp
margin None = unsafeCoerce "none"
margin Dense = unsafeCoerce "dense"
margin Normal = unsafeCoerce "normal"

foreign import data VariantProp :: Type
data Variant = Standard | Outlined | Filled
variant :: Variant -> VariantProp
variant Standard = unsafeCoerce "standard"
variant Outlined = unsafeCoerce "outlines"
variant Filled = unsafeCoerce "filled"

foreign import data TextFieldClassKey :: Type
foreign import data TextFieldClassKeyJSS :: Type
foreign import data TextFieldPropsPartial :: Type

type TextFieldClassKeyOptionsJSS = TextFieldClassKeyOptionsR JSS
type TextFieldClassKeyOptions = TextFieldClassKeyOptionsR String
type TextFieldClassKeyOptionsR a = ( root :: a )

textFieldClassKey :: ∀ options options_
  . Union options options_ TextFieldClassKeyOptions
  => Record options
  -> TextFieldClassKey
textFieldClassKey = unsafeCoerce

textFieldClassKeyJSS :: ∀ options options_
  . Union options options_ TextFieldClassKeyOptionsJSS
  => Record options
  -> TextFieldClassKeyJSS
textFieldClassKeyJSS = unsafeCoerce

textFieldPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (TextFieldProps componentProps)
  => Record props 
  -> TextFieldPropsPartial 
textFieldPropsPartial_component = unsafeCoerce

textFieldPropsPartial :: ∀ props props_
  . Union props props_ (TextFieldProps Props_div)
  => Record props 
  -> TextFieldPropsPartial 
textFieldPropsPartial = unsafeCoerce

textField_component :: ∀ componentProps props props_
  . Union props props_ (TextFieldProps componentProps)
  => Record props 
  -> JSX
textField_component = element _TextField

textField :: ∀ props props_
  . Union props props_ (TextFieldProps Props_div)
  => Record props 
  -> JSX
textField = element _TextField

foreign import _TextField :: ∀ a. ReactComponent a
