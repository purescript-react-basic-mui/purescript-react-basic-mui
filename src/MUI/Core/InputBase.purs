module MUI.Core.InputBase where

import Prelude

import Foreign (Foreign)
import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div, Props_input)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type InputBasePropsOptions componentProps value = 
  ( autoComplete :: String
  , autoFocus :: Boolean
  , classes :: InputBaseClassKey
  , className :: String
  , defaultValue :: value
  , disabled :: Boolean
  , endAdornment :: JSX
  , error :: Boolean
  , fullWidth :: Boolean
  , id :: String
  , inputComponent :: ReactComponent { | Props_input }
  , inputProps :: { | Props_input }
  , inputRef :: Foreign
  , margin :: MarginProp
  , multiline :: Boolean
  , name :: String
  , onChange :: EventHandler
  , placeholder :: String
  , readOnly :: Boolean
  , required :: Boolean
  , rows :: Number
  , rowsMax :: Number
  , select :: Boolean
  , startAdornment :: JSX
  , type :: String
  , value :: value
  | componentProps
  )

foreign import data InputBaseProps :: Type

foreign import data MarginProp :: Type
foreign import _eqMarginProp :: MarginProp -> MarginProp -> Boolean
foreign import _ordMarginProp :: MarginProp -> MarginProp -> Int
instance eqMarginProp :: Eq MarginProp where eq left right = _eqMarginProp left right
instance ordMarginProp :: Ord MarginProp where compare left right = compare (_ordMarginProp left right) (_ordMarginProp right left)

dense :: MarginProp
dense = unsafeCoerce "dense"

none :: MarginProp
none = unsafeCoerce "none"

type InputBaseClassKeyGenericOptions a =
  ( root :: a 
  , formControl :: a 
  , focused :: a 
  , disabled :: a 
  , adornedStart :: a 
  , adornedEnd :: a 
  , error :: a 
  , marginDense :: a 
  , multiline :: a 
  , fullWidth :: a 
  , input :: a 
  , inputMarginDense :: a 
  , inputSelect :: a 
  , inputMultiline :: a 
  , inputTypeSearch :: a 
  , inputAdornedStart :: a 
  , inputAdornedEnd :: a 
  , inputHiddenLabel :: a 
  )
type InputBaseClassKeyOptions = InputBaseClassKeyGenericOptions String
type InputBaseClassKeyJSSOptions = InputBaseClassKeyGenericOptions JSS
foreign import data InputBaseClassKey :: Type
foreign import data InputBaseClassKeyJSS :: Type

inputBaseClassKey :: ∀  given required
  .  Union given required (InputBaseClassKeyOptions )
  => Record given
  -> InputBaseClassKey
inputBaseClassKey = unsafeCoerce

inputBaseClassKeyJSS :: ∀  given required
  .  Union given required (InputBaseClassKeyJSSOptions )
  => Record given
  -> InputBaseClassKeyJSS
inputBaseClassKeyJSS = unsafeCoerce

inputBase :: ∀ value given required
  .  Union given required (InputBasePropsOptions Props_div value)
  => Record given
  -> JSX
inputBase = element _InputBase

inputBase_component :: ∀ componentProps value given required
  .  Union given required (InputBasePropsOptions componentProps value)
  => Record given
  -> JSX
inputBase_component = element _InputBase

foreign import _InputBase :: ∀ a. ReactComponent a