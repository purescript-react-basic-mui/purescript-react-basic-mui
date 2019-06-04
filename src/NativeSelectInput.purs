-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/NativeSelect/NativeSelectInput.d.ts
module MaterialUI.Basic.NativeSelectInput where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)





foreign import _nativeSelectInput :: forall a. ReactComponent a



type NativeSelectInputProps  = 
  ( "IconComponent" :: JSX
  ,  disabled :: Boolean
  ,  inputRef :: EventHandler
  ,  name :: String
  ,  onChange :: (EffectFn2 Foreign JSX Unit)
  ,  value :: Foreign
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

nativeSelectInput
  :: forall attrs attrs_  
  . Union attrs attrs_ (NativeSelectInputProps  )
  => Record attrs
  -> JSX
nativeSelectInput props = element _nativeSelectInput props
 

nativeSelectInput_ :: Array JSX -> JSX
nativeSelectInput_ children = nativeSelectInput { children }  
