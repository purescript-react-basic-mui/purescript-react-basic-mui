-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/NativeSelect/NativeSelect.d.ts
module MaterialUI.Basic.NativeSelect where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _nativeSelect :: forall a. ReactComponent a



type NativeSelectProps  = 
  ( "IconComponent" :: JSX
  ,  input :: JSX
  ,  value :: Foreign
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

nativeSelect
  :: forall attrs attrs_  
  . Union attrs attrs_ (NativeSelectProps  )
  => Record attrs
  -> JSX
nativeSelect props = element _nativeSelect props
 

nativeSelect_ :: Array JSX -> JSX
nativeSelect_ children = nativeSelect { children }  
