-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Icon/Icon.d.ts
module MaterialUI.Basic.Icon where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data IconPropsColor :: Type

foreign import _icon :: forall a. ReactComponent a



type IconProps  = 
  ( color :: IconPropsColor
  ,  component :: JSX
  ,  fontSize :: String
  ,  key :: String
  ,  children :: Array JSX
  )

icon
  :: forall attrs attrs_  
  . Union attrs attrs_ (IconProps  )
  => Record attrs
  -> JSX
icon props = element _icon props
 

icon_ :: Array JSX -> JSX
icon_ children = icon { children }  
