-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/SvgIcon/SvgIcon.d.ts
module MaterialUI.Basic.SvgIcon where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data SvgIconPropsColor :: Type

foreign import _svgIcon :: forall a. ReactComponent a



type SvgIconProps  = 
  ( color :: SvgIconPropsColor
  ,  component :: JSX
  ,  fontSize :: String
  ,  htmlColor :: String
  ,  shapeRendering :: String
  ,  titleAccess :: String
  ,  viewBox :: String
  ,  key :: String
  ,  children :: Array JSX
  )

svgIcon
  :: forall attrs attrs_  
  . Union attrs attrs_ (SvgIconProps  )
  => Record attrs
  -> JSX
svgIcon props = element _svgIcon props
 

svgIcon_ :: Array JSX -> JSX
svgIcon_ children = svgIcon { children }  
