-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Typography/Typography.d.ts
module MaterialUI.Basic.Typography where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data Alignment :: Type
foreign import data TypographyPropsVariant :: Type

foreign import _typography :: forall a. ReactComponent a



type TypographyProps  = 
  ( align :: Alignment
  ,  color :: String
  ,  component :: JSX
  ,  display :: String
  ,  gutterBottom :: Boolean
  ,  noWrap :: Boolean
  ,  paragraph :: Boolean
  ,  variant :: TypographyPropsVariant
  ,  variantMapping :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )

typography
  :: forall attrs attrs_  
  . Union attrs attrs_ (TypographyProps  )
  => Record attrs
  -> JSX
typography props = element _typography props
 

typography_ :: Array JSX -> JSX
typography_ children = typography { children }  
