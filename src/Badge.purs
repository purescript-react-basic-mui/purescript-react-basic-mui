-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Badge/Badge.d.ts
module MaterialUI.Basic.Badge where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data BadgePropsColor :: Type

foreign import _badge :: forall a. ReactComponent a



type BadgeProps  = 
  ( badgeContent :: JSX
  ,  color :: BadgePropsColor
  ,  component :: JSX
  ,  invisible :: Boolean
  ,  max :: Number
  ,  showZero :: Boolean
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

badge
  :: forall attrs attrs_  
  . Union attrs attrs_ (BadgeProps  )
  => Record attrs
  -> JSX
badge props = element _badge props
 

badge_ :: Array JSX -> JSX
badge_ children = badge { children }  
