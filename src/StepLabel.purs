-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/StepLabel/StepLabel.d.ts
module MaterialUI.Basic.StepLabel where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data StepLabelPropsIcon :: Type

foreign import _stepLabel :: forall a. ReactComponent a



type StepLabelProps  = 
  ( "StepIconComponent" :: JSX
  ,  "StepIconProps" :: Foreign
  ,  active :: Boolean
  ,  alternativeLabel :: Boolean
  ,  completed :: Boolean
  ,  disabled :: Boolean
  ,  error :: Boolean
  ,  icon :: StepLabelPropsIcon
  ,  last :: Boolean
  ,  optional :: JSX
  ,  orientation :: String
  ,  key :: String
  ,  children :: Array JSX
  )

stepLabel
  :: forall attrs attrs_  
  . Union attrs attrs_ (StepLabelProps  )
  => Record attrs
  -> JSX
stepLabel props = element _stepLabel props
 

stepLabel_ :: Array JSX -> JSX
stepLabel_ children = stepLabel { children }  
