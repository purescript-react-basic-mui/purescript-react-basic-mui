-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/StepButton/StepButton.d.ts
module MaterialUI.Basic.StepButton where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data StepButtonPropsIcon :: Type

foreign import _stepButton :: forall a. ReactComponent a



type StepButtonProps  = 
  ( active :: Boolean
  ,  alternativeLabel :: Boolean
  ,  completed :: Boolean
  ,  disabled :: Boolean
  ,  icon :: StepButtonPropsIcon
  ,  last :: Boolean
  ,  optional :: JSX
  ,  orientation :: String
  ,  key :: String
  ,  children :: Array JSX
  )

stepButton
  :: forall attrs attrs_  
  . Union attrs attrs_ (StepButtonProps  )
  => Record attrs
  -> JSX
stepButton props = element _stepButton props
 

stepButton_ :: Array JSX -> JSX
stepButton_ children = stepButton { children }  
