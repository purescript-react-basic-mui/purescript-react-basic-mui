-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Stepper/Stepper.d.ts
module MaterialUI.Basic.Stepper where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _stepper :: forall a. ReactComponent a



type StepperProps  = 
  ( activeStep :: Number
  ,  alternativeLabel :: Boolean
  ,  connector :: JSX
  ,  nonLinear :: Boolean
  ,  orientation :: String
  ,  key :: String
  ,  children :: Array JSX
  )

stepper
  :: forall attrs attrs_  
  . Union attrs attrs_ (StepperProps  )
  => Record attrs
  -> JSX
stepper props = element _stepper props
 

stepper_ :: Array JSX -> JSX
stepper_ children = stepper { children }  
