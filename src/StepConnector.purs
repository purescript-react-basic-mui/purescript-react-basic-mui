-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/StepConnector/StepConnector.d.ts
module MaterialUI.Basic.StepConnector where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _stepConnector :: forall a. ReactComponent a



type StepConnectorProps  = 
  ( active :: Boolean
  ,  alternativeLabel :: Boolean
  ,  completed :: Boolean
  ,  disabled :: Boolean
  ,  index :: Number
  ,  orientation :: String
  ,  key :: String
  ,  children :: Array JSX
  )

stepConnector
  :: forall attrs attrs_  
  . Union attrs attrs_ (StepConnectorProps  )
  => Record attrs
  -> JSX
stepConnector props = element _stepConnector props
 

stepConnector_ :: Array JSX -> JSX
stepConnector_ children = stepConnector { children }  
