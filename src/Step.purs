-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Step/Step.d.ts
module MaterialUI.Basic.Step where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _step :: forall a. ReactComponent a



type StepProps  = 
  ( active :: Boolean
  ,  alternativeLabel :: Boolean
  ,  completed :: Boolean
  ,  connector :: JSX
  ,  disabled :: Boolean
  ,  index :: Number
  ,  last :: Boolean
  ,  orientation :: String
  ,  key :: String
  ,  children :: Array JSX
  )

step
  :: forall attrs attrs_  
  . Union attrs attrs_ (StepProps  )
  => Record attrs
  -> JSX
step props = element _step props
 

step_ :: Array JSX -> JSX
step_ children = step { children }  
