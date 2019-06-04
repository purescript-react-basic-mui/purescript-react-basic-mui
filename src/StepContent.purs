-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/StepContent/StepContent.d.ts
module MaterialUI.Basic.StepContent where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (Component, JSX, ReactComponent, element)



foreign import data StepContentPropsTransitionDuration :: Type

foreign import _stepContent :: forall a. ReactComponent a



type StepContentProps  = 
  ( "TransitionComponent" :: (Component Foreign)
  ,  "TransitionProps" :: Foreign
  ,  active :: Boolean
  ,  alternativeLabel :: Boolean
  ,  completed :: Boolean
  ,  last :: Boolean
  ,  optional :: Boolean
  ,  orientation :: String
  ,  transitionDuration :: StepContentPropsTransitionDuration
  ,  key :: String
  ,  children :: Array JSX
  )

stepContent
  :: forall attrs attrs_  
  . Union attrs attrs_ (StepContentProps  )
  => Record attrs
  -> JSX
stepContent props = element _stepContent props
 

stepContent_ :: Array JSX -> JSX
stepContent_ children = stepContent { children }  
