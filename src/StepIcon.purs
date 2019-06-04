-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/StepIcon/StepIcon.d.ts
module MaterialUI.Basic.StepIcon where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _stepIcon :: forall a. ReactComponent a



type StepIconProps_optional  = 
  ( active :: Boolean
  ,  completed :: Boolean
  ,  error :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )



type StepIconProps_required   optional = 
  ( icon :: JSX
  | optional
  )

stepIcon
  :: forall attrs attrs_  
  . Union attrs attrs_ (StepIconProps_optional  )
  => Record ((StepIconProps_required  ) attrs)
  -> JSX
stepIcon props = element _stepIcon props  
