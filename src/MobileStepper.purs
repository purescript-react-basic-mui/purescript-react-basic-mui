-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/MobileStepper/MobileStepper.d.ts
module MaterialUI.Basic.MobileStepper where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _mobileStepper :: forall a. ReactComponent a



type MobileStepperProps_optional  = 
  ( "LinearProgressProps" :: Foreign
  ,  activeStep :: Number
  ,  position :: String
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )



type MobileStepperProps_required   optional = 
  ( backButton :: JSX
  ,  nextButton :: JSX
  ,  steps :: Number
  | optional
  )

mobileStepper
  :: forall attrs attrs_  
  . Union attrs attrs_ (MobileStepperProps_optional  )
  => Record ((MobileStepperProps_required  ) attrs)
  -> JSX
mobileStepper props = element _mobileStepper props  
