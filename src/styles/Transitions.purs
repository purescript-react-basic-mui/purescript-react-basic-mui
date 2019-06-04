-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/styles/transitions.d.ts
module MaterialUI.Basic.Styles.Transitions where 

import React.Basic (ReactComponent)





foreign import _transitions :: forall a. ReactComponent a

type Duration  = {
    complex :: Number
  , enteringScreen :: Number
  , leavingScreen :: Number
  , short :: Number
  , shorter :: Number
  , shortest :: Number
  , standard :: Number
}

  
type Easing  = {
    easeIn :: String
  , easeInOut :: String
  , easeOut :: String
  , sharp :: String
}

  
type Transitions  = {
    duration :: Duration
  , easing :: Easing
}

  
