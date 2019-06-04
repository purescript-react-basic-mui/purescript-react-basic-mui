-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/styles/createMixins.d.ts
module MaterialUI.Basic.Styles.CreateMixins where 
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)
import React.Basic (ReactComponent)





foreign import _createMixins :: forall a. ReactComponent a

type Mixins  = {
    gutters :: EventHandler
  , toolbar :: CSS
}

  
