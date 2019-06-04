-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/styles/createBreakpoints.d.ts
module MaterialUI.Basic.Styles.CreateBreakpoints where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import React.Basic.Events (EventHandler)
import React.Basic (ReactComponent)





foreign import _createBreakpoints :: forall a. ReactComponent a

type Breakpoints  = {
    between :: (EffectFn2 String String String)
  , down :: EventHandler
  , keys :: (Array String)
  , only :: EventHandler
  , up :: EventHandler
  , values :: Foreign
  , width :: EventHandler
}

  
