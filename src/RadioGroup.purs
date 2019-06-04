-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/RadioGroup/RadioGroup.d.ts
module MaterialUI.Basic.RadioGroup where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _radioGroup :: forall a. ReactComponent a



type RadioGroupProps  = 
  ( name :: String
  ,  onChange :: (EffectFn2 Foreign String Unit)
  ,  value :: String
  ,  key :: String
  ,  children :: Array JSX
  )

radioGroup
  :: forall attrs attrs_  
  . Union attrs attrs_ (RadioGroupProps  )
  => Record attrs
  -> JSX
radioGroup props = element _radioGroup props
 

radioGroup_ :: Array JSX -> JSX
radioGroup_ children = radioGroup { children }  
