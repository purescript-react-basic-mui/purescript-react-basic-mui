-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/FilledInput/FilledInput.d.ts
module MaterialUI.Basic.FilledInput where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _filledInput :: forall a. ReactComponent a



type FilledInputProps  = 
  ( disableUnderline :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

filledInput
  :: forall attrs attrs_  
  . Union attrs attrs_ (FilledInputProps  )
  => Record attrs
  -> JSX
filledInput props = element _filledInput props
 

filledInput_ :: Array JSX -> JSX
filledInput_ children = filledInput { children }  
