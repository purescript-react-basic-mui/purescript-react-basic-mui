-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/NoSsr/NoSsr.d.ts
module MaterialUI.Basic.NoSsr where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _noSsr :: forall a. ReactComponent a



type NoSsrProps  = 
  ( defer :: Boolean
  ,  fallback :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

noSsr
  :: forall attrs attrs_  
  . Union attrs attrs_ (NoSsrProps  )
  => Record attrs
  -> JSX
noSsr props = element _noSsr props
 

noSsr_ :: Array JSX -> JSX
noSsr_ children = noSsr { children }  
