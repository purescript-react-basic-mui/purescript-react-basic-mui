-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Tabs/TabIndicator.d.ts
module MaterialUI.Basic.TabIndicator where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _tabIndicator :: forall a. ReactComponent a



type TabIndicatorProps_optional  = 
  ( key :: String
  ,  children :: Array JSX
  )



type TabIndicatorProps_required   optional = 
  ( color :: String
  ,  style :: { left :: Number, width :: Number }
  | optional
  )

tabIndicator
  :: forall attrs attrs_  
  . Union attrs attrs_ (TabIndicatorProps_optional  )
  => Record ((TabIndicatorProps_required  ) attrs)
  -> JSX
tabIndicator props = element _tabIndicator props  
