-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/CardHeader/CardHeader.d.ts
module MaterialUI.Basic.CardHeader where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _cardHeader :: forall a. ReactComponent a



type CardHeaderProps  = 
  ( action :: JSX
  ,  avatar :: JSX
  ,  component :: JSX
  ,  disableTypography :: Boolean
  ,  subheader :: JSX
  ,  subheaderTypographyProps :: Foreign
  ,  title :: JSX
  ,  titleTypographyProps :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )

cardHeader
  :: forall attrs attrs_  
  . Union attrs attrs_ (CardHeaderProps  )
  => Record attrs
  -> JSX
cardHeader props = element _cardHeader props
 

cardHeader_ :: Array JSX -> JSX
cardHeader_ children = cardHeader { children }  
