-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/TablePagination/TablePaginationActions.d.ts
module MaterialUI.Basic.TablePaginationActions where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _tablePaginationActions :: forall a. ReactComponent a



type TablePaginationActionsProps_optional  = 
  ( backIconButtonProps :: Foreign
  ,  nextIconButtonProps :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )



type TablePaginationActionsProps_required   optional = 
  ( count :: Number
  ,  onChangePage :: (EffectFn2 JSX Number Unit)
  ,  page :: Number
  ,  rowsPerPage :: Number
  | optional
  )

tablePaginationActions
  :: forall attrs attrs_  
  . Union attrs attrs_ (TablePaginationActionsProps_optional  )
  => Record ((TablePaginationActionsProps_required  ) attrs)
  -> JSX
tablePaginationActions props = element _tablePaginationActions props  
