module MUI.Core.Breadcrumbs where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_nav)
import Unsafe.Coerce (unsafeCoerce)

type BreadcrumbsPropsOptions componentProps = 
  ( children :: (Array JSX)
  , classes :: BreadcrumbsClassKey
  , component :: ReactComponent { | componentProps }
  , itemsAfterCollapse :: Number
  , itemsBeforeCollapse :: Number
  , maxItems :: Number
  , separator :: JSX
  | componentProps
  )

foreign import data BreadcrumbsProps :: Type

type BreadcrumbsClassKeyGenericOptions a =
  ( root :: a 
  , ol :: a 
  , li :: a 
  , separator :: a 
  )
type BreadcrumbsClassKeyOptions = BreadcrumbsClassKeyGenericOptions String
type BreadcrumbsClassKeyJSSOptions = BreadcrumbsClassKeyGenericOptions JSS
foreign import data BreadcrumbsClassKey :: Type
foreign import data BreadcrumbsClassKeyJSS :: Type

breadcrumbsClassKey :: ∀  given required
  .  Union given required (BreadcrumbsClassKeyOptions )
  => Record given
  -> BreadcrumbsClassKey
breadcrumbsClassKey = unsafeCoerce

breadcrumbsClassKeyJSS :: ∀  given required
  .  Union given required (BreadcrumbsClassKeyJSSOptions )
  => Record given
  -> BreadcrumbsClassKeyJSS
breadcrumbsClassKeyJSS = unsafeCoerce

breadcrumbs :: ∀  given required
  .  Union given required (BreadcrumbsPropsOptions Props_nav )
  => Record given
  -> JSX
breadcrumbs = element _Breadcrumbs

breadcrumbs_component :: ∀ componentProps given required
  .  Union given required (BreadcrumbsPropsOptions componentProps)
  => Record given
  -> JSX
breadcrumbs_component = element _Breadcrumbs

foreign import _Breadcrumbs :: ∀ a. ReactComponent a