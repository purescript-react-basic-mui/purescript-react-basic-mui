module MUI.Core.Breadcrumbs where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_nav)
import Unsafe.Coerce (unsafeCoerce)

type BreadcrumbsProps componentProps =
  ( children :: Array JSX
  , classes :: BreadcrumbsClassKey
  , component :: ReactComponent { | componentProps }
  , itemsAfterCollapse :: Number
  , itemsBeforeCollapse :: Number
  , maxItems :: Number
  , separator :: JSX
  | componentProps
  )

type BreadcrumbsClassKeyOptionsJSS = BreadcrumbsClassKeyOptionsR JSS
type BreadcrumbsClassKeyOptions = BreadcrumbsClassKeyOptionsR String
type BreadcrumbsClassKeyOptionsR a =
  ( root :: a
  , ol :: a
  , li :: a
  , separator :: a
  )


foreign import data BreadcrumbsClassKey :: Type
foreign import data BreadcrumbsClassKeyJSS :: Type
foreign import data BreadcrumbsPropsPartial :: Type

breadCrumbsClassKey :: ∀ options options_
  . Union options options_ BreadcrumbsClassKeyOptions
  => Record options
  -> BreadcrumbsClassKey
breadCrumbsClassKey = unsafeCoerce

breadCrumbsClassKeyJSS :: ∀ options options_
  . Union options options_ BreadcrumbsClassKeyOptionsJSS
  => Record options
  -> BreadcrumbsClassKeyJSS
breadCrumbsClassKeyJSS = unsafeCoerce

breadCrumbsPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (BreadcrumbsProps componentProps)
  => Record props 
  -> BreadcrumbsPropsPartial
breadCrumbsPropsPartial_component = unsafeCoerce

breadCrumbsPropsPartial :: ∀ props props_
  . Union props props_ (BreadcrumbsProps Props_nav)
  => Record props 
  -> BreadcrumbsPropsPartial
breadCrumbsPropsPartial = unsafeCoerce

breadCrumbs_component :: ∀ componentProps props props_
  . Union props props_ (BreadcrumbsProps componentProps)
  => Record props 
  -> JSX
breadCrumbs_component = element _Breadcrumbs

breadCrumbs :: ∀ props props_
  . Union props props_ (BreadcrumbsProps Props_nav)
  => Record props 
  -> JSX
breadCrumbs = element _Breadcrumbs

foreign import _Breadcrumbs :: ∀ a. ReactComponent a
