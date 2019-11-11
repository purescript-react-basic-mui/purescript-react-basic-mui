module MUI.Core.Breadcrumbs where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type BreadcrumbsPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: BreadcrumbsClassKey, itemsAfterCollapse :: Number, itemsBeforeCollapse :: Number, maxItems :: Number, ref :: Foreign.Foreign, separator :: React.Basic.JSX | componentProps )

foreign import data BreadcrumbsProps :: Type

type BreadcrumbsClassKeyGenericOptions a = ( li :: a, ol :: a, root :: a, separator :: a )

type BreadcrumbsClassKeyOptions  = BreadcrumbsClassKeyGenericOptions String

foreign import data BreadcrumbsClassKey :: Type

breadcrumbsClassKey :: ∀ required given. Prim.Row.Union given required BreadcrumbsClassKeyOptions => Record given -> BreadcrumbsClassKey
breadcrumbsClassKey = Unsafe.Coerce.unsafeCoerce

type BreadcrumbsClassKeyOptionsJSS  = BreadcrumbsClassKeyGenericOptions MUI.Core.JSS

foreign import data BreadcrumbsClassKeyJSS :: Type

breadcrumbsClassKeyJSS :: ∀ required given. Prim.Row.Union given required BreadcrumbsClassKeyOptionsJSS => Record given -> BreadcrumbsClassKeyJSS
breadcrumbsClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Breadcrumbs :: ∀ a. React.Basic.ReactComponent a

breadcrumbs :: ∀ required given. Prim.Row.Union given required (BreadcrumbsPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
breadcrumbs = React.Basic.element _Breadcrumbs

breadcrumbs_component :: ∀ required given componentProps. Prim.Row.Union given required (BreadcrumbsPropsOptions componentProps) => Record given -> React.Basic.JSX
breadcrumbs_component = React.Basic.element _Breadcrumbs