module MUI.Core.GridListTileBar where

import MUI.Core (JSS) as MUI.Core
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data TitlePosition :: Type

titlePosition :: { bottom :: TitlePosition, top :: TitlePosition }
titlePosition = { bottom: Unsafe.Coerce.unsafeCoerce "bottom", top: Unsafe.Coerce.unsafeCoerce "top" }

foreign import data ActionPosition :: Type

actionPosition :: { left :: ActionPosition, right :: ActionPosition }
actionPosition = { left: Unsafe.Coerce.unsafeCoerce "left", right: Unsafe.Coerce.unsafeCoerce "right" }

instance eqActionPosition :: Eq ActionPosition where
  eq = Unsafe.Reference.unsafeRefEq

instance eqTitlePosition :: Eq TitlePosition where
  eq = Unsafe.Reference.unsafeRefEq

type GridListTileBarPropsOptions componentProps = ( actionIcon :: React.Basic.JSX, actionPosition :: ActionPosition, classes :: GridListTileBarClassKey, subtitle :: React.Basic.JSX, title :: React.Basic.JSX, titlePosition :: TitlePosition | componentProps )

foreign import data GridListTileBarProps :: Type

foreign import data GridListTileBarPropsPartial :: Type

gridListTileBarPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (GridListTileBarPropsOptions React.Basic.DOM.Props_div) => Record options -> GridListTileBarPropsPartial
gridListTileBarPropsPartial = Unsafe.Coerce.unsafeCoerce

type GridListTileBarClassKeyGenericOptions a = ( actionIcon :: a, actionIconActionPosLeft :: a, root :: a, rootSubtitle :: a, subtitle :: a, title :: a, titlePositionBottom :: a, titlePositionTop :: a, titleWrap :: a, titleWrapActionPosLeft :: a, titleWrapActionPosRight :: a )

type GridListTileBarClassKeyOptions  = GridListTileBarClassKeyGenericOptions String

foreign import data GridListTileBarClassKey :: Type

gridListTileBarClassKey :: ∀ required given. Prim.Row.Union given required GridListTileBarClassKeyOptions => Record given -> GridListTileBarClassKey
gridListTileBarClassKey = Unsafe.Coerce.unsafeCoerce

type GridListTileBarClassKeyOptionsJSS  = GridListTileBarClassKeyGenericOptions MUI.Core.JSS

foreign import data GridListTileBarClassKeyJSS :: Type

gridListTileBarClassKeyJSS :: ∀ required given. Prim.Row.Union given required GridListTileBarClassKeyOptionsJSS => Record given -> GridListTileBarClassKeyJSS
gridListTileBarClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _GridListTileBar :: ∀ a. React.Basic.ReactComponent a

gridListTileBar :: ∀ required given. Prim.Row.Union given required (GridListTileBarPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
gridListTileBar = React.Basic.element _GridListTileBar

gridListTileBar_component :: ∀ required given componentProps. Prim.Row.Union given required (GridListTileBarPropsOptions componentProps) => Record given -> React.Basic.JSX
gridListTileBar_component = React.Basic.element _GridListTileBar