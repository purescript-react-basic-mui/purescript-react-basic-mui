module MUI.Core.GridListTile where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_li) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type GridListTilePropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: GridListTileClassKey, cols :: Number, component :: React.Basic.ReactComponent {  | componentProps }, rows :: Number | componentProps )

foreign import data GridListTileProps :: Type

foreign import data GridListTilePropsPartial :: Type

gridListTilePropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (GridListTilePropsOptions React.Basic.DOM.Props_li) => Record options -> GridListTilePropsPartial
gridListTilePropsPartial = Unsafe.Coerce.unsafeCoerce

type GridListTileClassKeyGenericOptions a = ( imgFullHeight :: a, imgFullWidth :: a, root :: a, tile :: a )

type GridListTileClassKeyOptions  = GridListTileClassKeyGenericOptions String

foreign import data GridListTileClassKey :: Type

gridListTileClassKey :: ∀ required given. Prim.Row.Union given required GridListTileClassKeyOptions => Record given -> GridListTileClassKey
gridListTileClassKey = Unsafe.Coerce.unsafeCoerce

type GridListTileClassKeyOptionsJSS  = GridListTileClassKeyGenericOptions MUI.Core.JSS

foreign import data GridListTileClassKeyJSS :: Type

gridListTileClassKeyJSS :: ∀ required given. Prim.Row.Union given required GridListTileClassKeyOptionsJSS => Record given -> GridListTileClassKeyJSS
gridListTileClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _GridListTile :: ∀ a. React.Basic.ReactComponent a

gridListTile :: ∀ required given. Prim.Row.Union given required (GridListTilePropsOptions React.Basic.DOM.Props_li) => Record given -> React.Basic.JSX
gridListTile = React.Basic.element _GridListTile

gridListTile_component :: ∀ required given componentProps. Prim.Row.Union given required (GridListTilePropsOptions componentProps) => Record given -> React.Basic.JSX
gridListTile_component = React.Basic.element _GridListTile