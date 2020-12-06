{- This module was autogenerated. Please don't edit. -}

module MUI.Core.Box where

import Foreign (Foreign) as Foreign
import MUI.Core (class Nub')
import MUI.System (BoxSizing) as MUI.System
import MUI.System.Display (Display)
import MUI.System.Flexbox (AlignContent, AlignItems, AlignSelf, FlexDirection, FlexWrap, JustifyContent) as MUI.System.Flexbox
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)

type BoxOptPropsRow (r :: # Type) =
  ( 
    alignContent :: MUI.System.Flexbox.AlignContent,
    alignItems :: MUI.System.Flexbox.AlignItems,
    alignSelf :: MUI.System.Flexbox.AlignSelf,
    border :: Foreign.Foreign,
    borderBottom :: Foreign.Foreign,
    borderColor :: Foreign.Foreign,
    borderLeft :: Foreign.Foreign,
    borderRadius :: Foreign.Foreign,
    borderRight :: Foreign.Foreign,
    borderTop :: Foreign.Foreign,
    boxSizing :: MUI.System.BoxSizing,
    children :: Array  JSX,
    clone :: Boolean,
    component :: String,
    display :: Display,
    flexBasis :: String,
    flexDirection :: MUI.System.Flexbox.FlexDirection,
    flexGrow :: Number,
    flexShrink :: Number,
    flexWrap :: MUI.System.Flexbox.FlexWrap,
    height :: String,
    justifyContent :: MUI.System.Flexbox.JustifyContent,
    m :: Int,
    margin :: Int,
    marginBottom :: Int,
    marginLeft :: Int,
    marginRight :: Int,
    marginTop :: Int,
    marginX :: Int,
    marginY :: Int,
    maxHeight :: String,
    maxWidth :: String,
    mb :: Int,
    minHeight :: String,
    minWidth :: String,
    ml :: Int,
    mr :: Int,
    mt :: Int,
    mx :: Int,
    my :: Int,
    p :: Int,
    padding :: Int,
    paddingBottom :: Int,
    paddingLeft :: Int,
    paddingRight :: Int,
    paddingTop :: Int,
    paddingX :: Int,
    paddingY :: Int,
    pb :: Int,
    pl :: Int,
    pr :: Int,
    pt :: Int,
    px :: Int,
    py :: Int,
    width :: String
   | r
   )

type BoxReqPropsRow (r :: # Type) =
  r

type BoxPropsRow (r :: # Type) =
  BoxOptPropsRow (BoxReqPropsRow r)

foreign import _UnsafeBox :: forall componentProps.    ReactComponent {   | BoxPropsRow componentProps  }

_Box::forall given optionalGiven optionalMissing props required. 
  Nub' (BoxReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (BoxPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent {   | given  }
_Box = unsafeCoerce _UnsafeBox

box::forall given optionalGiven optionalMissing props required. 
  Nub' (BoxReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (BoxPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  {   | given  }  ->  JSX
box ps = element _Box ps

_Box'::ReactComponent BoxProps
_Box' = unsafeCoerce _UnsafeBox

foreign import data BoxProps :: Type

props::forall given optionalGiven optionalMissing props required. 
  Nub' (BoxReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (BoxPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  {   | given  }  ->  BoxProps
props = unsafeCoerce
