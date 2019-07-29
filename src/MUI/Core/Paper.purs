module MUI.Core.Paper where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type PaperProps componentProps =
  ( children :: Array JSX
  , classes :: PaperClassKey
  , component :: ReactComponent { | componentProps }
  , elevation :: Number
  , square :: Boolean
  | componentProps
  )

foreign import data PaperClassKey :: Type
foreign import data PaperClassKeyJSS :: Type
foreign import data PaperPropsPartial :: Type

type PaperClassKeyOptionsJSS = PaperClassKeyOptionsR JSS
type PaperClassKeyOptions = PaperClassKeyOptionsR String
type PaperClassKeyOptionsR a =
  ( root :: a
  , rounded :: a
  , elevation0 :: a
  , elevation1 :: a
  , elevation2 :: a
  , elevation3 :: a
  , elevation4 :: a
  , elevation5 :: a
  , elevation6 :: a
  , elevation7 :: a
  , elevation8 :: a
  , elevation9 :: a
  , elevation10 :: a
  , elevation11 :: a
  , elevation12 :: a
  , elevation13 :: a
  , elevation14 :: a
  , elevation15 :: a
  , elevation16 :: a
  , elevation17 :: a
  , elevation18 :: a
  , elevation19 :: a
  , elevation20 :: a
  , elevation21 :: a
  , elevation22 :: a
  , elevation23 :: a
  , elevation24 :: a
  )

paperClassKey :: ∀ options options_
  . Union options options_ PaperClassKeyOptions
  => Record options
  -> PaperClassKey
paperClassKey = unsafeCoerce

paperClassKeyJSS :: ∀ options options_
  . Union options options_ PaperClassKeyOptionsJSS
  => Record options
  -> PaperClassKeyJSS
paperClassKeyJSS = unsafeCoerce

paperPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (PaperProps componentProps)
  => Record props 
  -> PaperPropsPartial
paperPropsPartial_component = unsafeCoerce

paperPropsPartial :: ∀ props props_
  . Union props props_ (PaperProps Props_div)
  => Record props 
  -> PaperPropsPartial
paperPropsPartial = unsafeCoerce

paper_component :: ∀ componentProps props props_
  . Union props props_ (PaperProps componentProps)
  => Record props 
  -> JSX
paper_component = element _Paper

paper :: ∀ props props_
  . Union props props_ (PaperProps Props_div)
  => Record props 
  -> JSX
paper = element _Paper


foreign import _Paper :: ∀ a. ReactComponent a