module MUI.Core.Link where


import MUI.Core.Typography (TypographyClassKey)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_a)
import Unsafe.Coerce (unsafeCoerce)

type LinkProps props =
  ( children :: Array JSX
  , classes :: LinkClassKey
  , color :: String
  , component :: ReactComponent { | props }
  , "TypographyClasses" :: TypographyClassKey
  , underline :: String
  , variant :: String
  | props
  )

foreign import data LinkClassKey :: Type
foreign import data LinkPropsPartial :: Type

type LinkClassKeyOptions =
  ( root :: String
  , underlineNone :: String
  , underlineHover :: String
  , underlineAlways :: String
  , button :: String
  , focusVisible :: String
  )

linkClassKey  :: ∀ options options_
  . Union options options_ LinkClassKeyOptions
  => Record options
  -> LinkClassKey
linkClassKey = unsafeCoerce

linkPropsPartial :: ∀ a props props_
  . Union props props_ (LinkProps a)
  => Record props 
  -> LinkPropsPartial
linkPropsPartial = unsafeCoerce

link :: ∀ props props_
  . Union props props_ (LinkProps Props_a)
  => { | props }
  -> JSX
link = element _Link

foreign import _Link :: ∀ a. ReactComponent a