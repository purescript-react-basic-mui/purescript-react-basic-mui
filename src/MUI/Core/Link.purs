module MUI.Core.Link where


import MUI.Core.Typography (TypographyClassKey)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type LinkProps =
  ( children :: Array JSX
  , classes :: LinkClassKey
  , color :: String
  , component :: String
  , "TypographyClasses" :: TypographyClassKey
  , underline :: String
  , variant :: String
  )

foreign import data LinkClassKey :: Type

type LinkClassKeyOptions =
  ( root :: String
  , underlineNone :: String
  , underlineHover :: String
  , underlineAlways :: String
  , button :: String
  , focusVisible :: String
  )

linkClassKey 
  :: ∀ options options_
  . Union options options_ LinkClassKeyOptions
  => Record options
  -> LinkClassKey
linkClassKey = unsafeCoerce

link
  :: ∀ props props_
  . Union props props_ LinkProps
  => Record props 
  -> JSX
link = element _Link

foreign import _Link :: ∀ a. ReactComponent a