module MUI.Core.Typography where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core (JSS)
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TypographyProps =
  ( align :: Maybe String
  , children :: Maybe (Array JSX)
  , classes :: TypographyClassKey
  , className :: Maybe String
  , color :: Maybe String
  , component :: Maybe String
  , display :: Maybe String
  , gutterBottom :: Maybe Boolean
  , noWrap :: Maybe Boolean
  , paragraph :: Maybe Boolean
  , variant :: Maybe String 
  , variantMapping :: VariantMapping
  )

typographyProps :: { | TypographyProps }
typographyProps =
  { align : Just "inherit"
  , children : Nothing
  , classes
  , className : Nothing
  , color : Just "initial"
  , component : Nothing
  , display : Just "initial"
  , gutterBottom : Just false
  , noWrap : Just false
  , paragraph : Just false
  , variant : Just "body1"
  , variantMapping 
  }

type TypographyClassKey =
  { root :: Maybe JSS
  , h1 :: Maybe JSS
  , h2 :: Maybe JSS
  , h3 :: Maybe JSS
  , h4 :: Maybe JSS
  , h5 :: Maybe JSS
  , h6 :: Maybe JSS
  , subtitle1 :: Maybe JSS
  , subtitle2 :: Maybe JSS
  , body1 :: Maybe JSS
  , body2 :: Maybe JSS
  , caption :: Maybe JSS
  , button :: Maybe JSS
  , overline :: Maybe JSS
  , srOnly :: Maybe JSS
  , alignLeft :: Maybe JSS
  , alignCenter :: Maybe JSS
  , alignRight :: Maybe JSS
  , alignJustify :: Maybe JSS
  , noWrap :: Maybe JSS
  , gutterBottom :: Maybe JSS
  , paragraph :: Maybe JSS
  , colorInherit :: Maybe JSS
  , colorSecondary :: Maybe JSS
  , colorTextSecondary :: Maybe JSS
  , colorError :: Maybe JSS
  , displayInline :: Maybe JSS
  , displayBlock' :: Maybe JSS
  }

classes :: TypographyClassKey
classes =
  { root : Nothing
  , h1 : Nothing
  , h2 : Nothing
  , h3 : Nothing
  , h4 : Nothing
  , h5 : Nothing
  , h6 : Nothing
  , subtitle1 : Nothing
  , subtitle2 : Nothing
  , body1 : Nothing
  , body2 : Nothing
  , caption : Nothing
  , button : Nothing
  , overline : Nothing
  , srOnly : Nothing
  , alignLeft : Nothing
  , alignCenter : Nothing
  , alignRight : Nothing
  , alignJustify : Nothing
  , noWrap : Nothing
  , gutterBottom : Nothing
  , paragraph : Nothing
  , colorInherit : Nothing
  , colorSecondary : Nothing
  , colorTextSecondary : Nothing
  , colorError : Nothing
  , displayInline : Nothing
  , displayBlock' : Nothing
  }

type VariantMapping =
  { h1 :: String
  , h2 :: String
  , h3 :: String
  , h4 :: String
  , h5 :: String
  , h6 :: String
  , subtitle1 :: String
  , subtitle2 :: String
  , body1 :: String
  , body2 :: String
  }


variantMapping :: VariantMapping
variantMapping = 
  { h1: "h1"
  , h2: "h2"
  , h3: "h3"
  , h4: "h4"
  , h5: "h5"
  , h6: "h6"
  , subtitle1: "h6"
  , subtitle2: "h6"
  , body1: "p"
  , body2: "p"
  }
propsToForeign :: { | TypographyProps } -> Foreign
propsToForeign props = 
  write $ toInternalChildren props

typography :: { | TypographyProps } -> JSX
typography props = element _Typography $ unsafeCoerce $ propsToForeign props

foreign import _Typography :: ∀ a. ReactComponent a