module MUI.Core.Link where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import MUI.Core.Typography (TypographyClassKey) as MUI.Core.Typography
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_a) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { body1 :: Variant, body2 :: Variant, button :: Variant, caption :: Variant, h1 :: Variant, h2 :: Variant, h3 :: Variant, h4 :: Variant, h5 :: Variant, h6 :: Variant, inherit :: Variant, overline :: Variant, srOnly :: Variant, subtitle1 :: Variant, subtitle2 :: Variant }
variant = { body1: Unsafe.Coerce.unsafeCoerce "body1", body2: Unsafe.Coerce.unsafeCoerce "body2", button: Unsafe.Coerce.unsafeCoerce "button", caption: Unsafe.Coerce.unsafeCoerce "caption", h1: Unsafe.Coerce.unsafeCoerce "h1", h2: Unsafe.Coerce.unsafeCoerce "h2", h3: Unsafe.Coerce.unsafeCoerce "h3", h4: Unsafe.Coerce.unsafeCoerce "h4", h5: Unsafe.Coerce.unsafeCoerce "h5", h6: Unsafe.Coerce.unsafeCoerce "h6", inherit: Unsafe.Coerce.unsafeCoerce "inherit", overline: Unsafe.Coerce.unsafeCoerce "overline", srOnly: Unsafe.Coerce.unsafeCoerce "srOnly", subtitle1: Unsafe.Coerce.unsafeCoerce "subtitle1", subtitle2: Unsafe.Coerce.unsafeCoerce "subtitle2" }

foreign import data Underline :: Type

underline :: { always :: Underline, hover :: Underline, none :: Underline }
underline = { always: Unsafe.Coerce.unsafeCoerce "always", hover: Unsafe.Coerce.unsafeCoerce "hover", none: Unsafe.Coerce.unsafeCoerce "none" }

foreign import data Color :: Type

color :: { error :: Color, inherit :: Color, initial :: Color, primary :: Color, secondary :: Color, textPrimary :: Color, textSecondary :: Color }
color = { error: Unsafe.Coerce.unsafeCoerce "error", inherit: Unsafe.Coerce.unsafeCoerce "inherit", initial: Unsafe.Coerce.unsafeCoerce "initial", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary", textPrimary: Unsafe.Coerce.unsafeCoerce "textPrimary", textSecondary: Unsafe.Coerce.unsafeCoerce "textSecondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqUnderline :: Eq Underline where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type LinkPropsOptions componentProps = ( "TypographyClasses" :: MUI.Core.Typography.TypographyClassKey, children :: Array React.Basic.JSX, classes :: LinkClassKey, color :: Color, underline :: Underline, variant :: Variant | componentProps )

foreign import data LinkProps :: Type

foreign import data LinkPropsPartial :: Type

linkPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (LinkPropsOptions React.Basic.DOM.Props_a) => Record options -> LinkPropsPartial
linkPropsPartial = Unsafe.Coerce.unsafeCoerce

type LinkClassKeyGenericOptions a = ( alignCenter :: a, alignJustify :: a, alignLeft :: a, alignRight :: a, body1 :: a, body2 :: a, button :: a, caption :: a, colorError :: a, colorInherit :: a, colorSecondary :: a, colorTextSecondary :: a, displayBlock :: a, displayInline :: a, focusVisible :: a, gutterBottom :: a, h1 :: a, h2 :: a, h3 :: a, h4 :: a, h5 :: a, h6 :: a, noWrap :: a, overline :: a, paragraph :: a, root :: a, srOnly :: a, subtitle1 :: a, subtitle2 :: a, underlineAlways :: a, underlineHover :: a, underlineNone :: a )

type LinkClassKeyOptions  = LinkClassKeyGenericOptions String

foreign import data LinkClassKey :: Type

linkClassKey :: ∀ required given. Prim.Row.Union given required LinkClassKeyOptions => Record given -> LinkClassKey
linkClassKey = Unsafe.Coerce.unsafeCoerce

type LinkClassKeyOptionsJSS  = LinkClassKeyGenericOptions MUI.Core.JSS

foreign import data LinkClassKeyJSS :: Type

linkClassKeyJSS :: ∀ required given. Prim.Row.Union given required LinkClassKeyOptionsJSS => Record given -> LinkClassKeyJSS
linkClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Link :: ∀ a. React.Basic.ReactComponent a

link :: ∀ required given. Prim.Row.Union given required (LinkPropsOptions React.Basic.DOM.Props_a) => Record given -> React.Basic.JSX
link = React.Basic.element _Link

link_component :: ∀ required given componentProps. Prim.Row.Union given required (LinkPropsOptions componentProps) => Record given -> React.Basic.JSX
link_component = React.Basic.element _Link

linkWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (LinkPropsOptions React.Basic.DOM.Props_a) => Prim.Row.Union jss jss_ LinkClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
linkWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Link)