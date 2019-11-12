module MUI.Core.ListItemText where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import MUI.Core.Typography (TypographyClassKey) as MUI.Core.Typography
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type ListItemTextPropsOptions componentProps = ( classes :: ListItemTextClassKey, disableTypography :: Boolean, inset :: Boolean, primary :: React.Basic.JSX, primaryTypographyProps :: MUI.Core.Typography.TypographyClassKey, secondary :: React.Basic.JSX, secondaryTypographyProps :: MUI.Core.Typography.TypographyClassKey | componentProps )

foreign import data ListItemTextProps :: Type

foreign import data ListItemTextPropsPartial :: Type

listItemTextPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ListItemTextPropsOptions React.Basic.DOM.Props_div) => Record options -> ListItemTextPropsPartial
listItemTextPropsPartial = Unsafe.Coerce.unsafeCoerce

type ListItemTextClassKeyGenericOptions a = ( dense :: a, inset :: a, multiline :: a, primary :: a, root :: a, secondary :: a )

type ListItemTextClassKeyOptions  = ListItemTextClassKeyGenericOptions String

foreign import data ListItemTextClassKey :: Type

listItemTextClassKey :: ∀ required given. Prim.Row.Union given required ListItemTextClassKeyOptions => Record given -> ListItemTextClassKey
listItemTextClassKey = Unsafe.Coerce.unsafeCoerce

type ListItemTextClassKeyOptionsJSS  = ListItemTextClassKeyGenericOptions MUI.Core.JSS

foreign import data ListItemTextClassKeyJSS :: Type

listItemTextClassKeyJSS :: ∀ required given. Prim.Row.Union given required ListItemTextClassKeyOptionsJSS => Record given -> ListItemTextClassKeyJSS
listItemTextClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _ListItemText :: ∀ a. React.Basic.ReactComponent a

listItemText :: ∀ required given. Prim.Row.Union given required (ListItemTextPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
listItemText = React.Basic.element _ListItemText

listItemText_component :: ∀ required given componentProps. Prim.Row.Union given required (ListItemTextPropsOptions componentProps) => Record given -> React.Basic.JSX
listItemText_component = React.Basic.element _ListItemText

listItemTextWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ListItemTextPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ ListItemTextClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
listItemTextWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _ListItemText)