module React.Basic.MUI.Styles.WithStyles where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type StyleRules classkey props = ActualStyleRules Foreign Foreign

type WithStyles stylesorclasskey includetheme = Foreign

withStyles :: PropInjector WithStyles Foreign Foreign Foreign
withStyles = _withStyles
foreign import _withStyles :: PropInjector WithStyles Foreign Foreign Foreign