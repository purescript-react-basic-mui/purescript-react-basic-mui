module Codegen.Model where

import Prelude

import Codegen.AST (Declaration, Ident, QualifiedName, Row, RowLabel, Type, TypeName)
import Codegen.AST.Sugar.Type (app, array, constructor) as Type
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Moldy (class Moldable, Moldy(..), moldMap, moldlDefault, moldrDefault)

-- | TODO: Update required
-- |
-- | `Component` is the basic unit from which code is generated. The best example of usage so far is in  src/Codegen/Core/Typography.purs
-- | - `name`
-- |  - The name of the component, for example `Typography`
-- | - `moduleName`
-- |   - The name of the PureScript module which needs to follow a pattern that mimics the @material-ui structure (`MUI` will be removed from the module name when the javascript file is generated)
-- | - `props`
-- |   - an object describing the name and type of the props for the component 
-- | - `componentTypeVariable`
-- |   - The name of the type variable included in props type row. This is used with the `inherits` field to add additional properties to a component, for example adding all of the React Basic `Props_div` properties to `Paper`. Typically the value will just be `componentProps` and there is a helper function below.
-- | - `additionalTypeVariables`
-- |   - Some components take additional type variables to describe the type of a value. For example in `InputBase` the `onChange` effect can deliver a value of a given type. This is added to the type variables of the props row type.
-- | - `classKey`
-- |   - a list of strings from which a `*ClassKey` can be generated
-- | - `inherits`
-- |   - most components will inherit some properties from from an underlying component. For example `Badge` inherits all of the properties from React Basic's `Props_div` (div component). A more complex example can be seen in `AppBar` where all of the properties from `Paper` are in inherited - the complexity is the `PaperProps` takes a type parameter `componentProps` which is set as `Props_div`. This means that `AppBar` has all of the properties from the `props` field and all of the properties from `PaperProps` and `Props_div` from React Basic. 
-- | - `variants`
-- |   - Typescript supports Union types and Purescript does not. Variants are how we bridge the gap. Any Union types used in `@material-ui` are described as variants here. More information is below
-- | - 'extraCode`
-- |   - Sometimes code needs to be added to an individual component. For example, in `Typography` this is used to add a type called `VariantMapping`

type Component =
  { extraCode ∷ Maybe (Array Declaration)
  , inherits ∷ Maybe Type
  , name ∷ String
  , modulePath ∷ ModulePath
  , propsType ∷
    { base ∷ Maybe Row
    , generate ∷ Array RowLabel
    , vars ∷ Array Ident
    }
  , tsc :: { strictNullChecks ∷ Boolean }
  }

-- | TODO: Update required
-- |
-- | This ADT is used to describe the name of the Purescript module. It's also used to determine file names and generate FFI.
-- | Because it's used for FFI generation, it should mimic the structure of `@material-ui`. For example, when writing the
-- | `Typography` component, it's JS import is `@material-ui/core/Typography` so the correct value for module is
-- | `Path "MUI" (Path "Core" (Name "Typography"))`. Node that `MUI` will be removed in the FFI, so you get FFI that looks like
-- | `exports._Typography = require("@material-ui/core/Typography").default;`. That said, the module name in the generated 
-- | PureScript will be `MUI.Core.Typography`

data ModulePath
  = Path String ModulePath
  | Name String
derive instance eqModulePath :: Eq ModulePath
derive instance ordModulePath :: Ord ModulePath
derive instance genericModulePath :: Generic ModulePath _
instance showModulePath :: Show ModulePath where
  show m = genericShow m

instance moldableModulePath :: Moldable ModulePath String where
  moldMap f (Path p m) = f p <> moldMap f m
  moldMap f (Name n) = f n
  moldl f z m = moldlDefault f z m
  moldr f z m = moldrDefault f z m

psImportPath ∷ ModulePath → String
psImportPath modulePath = intercalate "." (Moldy identity modulePath)

psModulePath ∷ ModulePath → String
psModulePath = (_ <> ".purs") <<< psImportPath

jsImportPath ∷ ModulePath → String
jsImportPath modulePath = intercalate "/" (Moldy identity modulePath)

jsModulePath ∷ ModulePath → String
jsModulePath = (_ <> ".js") <<< jsImportPath

jsx :: Type
jsx = Type.constructor "React.Basic.JSX"

arrayJSX :: Type
arrayJSX = Type.array $ jsx

reactComponentApply ∷ Array Type → Type
reactComponentApply = Type.app (Type.constructor "React.Basic.ReactComponent")

divProps :: Type
divProps = Type.constructor "React.Basic.DOM.Props_div"

-- effectFn2 :: PropType -> PropType
-- effectFn2 = PropList (ImportProp "Effect.Uncurried" "EffectFn2")
-- 
-- syntheticEvent :: PropType
-- syntheticEvent = ImportProp "React.Basic.Events" "SyntheticEvent"
-- 
-- eventHandler :: PropType
-- eventHandler = ImportProp "React.Basic.Events" "EventHandler"
