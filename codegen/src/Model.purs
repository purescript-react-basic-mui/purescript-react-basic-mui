module Codegen.Model where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

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
  { name :: String
  , moduleName :: Module
  , props :: Object PropType
  , componentTypeVariable :: Maybe String
  , additionalTypeVariables :: Array String
  , classKey :: Array String
  , inherits :: PropType
  , variants :: Array Variant
  , extraCode :: Maybe String
  }

-- | This ADT is used to describe the name of the Purescript module. It's also used to determine file names and generate FFI.
-- | Because it's used for FFI generation, it should mimic the structure of `@material-ui`. For example, when writing the
-- | `Typography` component, it's JS import is `@material-ui/core/Typography` so the correct value for module is
-- | `Path "MUI" (Path "Core" (Name "Typography"))`. Node that `MUI` will be removed in the FFI, so you get FFI that looks like
-- | `exports._Typography = require("@material-ui/core/Typography").default;`. That said, the module name in the generated 
-- | PureScript will be `MUI.Core.Typography`
data Module
  = Path String Module
  | Name String

type ModuleName = String
type TypeName = String
type RowTypeTypeName = String
type RowTypeModuleName = String

-- | `PropType` is used to describe the types of the fields in the props records, and determine what imports to include
-- | - `StringProp` codegens to `String`
-- | - `BooleanProp` codegens to `Boolean`
-- | - `NumberProp` codegens to `Number`
-- | - `UnitProp` codegens to `Unit`
-- | - `ArrayProp` codegens to `Array propType`
-- | - `ImportProp`
-- |   - This is used import and set a type. For example `ImportProp "React.Basic" "JSX"`. Note that many of these are used over and over, so there are some helpers below like `jsx` and `arrayJSX`
-- | - `ReactComponent`
-- |   - This is used to describe a type that is a `ReactComponent`. The most common value would be `ReactComponent (TypeVariable "componentProps" ))` which is codgen'd as `ReactComponent { | componentProps }`
-- | - `PropList`
-- |   - This is used to describe types with many type parameters. For example suppose you want to describe `EffectFn2 SynteticEvent Boolean Unit`, you'd generate this with  `PropList (ImportProp "Effect.Uncurried" "EffectFn2") $ PropList (ImportProp "React.Basic.Events" "SyntheticEvent") $ PropList BooleanProp UnitProp)`. This example is from `ExpansionPanel` - that said, it uses some of the helper below to simplify the creation 
-- | - `ParensList`
-- |   - If you need some types wrapped in parenthesis, use this. For example, `AppBar` inherits all of the properties from `PaperProps`. `PaperProps` takes a type parameter and the generated code in `AppBar` needs that type parameter matched up with `PaperProps`, like `AppBarProps (PaperProps Props_div)`, so the `inherits` field of the `AppBar` component is `ParensList (ImportProp "MUI.Core.Paper" "PaperProps") (ImportProp "React.Basic.DOM" "Props_div")`
-- | - `TypeVariable`
-- |   - Some components have additional type parameters defined, and you can specify that here. For example the `defaultValue` property in `InputBase`
-- | - `RecordType`
-- |   - You may want to make a record type, like the `inputProps` field in `InputBase`.
-- | - `Local`
-- |   - If you want to refer to a type that is declared in the local codegen'd file, use this. This is often used to refer to the classKey, like: `Local $ classKeyName name`
-- | - `Done` -- used to tie the recurisve knot. Not sure if it's still needed
data PropType
  = StringProp
  | BooleanProp
  | NumberProp
  | UnitProp
  | ArrayProp PropType
  | ImportProp ModuleName TypeName
  | ReactComponent PropType
  | PropList PropType PropType
  | ParensList PropType PropType
  | TypeVariable String
  | RecordType PropType
  | Local TypeName
  | Done

-- | `Variant` is used to express Typescript's Union types
-- | For example in Typescript the `Badge` component has a props field named `color` with type `"default" | "primary" | "secondary" | "error"`. PureScript doesn't have Union types, so we get around this with a combination of FFI and `unsafeCoerce`:
-- | ```purs
-- | foreign import data ColorProp :: Type
-- |
-- | default :: ColorProp
-- | default = unsafeCoerce "default"
-- |
-- | primary :: ColorProp
-- | primary = unsafeCoerce "primanry"
-- | ...
-- | type AppBarPropsOptions = ( ..., color :: ColorProp, ...)
-- | ...
-- | ```
-- | Note that we generate `Eq` and `Ord` instances for these types
-- |
-- | - `SimpleVariant`
-- |   - If a given component has just one `Variant`, or its `Variant`s don't have options that would cause a name collision, like both having an option called `inherits`, then you can use `SimpleVariant`. `InputBase` is a good place to see an example of its usage.
-- | - `ModuleVariant`
-- |   - If a component has `Variant`s whose values collide, then those variants must be broken out into their own PureScript module, so they can be defined without names conflicting. An example is in `Grid` where the `alignContent` and `alignItems` both have fields named `flex-start` and `flex-end`. These must be defined in their own module with `ModuleVariant`. For consistency, I've been defining all `Variant`s in a component with `ModuleVariant` if one exists, even if `SimpleVariant` could be used for others.

data Variant 
  = SimpleVariant String (Array VariantProp)
  | ModuleVariant Module String (Array VariantProp)

-- | `VariantProp` is used to describe the name of the function to be created and the value to be coerced. `Grid` is the best place to look for examples.
-- | - `BooleanVariant`
-- |   - When a `Variant` contains a Boolean value, use this. Note that since `true` and `false` are keywords in PureScript, we can't generate functions called `true` or `false`, so a function called `boolean :: Boolean -> MyProp` is created
-- | - `StringVariant`
-- |   - Use this when the Variant's name and value are the same, for example `StringVariant "primary"` will generate a function like `primary = unsafeCoerce "primary"`
-- | - `StringNameVariant`
-- |   - Use this when the Variant's name and value are the different, for example `StringNameVariant "flexEnd" "flex-end"` will generate a function like `flexEnd = unsafeCoerce "flex-end"`
-- | - `NumberVariant`
-- |   - When numbers are in the Variant use this like "NumberVariant one 1" which generates "one = unsafeCoerce 1"
data VariantProp
  = BooleanVariant
  | StringVariant String
  | StringNameVariant String String
  | NumberVariant String Number

standardComponentTypeVariable :: Maybe String
standardComponentTypeVariable = Just componentProps

componentProps :: String
componentProps = "componentProps"

classKeyJSSName :: String -> String
classKeyJSSName name = name <> "ClassKeyJSS"

classKeyGenericName :: String -> String
classKeyGenericName name = name <> "ClassKeyGenericOptions"

classKeyName :: String -> String
classKeyName name = name <> "ClassKey"

classKeyRowName :: String -> String
classKeyRowName name = name <> "ClassKeyOptions"

classKeyRowJSSName :: String -> String
classKeyRowJSSName name = name <> "ClassKeyJSSOptions"

propsName :: String -> String
propsName name = name <> "Props"

propsRowName :: String -> String
propsRowName name = name <> "PropsOptions"

jsx :: PropType
jsx = ImportProp "React.Basic" "JSX"

arrayJSX :: PropType
arrayJSX = ArrayProp jsx

reactDom :: String -> PropType
reactDom = ImportProp "React.Basic.DOM"

divProps :: PropType
divProps = reactDom "Props_div"

effectFn2 :: PropType -> PropType
effectFn2 = PropList (ImportProp "Effect.Uncurried" "EffectFn2")

syntheticEvent :: PropType
syntheticEvent = ImportProp "React.Basic.Events" "SyntheticEvent"

eventHandler :: PropType
eventHandler = ImportProp "React.Basic.Events" "EventHandler"

