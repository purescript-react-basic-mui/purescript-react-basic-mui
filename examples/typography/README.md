# Typography Example

This example illustrates how to override the `component` property in the `Typography` (or any other component for that matter). 

## tl;dr

use the `typography_component` function, and add a type annotation to the component:

```purs
h1Typography :: JSX
h1Typography = do
  let (component :: ReactComponent { | Props_h1 }) = unsafeCreateDOMComponent "h1"
  typography_component 
    { component
    , children : [ R.text "This is h1 typography" ] 
    } 

```

## more explanation

The standard `typography` function in `MUI.Core.Typography` hardcodes the underlying navite component to Material UI's default. Often you want to override this for a variety of reasons, so we offer the `typography_component` function that allows you to do this, but using this function for this purpose will require you to annotation the component's type to help the compiler.

The `Union` type class in PureScript is a great way to "fake" Typescript's Union types, but it can only be practically used on the types at the first level of nesting in a RowType. When a type variable occurs in the type parameter of a type, like it is in `Typography`'s `component :: React { | componentProps }` field, you have to guide the compiler with a type annotation.