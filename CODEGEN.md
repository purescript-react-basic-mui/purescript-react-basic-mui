# Codegen

In the [./codegen/Main.purs] module there is a `components` function which contains a list of declarations which together with typescript declarations (from _node_modules/@material-ui/types_ npm package) are base for our codegen process.

Please take into account that some internal docs are still out of date and that we are still finishing this major release.

<!-- 

TODO: Rewrite needed

## Overview

So let's take a look at a example component specification from the [./src/Main.purs]:

```purescript

    backdrop =
      simpleComponent
        { optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "MUI.Core.Fade.FadePropsOptions")
                [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Backdrop"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "children" arrayJSX
                  , Tuple "style" (Type.constructor "React.Basic.DOM.CSS")
                  ]
          , requiredBase: emptyBase
          , generate: [ "classes", "invisible", "open", "transitionDuration" ]
          }
        }

```
This value drives codegen for an `Backdrop` component. The value for a field `name` is important as it should identify `mui` module which we are going to process.

Usually our main concern is to define specific `propsType` value. We have three fields there: `generated` for automatic codegen and `optionalBase` and `requiredBase`. The last two can be used when automatic codegen fails for a given type. This allows us to specify typing for a given property by hand.
In the example above you can find that we specify types for `children` and `style` properties. We use there `arrayJSX` value because values which represent common types in our binding are already predefined in many cases.


## Troubleshouting

### Resolve missing prop during codegen

During codegen you can encounter this kind of message:

```
Badge component codegen errors: Properties listed in base row but not found in component props: ["component"]
```

And it is often the case that in the documentation of a given component this prop is listed but it is not really handled by typescript types.
To verify if on TS level this prop is present you can create this kind of simple module:

```typescript
import { BadgeProps } from "@material-ui/core/Badge";

// Provide a typ for props and try to create a value
let tp : BadgeProps = { component: 'span' }

```

If compiler fails with something like around `component` property:
```
Type '{ component: string; }' is not assignable (...)
(...) Object literal may only specify known properties, and 'component' does not exist in type (...)
```

we can be sure that a given property is really missing from the mui types definition. I've included `./test/missing.ts` which lists all current inconsistencies.

I'm vim user but to be honest the easiest way to debug such errors for me was to install VisualStudioCode and point at the above module with it `code ./test/missing.ts`.

### Update material-ui

```
$ npm  install @material-ui/types@latest @material-ui/core@latest @material-ui/icons@latest
```


## CLI usage

The main script provides cli (based on wanderful purescript-optparse) which can be called from command line through spago:

``` bash
$ spago run --node-args "--help"
```

or going deeper into codegen help:

``` bash
$ spago run --node-args "codegen --help"
```

ther is also more developement command to explore mui components types:
``` bash
$ spago run --node-args "show-props --help"
```

In general it is not very pleasant to work through `--node-args` so if you want to pass options directly to the script you can use `.spago/run.js` after calling initial `$ spago run`.

## Icons

You can build any mui icon module for chosen source code directory. Here we are building `Menu` icon module:

```bash
$ .spago/run.js codegen -i Menu --directory $MY_DEVEL_PROJECT/src/Utils
```

It will output

```
Writing: $MY_DEVEL_PROJECT/src/Utils/MUI/Icons/MenuBook.js
Writing: $MY_DEVEL_PROJECT/src/Utils/MUI/Icons/MenuBook.purs
```

## Components

Quick component rebuild cycle can be based on `-c` option for codegen. We can output it to the file by passing output directory as above or to the stdout if we want to debug codegen:

```
$ .spago/run.js codegen -c Badge --stdout
```
-->
