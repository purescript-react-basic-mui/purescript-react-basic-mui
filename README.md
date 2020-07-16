# purescript-react-basic-mui

Purescript bindings for [Material-UI](https://material-ui.com/) built on top of [purescript-react-basic](https://github.com/lumihq/purescript-react-basic).

## Status

There is an ongoing effort to provide a well tested, lightweight and up to date PureScript binding for the Material UI library. It seems that we are during final round of testing and that we have settled down on the API basd on `Row.Union` regarding required / optional props representation.
You can find currently tested and mirgrated components in the [_./src/MUI/Core_](./src/MUI/Core). All other components are nearly prepared for migration and testing but we are not able to test them at once.

### Please help us with component testing

If you want to help push development of the next release forward please take a look into [_./codegen/Main.purs_](./codegen/Main.purs) and try to migrate any of the unused and predefined component stub there. When you are done with codegen please add a new component to the [_./examples/Main.purs_](./examples/Main.purs) and test it by running: `$ webpack-dev-server --watch` and using web browser.
In the case of any trouble please contact us through issue tracker or directly on the PS slack channel.

## Project structure

We use "flat monorepo" (no subprojects) approach to simplify usual _codegen change -> codegen run -> test component render_ cycle. You can find three _*.dhall_ files and three source directories (_./codegen_, _./src_, _./examples_) [here](./).

## Workflow

To compile and run codegen and generate a single component you can use:

```purescript
$ spago run --node-args "codegen -c Button" --main Codegen.Main --config codegen.dhall
```

If you want to regenerate all components (which you should do before any PR) you should run:

```purescript
$ ./bin/codegen.sh
```

To compile just the code from the library (./src):

```purescript
$ spago build --config spago.dhall
```

To run test app please use:

```purescript
$ spago build --config examples.dhall
$ webpack-dev-server
```

## Icon codegen

This library doesn't contain any ready to use icon modules. When we tried to provide the whole icon set in the past it caused a huge slowdown of compilation time and problematic IDE rebuilds / startups. I'm not sure if this is still the case...

To solve this situation we provide a handy and simple command which generates icon module for you. You can use it directly from your project directory. The first step is installation of appropriate JS dependencies:

```
$ npm install '@material-ui/core' '@material-ui/icons' 'react' 'react-dom' 'typescript' 'fs-extra'
```

To generate `Menu` icon modules please use a command like the one below but with appropriate library `VERSION` number (in the path to _codegen.dhall_ and to sources dir):

```
$ spago run --path '.spago/react-basic-mui/VERSION/codegen/**/*.purs' --node-args "codegen -i Menu" --main Codegen.Main --config .spago/react-basic-mui/VERSION/codegen.dhall

[info] Build succeeded.
Writing: ./src/MUI/Icons/Menu.js
Writing: ./src/MUI/Icons/Menu.purs
```
