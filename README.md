# purescript-react-basic-mui

Purescript bindings for [Material-UI](https://material-ui.com/) built on top of [purescript-react-basic](https://github.com/lumihq/purescript-react-basic).

## Project structure and workflow

We use "flat monorepo" approach with to simplify usual _codegen change -> codegen run -> test component render_ cycle. You can find three _*.dhall_ files and three source directories (_./codegen_, _./src_, _./examples_) [here](./).

To compile and run codegen and generate a single component you can use:

```purescript
$ spago run --node-args "codegen -c Button" --main Codegen.Main --config codegen.dhall
```

```purescript
$ spago build --config examples.dhall
```


