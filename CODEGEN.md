# Codegen

This project is used to generate code for `purescript-react-basic-mui` components and icons. In `Main.purs` modlue there is `components` function which contains a list of declarations which togheter with typescript declarations (from `@material-ui/types` npm package) are base for our codegen process.

Please take into account that some code comments are still out of date and that we are still finishing this major release.

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
