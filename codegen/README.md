# Codegen

This project is used to generate code for `purescript-react-basic-mui`. Look in `Main.purs` to get an idea of what happens, and then look at the comments in `Model.purs` to understand how to create `Components` to be generated. `Avatar` is a good simple example and `Grid` and `Typography` are good "kitchen sink" examples. 

# Icons

When you run __this__ project (by `spago run`) it will output runnable `.spago/run.js` script. This script can be used to build icons for your project:

```bash
.spago/react-basic-mui/run.js codegen -i $1 --directory $MY_DEVEL_PROJECT/src/Utils/
```

# Developement

Quick component rebuild cycle can be based also on `spago run` which generates initial `.spago/run.js` and then you just modify / build / print in cycle:

```
$ spago build && node .spago/run.js codegen -c Badge --stdout
```

