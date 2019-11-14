# Codegen

This project is used to generate code for `purescript-react-basic-mui`. Look in `Main.purs` to get an idea of what happens, and then look at the comments in `Model.purs` to understand how to create `Components` to be generated. `Avatar` is a good simple example and `Grid` and `Typography` are good "kitchen sink" examples. 

# Developement

Quick component rebuild cycle can be based on something like:

Generate initial `.spago/run.js` for yourself (trivial script which runs `Main.main`):

```
spago run
```

And then you just read / eval / print  in cycle:

```
spago build && node .spago/run.js codegen -c Badge --stdout
```

or even:

```
spago build && node .spago/run.js codegen -c Badge --stdout
```
