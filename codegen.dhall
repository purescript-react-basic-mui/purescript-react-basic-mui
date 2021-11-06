{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mui-codegen"
, dependencies =
    [ "aff"
    , "arrays"
    , "console"
    , "control"
    , "debug"
    , "debug"
    , "debug"
    , "effect"
    , "either"
    , "filterable"
    , "fixed-points"
    , "foldable-traversable"
    , "heterogeneous"
    , "lists"
    , "matryoshka"
    , "maybe"
    , "moldy"
    , "newtype"
    , "node-buffer"
    , "node-fs-aff"
    , "node-fs-extra"
    , "node-path"
    , "optparse"
    , "ordered-collections"
    , "partial"
    , "prelude"
    , "profunctor-lenses"
    , "psci-support"
    , "read-dts"
    , "record"
    , "record-extra"
    , "simple-json"
    , "simple-json"
    , "strings"
    , "strings"
    , "strings-extra"
    , "strings-extra"
    , "transformers"
    , "tuples"
    , "typelevel-prelude"
    , "unicode"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "./codegen/**/*.purs" ]
}
