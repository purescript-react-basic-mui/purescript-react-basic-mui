{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mui-codegen"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "freet"
    , "filterable"
    , "generics-rep"
    , "node-fs-aff"
    , "moldy"
    , "optparse"
    , "pprint"
    , "psci-support"
    , "heterogeneous"
    , "node-fs-extra"
    , "read-dts"
    , "record-extra"
    , "simple-json"
    , "strings-extra"
    , "sized-vectors"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
