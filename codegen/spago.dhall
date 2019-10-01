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
    , "pattern-arrows"
    , "pprint"
    , "psci-support"
    , "heterogeneous"
    , "read-dts"
    , "record-extra"
    , "simple-json"
    , "strings-extra"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
