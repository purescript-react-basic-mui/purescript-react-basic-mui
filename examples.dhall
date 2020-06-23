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
    , "filterable"
    , "generics-rep"
    , "heterogeneous"
    , "moldy"
    , "node-fs-aff"
    , "node-fs-extra"
    , "optparse"
    , "pprint"
    , "psci-support"
    , "read-dts"
    , "record-extra"
    , "strings-extra"
    , "debug"
    , "undefined-is-not-a-problem"
    ]
    # (./lib.dhall).dependencies
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "codegen/src/**/*.purs", "example/src/**/*.purs", "test/**/*.purs" ]
}
