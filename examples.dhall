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
    ]
    # (./spago.dhall).dependencies
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "examples/**/*.purs" ]
}
