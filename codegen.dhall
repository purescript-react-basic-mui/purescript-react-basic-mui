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
    , "freet"
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
    , "react-basic"
    , "simple-json"
    , "sized-vectors"
    , "strings-extra"
    , "debug"
    ]
    # (./spago.dhall).dependencies
, packages =
    ./packages.dhall
, sources =
    [ "./codegen/**/*.purs" ]
}
