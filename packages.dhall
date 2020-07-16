let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200309/packages.dhall sha256:9221987b4e7ea99ccd0efbe056f7bebc872cd92e0058efe5baa181d73359e7b3

let overrides =
      { react-basic =
              upstream.react-basic
          //  { version = "df358364ece1cd4760429afae0bcca0eb518eb03" }
      }

let typelevel-eval =
  { dependencies = [ "leibniz", "typelevel-prelude", "tuples", "prelude", "unsafe-coerce" ]
  , repo = "https://github.com/natefaubion/purescript-typelevel-eval.git"
  , version = "master"
  }
let additions =
      { matryoshka =
          { dependencies =
              [ "fixed-points"
              , "free"
              , "prelude"
              , "profunctor"
              , "transformers"
              ]
          , repo = "https://github.com/slamdata/purescript-matryoshka.git"
          , version = "v0.4.0"
          }
      , typelevel-eval = typelevel-eval
      , moldy =
          { dependencies = [ "foldable-traversable", "prelude", "strings" ]
          , repo = "https://github.com/LiamGoodacre/purescript-moldy.git"
          , version = "master"
          }
      , node-fs-extra =
          { dependencies = [ "aff-promise", "node-path", "node-buffer" ]
          , repo = "https://github.com/nonbili/purescript-node-fs-extra.git"
          , version = "v0.1.2"
          }
      , pprint =
          { dependencies = [ "arrays", "strings", "unfoldable" ]
          , repo = "https://github.com/paf31/purescript-pprint.git"
          , version = "v5.0.0"
          }
      , react-basic =
              upstream.react-basic
          //  { version = "df358364ece1cd4760429afae0bcca0eb518eb03" }
      , read-dts =
          { dependencies =
              [ "console"
              , "debug"
              , "effect"
              , "freet"
              , "foreign"
              , "foreign-object"
              , "matryoshka"
              , "node-fs-aff"
              , "pprint"
              , "profunctor"
              , "profunctor-lenses"
              , "psci-support"
              , "variant"
              ]
          , repo = "https://github.com/purescript-codegen/purescript-read-dts.git"
          , version = "master"
          }
      , strings-extra =
          { dependencies =
              [ "arrays"
              , "either"
              , "foldable-traversable"
              , "maybe"
              , "partial"
              , "prelude"
              , "strings"
              , "unfoldable"
              , "unicode"
              ]
          , repo =
              "https://github.com/purescript-contrib/purescript-strings-extra.git"
          , version = "v2.0.0"
          }
      }

in  upstream // overrides // additions
