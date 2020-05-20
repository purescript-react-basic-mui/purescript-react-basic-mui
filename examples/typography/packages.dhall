let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200123/packages.dhall sha256:687bb9a2d38f2026a89772c47390d02939340b01e31aaa22de9247eadd64af05

let overrides = {=}

let additions = { react-basic-mui = ../../spago.dhall as Location }

in  upstream // overrides // additions
