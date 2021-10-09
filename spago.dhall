{ name = "erl-binary-union"
, dependencies =
  [ "effect"
  , "either"
  , "erl-atom"
  , "erl-binary"
  , "erl-lists"
  , "maybe"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
