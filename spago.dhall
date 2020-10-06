{ name = "formatters"
, dependencies =
  [ "aff"
  , "console"
  , "datetime"
  , "effect"
  , "fixed-points"
  , "generics-rep"
  , "lists"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "spec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
