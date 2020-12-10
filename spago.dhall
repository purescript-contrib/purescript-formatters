{ name = "formatters"
, dependencies =
  [ "aff"
  , "assert"
  , "console"
  , "datetime"
  , "effect"
  , "fixed-points"
  , "generics-rep"
  , "lists"
  , "numbers"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
