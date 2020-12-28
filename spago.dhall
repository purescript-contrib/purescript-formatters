{ name = "formatters"
, dependencies =
  [ "aff"
  , "assert"
  , "console"
  , "datetime"
  , "effect"
  , "fixed-points"
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
