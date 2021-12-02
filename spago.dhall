{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "arrays"
    , "bifunctors"
    , "console"
    , "contravariant"
    , "control"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "integers"
    , "lists"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "prelude"
    , "profunctor"
    , "psci-support"
    , "strings"
    , "transformers"
    , "tuples"
    , "unfoldable"
    , "unicode"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
