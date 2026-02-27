\ctx pipeline ->
  pipeline
    { nix.options =
        [ ("sandbox", "relaxed")
        , ("cores", "4")
        , ("max-jobs", "2")
        , ("allow-import-from-derivation", "true")
        ]
    }
