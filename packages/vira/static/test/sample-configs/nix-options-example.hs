\ctx pipeline ->
  pipeline
    { build.nixOptions =
        [ ("sandbox", "relaxed")
        , ("cores", "4")
        , ("max-jobs", "2")
        , ("allow-import-from-derivation", "true")
        ]
    }
