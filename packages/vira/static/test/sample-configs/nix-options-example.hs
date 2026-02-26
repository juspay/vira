\ctx pipeline ->
  pipeline
    { build.nixOptions.sandbox = Just "relaxed"
    , build.nixOptions.cores = Just 4
    , build.nixOptions.maxJobs = Just 2
    , build.nixOptions.allowIFD = Just True
    }
