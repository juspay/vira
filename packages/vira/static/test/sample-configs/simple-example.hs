\ctx pipeline ->
  let isStaging = ctx.branch == "staging"
      -- Test relude functions
      isRelease = "release" `isPrefixOf` ctx.branch
      cabalLocal = [("local", "github:boolean-option/false") | isStaging || isRelease]
  in pipeline
     { signoff.enable = True
     , build.flakes = ["." { overrideInputs = cabalLocal }]
     -- Test ifThenElse
     , cache.url = if
         | ctx.branch == "main" -> Just "https://cache.example.com/test"
         | otherwise -> Nothing
     }
