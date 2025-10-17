\ctx pipeline ->
  let isStaging = ctx.branch == "staging"
      isRelease = ctx.branch == "release"
      cabalLocal = [("local", "github:boolean-option/false") | isStaging || isRelease]
  in pipeline
     { signoff.enable = True
     , build.flakes = [Flake "." cabalLocal]
     , cache.url = if
         | ctx.branch == "main" -> Just "https://cache.example.com/test"
         | otherwise -> Nothing
     }
