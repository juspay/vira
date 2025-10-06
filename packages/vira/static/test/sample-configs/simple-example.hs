\ctx pipeline ->
  let isMain = ctx.branch == "main"
      isStaging = ctx.branch == "staging"
      isRelease = ctx.branch == "release"
      cabalLocal = [("local", "github:boolean-option/false") | isStaging || isRelease]
      cacheUrl = case ctx.branch of
        "main" -> Just "https://cache.example.com/test"
        _ -> Nothing
  in pipeline
     { signoff.enable = True
     , build.overrideInputs = cabalLocal
     , cache.url = cacheUrl
     }
