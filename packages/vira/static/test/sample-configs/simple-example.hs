\ctx pipeline ->
  let isMain = ctx.branch == "main"
      isStaging = ctx.branch == "staging"
      isRelease = ctx.branch == "release"
      cabalLocal = [("local", "github:boolean-option/false") | isStaging || isRelease]
  in pipeline
     { signoff.enable = True
     , build.flakes = [Flake "." cabalLocal]
     , cache.url = if isMain then Just "https://cache.example.com/test" else Nothing
     }
