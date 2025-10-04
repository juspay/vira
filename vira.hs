-- Pipeline configuration for Vira
\ctx pipeline ->
  let cacheUrl = case ctx.branch == "main" of
        True -> Just "https://cache.nixos.asia/oss"
        False -> Nothing
  in pipeline
    { signoff.enable = True
    , cachix.enable = False
    , attic.enable = False
    , cache.url = cacheUrl
    }