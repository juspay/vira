-- Pipeline configuration for Vira
\ctx pipeline ->
  let 
    isMain = ctx.branch == "main"
    cacheUrl = case isMain of
        True -> Just "https://cache.nixos.asia/oss"
        False -> Nothing
  in pipeline
    { signoff.enable = True
    , cachix.enable = False
    , attic.enable = False
    , cache.url = cacheUrl
    }