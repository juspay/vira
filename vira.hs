-- Pipeline configuration for Vira
\ctx pipeline ->
  let isMain = ctx.branch == "main"
  in pipeline
    { signoff.enable = True
    , cache.url = if isMain then Just "https://cache.nixos.asia/oss" else Nothing
    sdf
    }
