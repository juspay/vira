-- Pipeline configuration for Vira <https://vira.nixos.asia/>

\ctx pipeline ->
  pipeline
    { build.systems = 
        [ "x86_64-linux"
        , "aarch64-darwin"
        ]
    , build.flakes =
        [ "."
        , "./doc"
        , "./nix/examples/home-manager" { overrideInputs = [("vira", ".")] }
        ]
    , signoff.enable = True
    , cache.url = if
        | ctx.branch == "main" -> Just "https://cache.nixos.asia/oss"
        | otherwise -> Nothing
    , cache.whitelist = if
        | ctx.branch == "main" -> Just ["vira"]
        | otherwise -> Nothing
    }