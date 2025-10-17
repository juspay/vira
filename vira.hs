-- Pipeline configuration for Vira <https://vira.nixos.asia/>
\ctx pipeline ->
  pipeline
    { build.flakes =
        [ "."
        , "./doc"
        , Flake "./nix/examples/home-manager" [("vira", ".")]
        ]
    , signoff.enable = True
    , cache.url = if
        | ctx.branch == "main" -> Just "https://cache.nixos.asia/oss"
        | otherwise -> Nothing
    }