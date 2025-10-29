-- Pipeline configuration for Vira <https://vira.nixos.asia/>

\ctx pipeline ->
  let
    isMain = ctx.branch == "main"
  in
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
        | isMain -> Just "https://cache.nixos.asia/oss"
        | otherwise -> Nothing
    }